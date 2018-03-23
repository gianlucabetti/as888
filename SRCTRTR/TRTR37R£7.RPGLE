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
004000151120     Ffiar531c  uF a E           K DISK
004100000000     FTABEL00F  IF   E           K DISK
004200900426     FAZORG01L  IF   E           K DISK
004300941121     FFNLBL01L  IF   E           K DISK
004400941121     FTNCSB03L  UF A E           K DISK
004500941121     FTNCSA01L  UF A E           K DISK
004600941121     FTNCSV00F  O  A E             DISK
004700981118     Fndppa01L  IF   E           K DISK
004800990518     FNdcpa01L  uF   E           K DISK
004900941130     FTNBLA01L  IF   E           K DISK
005000121107
005100020827     D CMD1            S             51    DIM(1) CTDATA PERRCD(1)
005200020827     D CM2             S              1    DIM(51) CTDATA PERRCD(51)
005300020827     D CM3             S              1    DIM(51) CTDATA PERRCD(51)
005400020827     D CM5             S              1    DIM(213) CTDATA PERRCD(66)
005500020827     D C6              S              1    DIM(213)
005600920113     D*
005700991019     D ERR             S             35    DIM(42) CTDATA PERRCD(1)             COSTANTI ERRORA
005800950202     D K35             S              1    DIM(35)                              COMODO        A
005900070119     d cmdt            s             60    dim(01) ctdata perrcd(1)
006000011218
006100011218     d sksv            s              1    dim(20)                              sk sigla varie
006200011218     d skva            s             11  3 dim(20)                              sk imp.varie
006300011218
006400151008     d infdsarbr       ds
006500151008     d   recname             261    270
006600151008     D   nrri                397    400i 0
006700970916     D WLBDAT          DS                  INZ
006800970916     D  G02DAT                 1      8  0
006900970916     D  G02INV                 9     16  0
007000970916     D  G02ERR                17     17
007100970916     D  G02TGI                18     22  0
007200970916     D WGIDAT          DS                  INZ
007300970916     D  GIODAT                 1      8  0
007400970916     D  GIOINV                 9     16  0
007500970916     D  GIOTGI                17     21  0
007600941130     D CSBDS         E DS                  EXTNAME(TNCSB00F)
007700941130     D               E DS                  EXTNAME(TNCSV00F)
007800941130     D  CSVDS                 16    229
007900920113     D KPJBA         E DS
008000920113     D  LIBSYS                92    101
008100981118     D  wsoc                  98    100
008200920113     D  LEGFIL               483    492
008300920113     D  MBRFIL               493    502
008400920113     D  ALFFIL               494    496
008500151008     d fnlvv1ds      e ds
008600151008      * ATTENZIONE
008700151008     d* Se dovesse cambiare la lunghezza record di una delle seguenti DS
008800151008     d* riverificare la definizione del campo parvar
008900151008     d dsarbg          ds                  qualified
009000151008     d   fnarbg                            likerec(fnarbg) overlay(dsarbg)
009100151008     d dsarbk          ds                  qualified
009200151008     d   fnarbk                            likerec(fnarbk) overlay(dsarbk)
009300151008     d dsarbd          ds                  qualified
009400151008     d   fnarbd                            likerec(fnarbd) overlay(dsarbd)
009500151008     d dsarbt          ds                  qualified
009600151008     d   fiarbt                            likerec(fiarbt) overlay(dsarbt)
009700151008      * Definizione del seguente campo in base alla dsarbd che è la più lunga
009800151008      * di tutte.
009900151008     dparvar           s            224
010000151008     dpartitas         s                   like(titasds)
010100151008     dparcsb           s                   like(dscsb)
010200970714     D* DS PER TISI95R - CONTROLLO CAP
010300970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
010400021014     D ddatiute      e ds
010500021014     D azuteds       e ds                  extname(AZUTE00F)
010600020604     D tibs34ds      E DS
010700021112     D tibs36ds      E DS
010800070201     D tibs55ds      E DS
010900930514     D DSTB          E DS
011000930514     D DS3A          E DS
011100920113     D DS7L          E DS
011200150108     D DS7R          E DS
011300950202     D DS1A          E DS
011400010126     D og143         E DS
011500010126     D dsbl4j        E DS
011600060317     D dta4a         E DS
011700971215     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011800981001     D DSBS69        E DS                  EXTNAME(TIBS69DS)
011900981001     D DSACO         E DS                  EXTNAME(CNACO00F)
012000981001     D DSIND         E DS                  EXTNAME(CNIND00F)
012100981001     D DSCLP         E DS                  EXTNAME(CNCLP00F)
012200981001     D DSCLS         E DS                  EXTNAME(FNCLS00F)
012300121115     D dstitas       E DS                  EXTNAME(titas00f) qualified
012400121116     D titasds       E DS                  EXTNAME(titas00f)
012500121115     D dscsb         E DS                  EXTNAME(tncsb00f) qualified
012600080408     D SAVDS3a       E DS                  extname(ds3a) prefix(S_)
012700120510     d*dvpoDECO      e ds
012800121114     D Tibs02DS      E DS
012900151119     D dar5fat       E DS
013000071102     d
013100990505     DWDELARBU         s              1
013200990505     dwctb             s                   like(ppactb) inz('CG')
013300981118     dwkcc             s                   like(ppakcc) inz('000151')
013400981118     dwksc             s                   like(ppaksc) inz('00000000')
013500981118     dwdft             s                   like(ppadtpar)
013600981118     dwnft             s                   like(ppanrpar)
013700981118     dwfiv             s                   like(ppaserpar) inz
013800981118     dsavfiv           s                   like(tasfiv)
013900981118     dsavnft           s                   like(tasnft)
014000981118     dsavdft           s                   like(tasdft)
014100981118     dsavksc           s                   like(tasksc)
014200151012     dtasdsp           s                   like(tamddt)
014300990505     dWCAS             s                   like(arbcas)
014400990625     d§trc             s                   like(ta4trc)
014500991102     dwcsbvca          s                   like(csbvca)
014600151119     dkar5trd          s                   like(ar5trd)
014700011218
014800011218     d flgriorg        s              1
014900011218     d xx              s              2  0                                      indice sk varie
015000070119     d commt           s             80
015100070119     d lenght          s             15  5 inz(80)
015200120424     D k               s              5  0                                      indice
015300121205     D wanncas         s              1
015400151008     D                SDS
015500151008     D  NOMPGM                 1     10
015600941130     I*
015700950201     IFNARBD00      11
015800950201     IFNARBK00      12
015900991014     IFIARBT00      13
016000950201     IFNARBG00      15
016100950201     IFNARBM00      16
016200941121     I*
016300990624     ITITAS000      02
016400990624     ITITAS010      03
016500990624     ITITASP00      04
016600010126     I*
016700010126     ITITA4000      05
016800010126     ITITA4P00      06
016900071029     I*
017000071029     ITITAA000      07
017100071029     ITITAAP00      08
017200920113     I/SPACE 3
017300920113     C****************************************************************
017400920113     C*  RIEPILOGO INDICATORI
017500920113     C***************************************************************
017600950201     C* 01    - LETTURA SUL FILE DEGLI ERRORI DI FNARBE
017700010126     C* 02/06 - INDICATORI DI TIPO RECORD
017800941130     C* 17    - STAMPO ERRORE VARIAZIONE CONS PARTICOLARE
017900920228     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
018000930514     C* 23    - OFF SPEDIZIONE STORNATA
018100930513     C* 24    - NON AGGIORNO TUTTI I CAMPI SE 2BOLL GIA' CONTABILIZZATA
018200930513     C* 25    - NON ESISTE ANAGRAFICA C/A
018300981118     C* 26    - NON ESISTE RECORD IN partita
018400930513     C* 27    - NON AGGIORNO TUTTI I CAMPI SE 1BOLL GIA' CONTABILIZZATA
018500930512     C* 29    - ON  33 ON  - NON ESISTE NEMMENO        SU BLTAS10
018600930702     C*       - ON  33 OFF - SPEDIZIONE CONTABILIZZATA SU BLTAS
018700950201     C* 30    - LETTURA FNARBR3C
018800920212     C* 31    - FILE CONTRASSEGNI NON TROVATO O GIA' INCASATO
018900920212     C* 32    - COMODO
019000920228     C* 33    - ON SPEDIZIONE ESISTENTE SU BLTAS10
019100920212     C* 34    - COMODO
019200950201     C* 35    - MANCA RECORD SU FNARBM0R
019300920214     C* 36    - SCRIVO FLRBM0E QUANDO SERVE
019400920213     C* 37    - RECORD DA NON CANCELLARE DOPO WRITE ERRORE
019500920226     C* 38    - SPEDIZIONE GIA' TASSATA
019600920227     C* 39    - DI COMODO
019700920303     C* 40    - SPEDIZIONE ANNULLATA
019800941130     C* 41    - ESISTENTE TNCSB000
019900930303     C* 42    - ESISTENTE LEGAME BOLLA (C/ASSEGNO SULLA MAMMA)
020000981001     C* 48    - NON ESISTE RECORD SU TNTMD
020100920113     C*****************************************************************
020200920113     C*
020300920113     C* ALLOCO MEMBRI
020400920113     C                   EXSR      CAEX
020500920113     C   20              GOTO      FINE
020600920113     C*
020700991014     C                   READ      FIARBR3C                               30
020800920113     C*
020900930512    1C     *IN30         DOWEQ     '0'
021000990414     C                   SETOFF                                       242736
021100930622     C*
021200981001     C                   MOVEL     ' '           DMDD              1
021300930514     C                   MOVEL     ' '           ST1
021400981001     C* SOLO NEL CASO IN CUI SI TRATTI DI LETTURA DI VARIAZIONI MITTENTE E
021500981001     C* DESTINATARIO ( *IN11 *IN16 ON ) CONTROLLO SE SI TRATTA DI VARIAZIONI
021600981001     C* DM / DD PERCHE' IN QUESTO CASO DEVO SCRIVERE IL FILE DATI ANAGRAFICI
021700981001     C* SPEDIZIONI CHE VIENE UTILIZZATO PER STAMPE RELATIVE AI DANNI
021800981001     C* INOLTRE SEMPRE PER 11 E 16 ACCESO PER LE VARIAZIONI I0 / MI (VARIAZIONI
021900981001     C* DESTINATARIO E MITTENTE) DEVO AGGIORNARE QUESTO FILE INDIPENDENTEMENTE
022000981001     C* DAL FATTO CHE SIA RELATIVO AD UNA BOLLA GIA' CONTABILIZZATA
022100981001     C     *IN11         IFEQ      '1'
022200981001     C     *IN16         OREQ      '1'
022300981001     C                   EXSR      AGTMD
022400981001     C* SE HO LETTO UNA VARIAZIONE DM O DD LEGGO IL RECORD SUCCESSIVO
022500981001     C* ALTRIMENTI PROSEGUO
022600981001     C     DMDD          IFNE      ' '
022700981001     C                   GOTO      NEXT
022800981001     C                   ENDIF
022900981001     C*
023000981001     C                   ENDIF
023100930513     C*
023200930513     C* PRENDO  C O D I C E   B O L L A
023300920113     C                   MOVEL     '3A'          COD
023400920113     C                   MOVEL     *BLANKS       KEY
023500991014     C  N12              MOVEL     ARBCBO        KEY
023600950203     C   12              MOVEL     ARBCBP        KEY
023700920113     C     KTAB          CHAIN     TABEL                              33
023800920114     C*
023900920212     C* SE NON ESISTE TIPO BOLLA: ERRORE E NON CANCELLO
024000930513    2C     *IN33         IFEQ      '1'
024100991014     C                   MOVEL     ERR(2)        ARBDER
024200930513     C*
024300000703     C                   Z-ADD     102           COMFEV
024400930513     C                   SETON                                        37
024500920212     C*
024600930513     C                   EXSR      WTRERR
024700930513     C*
024800930513   X2C                   ELSE
024900930513     C*
025000930513     C                   MOVEL     TBLUNI        DS3A
025100950203     C* IMPOSTO IL TIPO BOLLA PER EVENTUALE ERRORE
025200950203     C                   MOVEL     §3ATB1        TBLERR            1
025300990519     C                   MOVEL     §3ARBL        TBLRBL            1
025400930513     C*
025500930513     C* PRENDO  C A U S A L E   V A R I A Z I O N E
025600991014     C                   MOVEL     ARBCVB        COMCVB            2
025700930513     C*
025800930513     C                   MOVEL     '7L'          COD
025900930513     C                   MOVEL     *BLANKS       KEY
026000930513     C                   MOVEL     COMCVB        KEY
026100930513     C     KTAB          CHAIN     TABEL                              33
026200930513     C*
026300930513     C* 33 ON - NON ESISTE CAUSALE VARIAZIONE: STAMPA E NON ANNULLO
026400930513    3C     *IN33         IFEQ      '1'
026500991014     C                   MOVEL     ERR(3)        ARBDER
026600930513     C*
026700930513     C                   Z-ADD     102           COMFEV
026800930513     C                   SETON                                        37
026900930513     C*
027000930513     C                   EXSR      WTRERR
027100930513    3C                   END
027200930513     C*
027300930513     C  N33              MOVEL     TBLUNI        DS7L
027400930513    2C                   END
027500920130     C*
027600930513    2C     *IN33         IFEQ      '0'
027700920226     C                   MOVEL     COMCVB        FLGCVB            1
027800920226     C*
027900990701     C                   CLEAR                   KSC42BO
028000990701     c*
028100920226     C* CONTROLLO ESISTENZA 2   BOLLA
028200950202    3C     FLGCVB        IFNE      'C'
028300950202     C     §3ATB2        ANDNE     '  '
028400970514     C* NON CONTROLLO LA 2 BOLLA SE TASSAZIONE IN PARTENZA PERCHE' NON
028500970514     C*  LA VA MAI AD AGGIORNARE
028600970514     C     COMCVB        ANDNE     'TP'
028700920226     C                   MOVEL     §3ATB2        COMTBL
028800920226     C                   EXSR      CTRSPE
028900941130     C*
029000941130     C* SE NON TROVATI ERRORI --> SALVO CAMPI CHE MI SERVONO
029100941130    4C     *IN29         IFEQ      *OFF
029200941130     C                   MOVE      TASKSC        SAVKS4            4 0
029300981118     C                   MOVE      TASKSC        SAVKsc
029400941130     C                   MOVEL     TASFIV        SAVFIV
029500941130     C                   MOVE      TASDFT        SAVDFT
029600941130     C                   MOVEL     TASNFT        SAVNFT
029700960625     C                   MOVEL     TASTBL        SAVTBL
029800941130     C*
029900941130     C* 24 ON - 2 BOLLA GIA' CONTABILIZZATA
030000941130    5C     *IN02         IFEQ      *ON
030100941130     C     TASFIC        ANDNE     ' '
030200941130     C     *IN02         OREQ      *OFF
030300941130     C                   SETON                                        24
030400941130    5C                   ENDIF
030500941130    4C                   ENDIF
030600930513    3C                   END
030700920226     C*
030800920226     C* SE TROVATA 2 BOLLA CONTROLLO LA 1
030900930513    3C  N29FLGCVB        IFNE      '2'
031000920226     C                   MOVEL     §3ATB1        COMTBL
031100920226     C                   EXSR      CTRSPE
031200941130     C*
031300941130    4C     *IN29         IFEQ      *OFF
031400981118     C* 27 ON - 1 BOLLA GIA' CONTABILIZZATA --> AGGIORNO ANCHE yncpa
031500941130    5C     *IN02         IFEQ      *ON
031600941130     C     TASFIC        ANDNE     ' '
031700941130     C     *IN02         OREQ      *OFF
031800941130     C                   SETON                                        27
031900941130    5C                   END
032000941130    4C                   END
032100930513    3C                   END
032200990414     C*
032300941130     C*
032400941130     C* 29 OFF - AGGIORNO BOLLA SE NON SONO STORNATE ENTRAMBI LE BOLLE
032500930514     C  N29ST1           IFEQ      ' '
032600991021     c     flgcvb        oreq      'C'
032700930514     C                   EXSR      AGGBOL
032800930514     C                   END
032900941130     C*
033000930514    2C                   END
033100920113     C*
033200950203    2C  N37              DO
033300920212     C   11              ADD       1             NUMRBD            7 0
033400920113     C   12              ADD       1             NUMRBK            7 0
033500920113     C   13              ADD       1             NUMRBT            7 0
033600950201     C   15              ADD       1             NUMRBG            7 0
033700950201     C   16              ADD       1             NUMRBM            7 0
033800920219     C* CANCELLO RECORD
033900950201     C   11              DELETE    FNARBD00
034000950201     C   12              DELETE    FNARBK00
034100950202     C   12KARBM         DELETE    FNARBMRR                           32
034200991014     C   13              DELETE    FIARBT00
034300100303     c   13              if        st1='1'
034400100303     C     KARBU         SETLL     FIARBU1R
034500100303     C     KARBU         READE     FIARBU1R                               30
034600100303     C*
034700100303     C     *IN30         DOWEQ     *OFF
034800100303     C                   DELETE    FIARBURR
034900100303     C*
035000100303     C     KARBU         READE     FIARBU1R                               30
035100100303     C                   ENDDO
035200100303     c
035300100303     c                   endif
035400100303     c
035500950201     C   15              DELETE    FNARBG00
035600950201     C   16              DELETE    FNARBM00
035700930513    2C                   END
035800920113     C*
035900981001     C     NEXT          TAG
036000981001     C*
036100920113     C                   SETOFF                                       111213
036200991014     C                   SETOFF                                         1516
036300950201     C                   SETOFF                                       353738
036400941130     C                   SETOFF                                       2933
036500991014     C                   READ      FIARBR3C                               30
036600930512    1C                   END
036700970714     C*
036800970714     C                   CLEAR                   DSSI95
036900970714     C                   MOVEL     'C'           I95TLA
037000970714     C                   CALL      'TISI95R'
037100970714     C                   PARM                    DSSI95
037200971215     C*
037300971215     C                   MOVEL     'C'           D55TLA
037400971215     C                   CALL      'FNLV55R'
037500971215     C                   PARM                    DSLV55
037600981001     C*
037700981001     C                   CLEAR                   DSBS69
037800981001     C                   MOVEL     'C'           I69TLA
037900981001     C                   CALL      'TIBS69R'
038000981001     C                   PARM                    DSBS69
038100981001     C                   PARM                    DSACO
038200981001     C                   PARM                    DSIND
038300981001     C                   PARM                    DSCLP
038400981001     C                   PARM                    DSCLS
038500020604     c*
038600021112     C                   CLEAR                   tibs36ds
038700021112     C                   MOVEL     'C'           I36TLA
038800021112     C                   CALL      'TIBS36R'
038900021112     C                   PARM                    TIBS36DS
039000120424
039100121114
039200121114     C                   CLEAR                   tibs02ds
039300121114     C                   MOVEL     'C'           t02tla
039400121114     C                   CALL      'TIBS02R'
039500121114     C                   PARM                    kpjba
039600121114     C                   PARM                    tibs02ds
039700151009
039800151009     C                   MOVEL     'C'           iv1tla
039900151009     c                   movel     fnlvv1ds      kpjbu
040000151009     C                   CALL      'FNLVV1R'
040100151009     C                   PARM                    kpjba
040200920113     C*
040300920113     C     FINE          TAG
040400920113     C*
040500920113     C                   SETON                                        LR
040600920113     C**---------------------------------------------------------
040700920113     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
040800920113     C**---------------------------------------------------------
040900920113     C     CAEX          BEGSR
041000020827     C                   MOVE      ')'           VAR11            11
041100020827     C                   MOVEL     MBRfil        VAR11
041200020827     C                   Z-ADD     51            LUNG             15 5
041300020827     C                   Z-ADD     213           LUNG2            15 5
041400920113     C* CHK E ADD SU TUTTI I FILE FISICI
041500950202     C                   MOVEL     'FNARBD0R'    FISICO           10
041600930212     C                   CALL      'TRUL50C'
041700930212     C                   PARM                    FISICO
041800930212     C                   PARM                    MBRFIL
041900930212     C                   PARM                    LIBSYS
042000930212     C                   PARM                    LOGICO           10
042100930212     C                   PARM                    MBRLOG           10
042200930212     C                   PARM                    ULFLG             1
042300950202     C                   MOVEL     'FNARBD1R'    LOGICO
042400950202     C                   MOVEL     MBRFIL        MBRLOG
042500950202     C                   CALL      'TRUL50C'
042600950202     C                   PARM                    FISICO
042700950202     C                   PARM                    MBRFIL
042800950202     C                   PARM                    LIBSYS
042900950202     C                   PARM                    LOGICO           10
043000950202     C                   PARM                    MBRLOG           10
043100950202     C                   PARM                    ULFLG             1
043200950202     C*
043300950202     C                   CLEAR                   LOGICO
043400950202     C                   CLEAR                   MBRLOG
043500991015     C                   MOVEL     'FIARBT0R'    FISICO
043600930212     C                   CALL      'TRUL50C'
043700930212     C                   PARM                    FISICO
043800930212     C                   PARM                    MBRFIL
043900930212     C                   PARM                    LIBSYS
044000930212     C                   PARM                    LOGICO           10
044100930212     C                   PARM                    MBRLOG           10
044200930212     C                   PARM                    ULFLG             1
044300950201     C                   MOVEL     'FNARBK0R'    FISICO
044400930212     C                   CALL      'TRUL50C'
044500930212     C                   PARM                    FISICO
044600930212     C                   PARM                    MBRFIL
044700930212     C                   PARM                    LIBSYS
044800930212     C                   PARM                    LOGICO           10
044900930212     C                   PARM                    MBRLOG           10
045000930212     C                   PARM                    ULFLG             1
045100950202     C*
045200950201     C                   MOVEL     'FNARBM0R'    FISICO
045300930212     C                   CALL      'TRUL50C'
045400930212     C                   PARM                    FISICO
045500930212     C                   PARM                    MBRFIL
045600930212     C                   PARM                    LIBSYS
045700930212     C                   PARM                    LOGICO           10
045800930212     C                   PARM                    MBRLOG           10
045900930212     C                   PARM                    ULFLG             1
046000950202     C                   MOVEL     'FNARBM1R'    LOGICO
046100950202     C                   MOVEL     MBRFIL        MBRLOG
046200950202     C                   CALL      'TRUL50C'
046300950202     C                   PARM                    FISICO
046400950202     C                   PARM                    MBRFIL
046500950202     C                   PARM                    LIBSYS
046600950202     C                   PARM                    LOGICO           10
046700950202     C                   PARM                    MBRLOG           10
046800950202     C                   PARM                    ULFLG             1
046900950202     C*
047000950202     C                   CLEAR                   LOGICO
047100950202     C                   CLEAR                   MBRLOG
047200991015     C                   MOVEL     'FIARBU0R'    FISICO
047300950202     C                   CALL      'TRUL50C'
047400950202     C                   PARM                    FISICO
047500950202     C                   PARM                    MBRFIL
047600950202     C                   PARM                    LIBSYS
047700950202     C                   PARM                    LOGICO           10
047800950202     C                   PARM                    MBRLOG           10
047900950202     C                   PARM                    ULFLG             1
048000991015     C                   MOVEL     'FIARBU1R'    LOGICO
048100950202     C                   MOVEL     MBRFIL        MBRLOG
048200950202     C                   CALL      'TRUL50C'
048300950202     C                   PARM                    FISICO
048400950202     C                   PARM                    MBRFIL
048500950202     C                   PARM                    LIBSYS
048600950202     C                   PARM                    LOGICO           10
048700950202     C                   PARM                    MBRLOG           10
048800950202     C                   PARM                    ULFLG             1
048900950202     C*
049000950202     C                   CLEAR                   LOGICO
049100950202     C                   CLEAR                   MBRLOG
049200950202     C                   MOVEL     'FNARBG0R'    FISICO
049300950202     C                   CALL      'TRUL50C'
049400950202     C                   PARM                    FISICO
049500950202     C                   PARM                    MBRFIL
049600950202     C                   PARM                    LIBSYS
049700950202     C                   PARM                    LOGICO           10
049800950202     C                   PARM                    MBRLOG           10
049900950202     C                   PARM                    ULFLG             1
050000920113     C*
050100920113     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE FACCIO ADDLFM
050200920113     C                   SETOFF                                       20
050300920212     C                   MOVE      VAR11         CMD1
050400920113     C                   MOVEL     *BLANKS       COMMAN
050500920113     C                   MOVEA     CMD1(1)       COMMAN           80
050600920113     C                   CALL      'QCMDEXC'                            20
050700920113     C                   PARM                    COMMAN
050800920113     C                   PARM                    LUNG
050900920113     C*
051000920113     C   20              DO
051100950202     C                   MOVEA     CM5           C6
051200920113     C*
051300020827     C                   MOVEA     MBRFIL        C6(27)
051400020827     C                   MOVEA     MBRFIL        C6(69)
051500020827     C                   MOVEA     MBRFIL        C6(102)
051600020827     C                   MOVEA     MBRFIL        C6(135)
051700020827     C                   MOVEA     MBRFIL        C6(168)
051800020827     C                   MOVEA     MBRFIL        C6(201)
051900920113     C                   SETOFF                                       20
052000920113     C                   MOVEL     *BLANKS       COMMA2
052100020827     C                   MOVEA     C6            COMMA2          213
052200920113     C                   CALL      'QCMDEXC'                            H1
052300920113     C                   PARM                    COMMA2
052400920113     C                   PARM                    LUNG2
052500920113     C   20              GOTO      ENDCAE
052600920113     C                   END
052700020827
052800020827     C                   MOVE      '))'          VAR12            12
052900020827     C                   MOVEL     MBRfil        VAR12
053000920113     C*
053100920114     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
053200041006     C**                 SETOFF                                       20
053300041006     C**                 MOVEa     VAR12         CM2(40)
053400041006     C**                 MOVEA     'FIARBR3C'    CM2(13)
053500041006     C**                 MOVEL     *BLANKS       COMMAN
053600041006     C**                 MOVEA     CM2           COMMAN
053700041006     C**                 CALL      'QCMDEXC'                            20
053800041006     C**                 PARM                    COMMAN
053900041006     C**                 PARM                    LUNG
054000920113     C*
054100041006     C** 20              GOTO      ENDCAE
054200041006     C**
054300930212     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
054400041006     C**                 SETOFF                                       20
054500041006     C**                 MOVEa     VAR12         CM2(40)
054600041006     C**                 MOVEA     'FNARBM1R'    CM2(13)
054700041006     C**                 MOVEL     *BLANKS       COMMAN
054800041006     C**                 MOVEA     CM2           COMMAN
054900041006     C**                 CALL      'QCMDEXC'                            20
055000041006     C**                 PARM                    COMMAN
055100041006     C**                 PARM                    LUNG
055200041006     C**
055300041006     C** 20              GOTO      ENDCAE
055400041006     C**
055500041006     C**                 SETOFF                                       20
055600041006     C**                 MOVEa     VAR12         CM2(40)
055700041006     C**                 MOVEA     'FNARBD1R'    CM2(13)
055800041006     C**                 MOVEL     *BLANKS       COMMAN
055900041006     C**                 MOVEA     CM2           COMMAN
056000041006     C**                 CALL      'QCMDEXC'                            20
056100041006     C**                 PARM                    COMMAN
056200041006     C**                 PARM                    LUNG
056300041006     C**
056400041006     C** 20              GOTO      ENDCAE
056500041006     C**
056600041006     C**ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
056700041006     C**                 SETOFF                                       20
056800041006     C**                 MOVEa     VAR12         CM2(40)
056900041006     C**                 MOVEA     'FIARBU1R'    CM2(13)
057000041006     C**                 MOVEL     *BLANKS       COMMAN
057100041006     C**                 MOVEA     CM2           COMMAN
057200041006     C**                 CALL      'QCMDEXC'                            20
057300041006     C**                 PARM                    COMMAN
057400041006     C**                 PARM                    LUNG
057500041006     C**
057600041006     C** 20              GOTO      ENDCAE
057700041006     C**OVRDBF
057800041006     C                   SETOFF                                       20
057900020828     C                   MOVEa     VAR11         CM3(41)
058000950202     C                   MOVEA     'FNARBM1R'    CM3(13)
058100920113     C                   MOVEL     *BLANKS       COMMAN
058200920212     C                   MOVEA     CM3(1)        COMMAN
058300920113     C                   CALL      'QCMDEXC'                            20
058400920113     C                   PARM                    COMMAN
058500920113     C                   PARM                    LUNG
058600920212     C*
058700920212     C   20              GOTO      ENDCAE
058800950202     C                   SETOFF                                       20
058900020828     C                   MOVEa     VAR11         CM3(41)
059000991015     C                   MOVEA     'FIARBU1R'    CM3(13)
059100950202     C                   MOVEL     *BLANKS       COMMAN
059200950202     C                   MOVEA     CM3(1)        COMMAN
059300950202     C                   CALL      'QCMDEXC'                            20
059400950202     C                   PARM                    COMMAN
059500950202     C                   PARM                    LUNG
059600950202     C*
059700950202     C   20              GOTO      ENDCAE
059800920214     C* OVRDBF
059900920214     C                   SETOFF                                       20
060000020828     C                   MOVEa     VAR11         CM3(41)
060100950201     C                   MOVEA     'FNARBD1R'    CM3(13)
060200920214     C                   MOVEL     *BLANKS       COMMAN
060300920214     C                   MOVEA     CM3(1)        COMMAN
060400920214     C                   CALL      'QCMDEXC'                            20
060500920214     C                   PARM                    COMMAN
060600920214     C                   PARM                    LUNG
060700920214     C*
060800920214     C   20              GOTO      ENDCAE
060900920212     C*
061000991015     C                   MOVEA     'FIARBR3C'    CM3(13)
061100920212     C                   MOVEL     *BLANKS       COMMAN
061200920212     C                   MOVEA     CM3(1)        COMMAN
061300920212     C                   CALL      'QCMDEXC'                            20
061400920212     C                   PARM                    COMMAN
061500920212     C                   PARM                    LUNG
061600151008     C   20              GOTO      ENDCAE
061700151008      *
061800151008     C                   MOVEA     'FNARBD0R'    CM3(13)
061900151008     C                   MOVEL     *BLANKS       COMMAN
062000151008     C                   MOVEA     CM3(1)        COMMAN
062100151008     C                   CALL      'QCMDEXC'                            20
062200151008     C                   PARM                    COMMAN
062300151008     C                   PARM                    LUNG
062400151008     C   20              GOTO      ENDCAE
062500151008     C                   MOVEA     'FNARBK0R'    CM3(13)
062600151008     C                   MOVEL     *BLANKS       COMMAN
062700151008     C                   MOVEA     CM3(1)        COMMAN
062800151008     C                   CALL      'QCMDEXC'                            20
062900151008     C                   PARM                    COMMAN
063000151008     C                   PARM                    LUNG
063100151008     C   20              GOTO      ENDCAE
063200151008     C                   MOVEA     'FNARBG0R'    CM3(13)
063300151008     C                   MOVEL     *BLANKS       COMMAN
063400151008     C                   MOVEA     CM3(1)        COMMAN
063500151008     C                   CALL      'QCMDEXC'                            20
063600151008     C                   PARM                    COMMAN
063700151008     C                   PARM                    LUNG
063800151008     C   20              GOTO      ENDCAE
063900151008     C                   MOVEA     'FIARBT0R'    CM3(13)
064000151008     C                   MOVEL     *BLANKS       COMMAN
064100151008     C                   MOVEA     CM3(1)        COMMAN
064200151008     C                   CALL      'QCMDEXC'                            20
064300151008     C                   PARM                    COMMAN
064400151008     C                   PARM                    LUNG
064500151008     C   20              GOTO      ENDCAE
064600920113     C*
064700991015     C  N20              OPEN      FIARBR3C
064800950202     C  N20              OPEN      FNARBM1R
064900950201     C  N20              OPEN      FNARBD1R
065000991015     C  N20              OPEN      FIARBU1R
065100151008     C  N20              OPEN      FNARBD0R
065200151008     C  N20              OPEN      FNARBK0R
065300151008     C  N20              OPEN      FNARBG0R
065400151008     C  N20              OPEN      FIARBT0R
065500920113     C*
065600920113     C     ENDCAE        ENDSR
065700920113     C**---------------------------------------------------------
065800920113     C** AGGIORNO BOLLA PARTENZE
065900920113     C**---------------------------------------------------------
066000920113     C     AGGBOL        BEGSR
066100920130     C*
066200941130     C                   SELECT
066300061110     C     FLGCVB        WHENEQ    'F'
066400061110     C                   EXSR      AGARBF
066500061110     C*
066600941130     C     FLGCVB        WHENEQ    'I'
066700920113     C                   EXSR      AGARBI
066800941130     C*
066900941130     C     FLGCVB        WHENEQ    'K'
067000920127     C                   EXSR      AGARBK
067100920228     C*
067200941130     C     FLGCVB        WHENEQ    'T'
067300920113     C                   EXSR      AGARBT
067400920113     C*
067500941130     C     FLGCVB        WHENEQ    '2'
067600920113     C                   EXSR      AGAR2T
067700950202     C*
067800950202     C     FLGCVB        WHENEQ    'M'
067900950202     C                   EXSR      AGARBM
068000950202     C*
068100950202     C     FLGCVB        WHENEQ    'C'
068200950202     C                   EXSR      AGARBC
068300060317     C*
068400060317     C     FLGCVB        WHENEQ    'R'
068500060317     C                   EXSR      AGARBR
068600941130     C                   ENDSL
068700920113     C*
068800930513     C                   ENDSR
068900061110     C**---------------------------------------------------------
069000061110     C** AGGIORNO CODICE VARIAZIONI FX - codice fiscale
069100061110     C**---------------------------------------------------------
069200061110     C     AGARBF        BEGSR
069300061110     c
069400061110     C                   SETOFF                                       3126
069500061110     C                   SETOFF                                       2225
069600061110     C                   MOVE      TASKSC        KSC4              4 0
069700071029     c* Vedo quale bolla aggiornare
069800071029     c                   clear                   bolla             1
069900071029     c* Destinatario
070000071029    1c                   if        arbrd2='D'
070100071029     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
070200071029    2C     *IN27         IFEQ      '1'
070300071029     C     KSC4          ANDEQ     9999
070400071029     C     TASNFT        ANDGT     0
070500071029     c                   eval      bolla ='1'
070600071029    2c                   endif
070700071029     c
070800071029    2C     *IN24         ifeq      '1'
070900071029     C     SAVKS4        ANDEQ     9999
071000071029     C     SAVNFT        ANDGT     0
071100071029     c                   eval      bolla ='2'
071200071029    2c                   endif
071300071029     c
071400071029   x1c                   else
071500071029     c* Mittente
071600071029    2C     *IN27         ifeq      '1'
071700071029     C     KSC4          ANDEQ     8888
071800071029     C     TASNFT        ANDGT     0
071900071029     c                   eval      bolla ='1'
072000071029    2c                   endif
072100071029    1c                   endif
072200071029     c*
072300061110     C*
072400061110     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
072500071029    1c                   if        bolla<>' '
072600061110     C*
072700071029     C* SE SI TRATTA DI storno ignoro
072800071029    2c                   if        bolla='2'
072900061110     C                   MOVEL(P)  SAVTBL        KEY
073000061110     C                   ELSE
073100061110     C                   MOVEL(P)  TASTBL        KEY
073200061113    2C                   ENDIF
073300061110     C*
073400061110     C                   MOVEL     'TB'          COD
073500061110     C     KTAB          CHAIN     TABEL                              32
073600061110     C  N32              MOVEL     TBLUNI        DSTB
073700061110     C   32              MOVEL     *ALL'1'       DSTB
073800071029     c
073900061110     C*  FCB="1" ASSEGNATO BUONO
074000061113    2C     §TBFCB        IFEQ      '1'
074100061110     C*
074200061110     c                   clear                   wdft
074300061113    3c                   select
074400071029     c***                when      *in27
074500071029     c                   when      bolla='1'
074600061110     c                   move      tasnft        wnft
074700061110     c                   if        tasdft > 0
074800061110     c                   move      tasdft        wdft
074900061110     c                   end
075000061110     c                   move      tasksc        wksc
075100071029     c***                when      *in24
075200071029     c                   when      bolla='2'
075300061110     c                   move      savnft        wnft
075400061110     c                   if        savdft > 0
075500061110     c                   move      savdft        wdft
075600061110     c                   end
075700061110     c                   move      savksc        wksc
075800061113    3c                   endsl
075900061110     c
076000061110     C     Kppa          CHAIN     ndppa01l                           26
076100061113    3C     *IN26         IFEQ      *OFF
076200061110     c     kycpa         chain     ndcpa01l                           26
076300061113    4C     *IN26         IFEQ      *OFF
076400071102     c
076500061110     C                   MOVEL     ARBCPI        cpacdfisc
076600071102     c
076700071102     c* Solo se reperita o partita la procedura aggiorno
076800120510     c*****              if        arbrsd<>*blanks or arbdtv>=§vpopiv
076900071029     C                   MOVEL     ARBRSD        cpapartiva
077000120510     c*****              endif
077100071102     c
077200061110     C                   UPDATE    ndcpa000
077300061113    4C                   ENDIF
077400061113    3C                   ENDIF
077500061110     C*
077600061113    2C                   END
077700061113    1C                   END
077800061110     c
077900061110     C* Aggiorna record Q di TITA4
078000061110     C                   movel     'Q'           §trc
078100061110     c                   setoff                                       0506
078200061110     C     ktita         chain     tita430c                           32
078300061113     c* se non c'e' lo scrivo
078400061113    1c                   if        *in32
078500061113     c                   movel     arbaas        ta4aas
078600061113     c                   movel     arblnp        ta4lnp
078700061113     c                   movel     arbnrs        ta4nrs
078800061113     c                   movel     arbnsp        ta4nsp
078900061113     c                   movel     'Q'           ta4trc
079000061113     c                   clear                   ta4not
079100061113    1c                   endif
079200061113     c
079300061110     c* SE mittente a SX , destinatario a DX
079400071029    1c                   if        arbrd2='M'
079500061110     c                   movel     arbcpi        ta4not
079600061110     c                   else
079700061110     c                   move      arbcpi        ta4not
079800061113    1c                   endif
079900061110     c*
080000061113    1c                   if        not *in32
080100061113    2C                   IF        TA4NOT<>*BLANKS
080200061110     C   05              update    tita4000
080300061110     C   06              update    tita4p00
080400061113     C                   ELSE
080500061113     C*SE eliminata e non c'e' più nulla, cancello il record
080600061113     c   05              delete    tita4000
080700061113     c   06              delete    tita4p00
080800061113    2c                   endif
080900061113     c
081000061113   x1c                   else
081100061113    2c                   if        ta4not<>*blanks
081200061113     C  N04              write     tita4000
081300061113     C   04              write     tita4p00
081400061113    2c                   endif
081500061113     c*
081600061113    1c                   endif
081700071029     c
081800071029     c* Aggiorno partita iva
081900071102     c*                      solo se presente o partita la procedura
082000120510    0c******             if        arbrsd<>*blanks or arbdtv>=§vpopiv
082100071029     c* Destinatario --> in titas
082200071029    1c                   if        arbrd2='D'
082300071029     c                   movel     arbrsd        tascpd
082400071029     C* AGGIORNO TITAS
082500071029     C   02              UPDATE    TITAS000
082600071029     C   03              UPDATE    TITAS010
082700071029     C   04              UPDATE    TITASP00
082800071102   x1c                   else
082900071029     c* Mittente --> in titaa
083000071029     c* Aggancio TITAA e aggiorno la partita del mittente
083100071029     c                   movel     'M'           §trc
083200071029     c                   setoff                                       0708
083300071029     c     ktita         chain     titaa30c                           30
083400071102IF  2C                   if        %found(titaa30c)
083500071029     C                   MOVEL     ARBrsd        TAacpi
083600071029     c   07              update    titaa000
083700071029     c   08              update    titaap00
083800071102    2c                   endif
083900071102    1c                   endif
084000071029     C*
084100071029     C* S E C O N D A   B O L L A
084200071029     C*
084300071029     c* solo per destinatario
084400071029    1c                   if        arbrd2='D'  and §3atb2<>'  '
084500071029     C                   MOVEL     §3ATB2        COMTBL
084600071029     C                   SETOFF                                       020304
084700071029     C     KBLP          CHAIN     TiTAS30C                           39
084800071029     c* Aggiorno partita iva
084900071029     c                   movel     arbrsd        tascpd
085000071029     C* AGGIORNO TITAS
085100071029     C   02              UPDATE    TITAS000
085200071029     C   03              UPDATE    TITAS010
085300071029     C   04              UPDATE    TITASP00
085400071102    1C                   END
085500120510    0C*****              END
085600061110     C*
085700061110     C                   ENDSR
085800920113     C**---------------------------------------------------------
085900920113     C** AGGIORNO CODICE VARIAZIONI IX
086000920113     C**---------------------------------------------------------
086100920211     C     AGARBI        BEGSR
086200941130     C                   SETOFF                                       3126
086300941130     C                   SETOFF                                       2225
086400930513     C                   MOVE      TASKSC        KSC4              4 0
086500930513     C*
086600981118     C* chain su YNCPA01L per aggiornare la PIVA e dati destinatario
086700930513    1C     *IN27         IFEQ      '1'
086800930513     C     KSC4          ANDEQ     9999
086900960625     C     TASNFT        ANDGT     0
087000930513     C     *IN24         OREQ      '1'
087100930513     C     SAVKS4        ANDEQ     9999
087200960625     C     SAVNFT        ANDGT     0
087300960625     C*
087400960625     C* SE SI TRATTA DI ASSEGNATO STORNO IGNORO
087500960625     C     *IN24         IFEQ      '1'
087600960625     C     SAVKS4        ANDEQ     9999
087700960625     C                   MOVEL(P)  SAVTBL        KEY
087800960625     C                   ELSE
087900960625     C                   MOVEL(P)  TASTBL        KEY
088000960625     C                   ENDIF
088100960625     C*
088200960625     C                   MOVEL     'TB'          COD
088300960625     C     KTAB          CHAIN     TABEL                              32
088400960625     C  N32              MOVEL     TBLUNI        DSTB
088500960625     C   32              MOVEL     *ALL'1'       DSTB
088600960625     C*  FCB="1" ASSEGNATO BUONO
088700960625   1AC     §TBFCB        IFEQ      '1'
088800930513     C*
088900981118     c                   clear                   wdft
089000981118     c                   select
089100981118     c                   when      *in27
089200981118     c                   move      tasnft        wnft
089300981118     c                   if        tasdft > 0
089400981118     c                   move      tasdft        wdft
089500981118     c                   end
089600981118     c***                move      tasfiv        wfiv
089700981118     c                   move      tasksc        wksc
089800981118     c                   when      *in24
089900981118     c                   move      savnft        wnft
090000981118     c                   if        savdft > 0
090100981118     c                   move      savdft        wdft
090200981118     c                   end
090300981118     c***                move      savfiv        wfiv
090400981118     c                   move      savksc        wksc
090500981118     c                   endsl
090600981118     C     Kppa          CHAIN     ndppa01l                           26
090700960625    2C     *IN26         IFEQ      *OFF
090800990518     c     kycpa         chain     ndcpa01l                           26
090900981118    2C     *IN26         IFEQ      *OFF
091000990518     C                   MOVEL     ARBRSD        cpadescr
091100990518     C                   MOVEL     ARBIND        cpaindiriz
091200981118     C                   MOVEL     ARBCAD        cpacap
091300990518     C                   MOVEL     ARBLOD        cpalocalit
091400981118     C                   MOVEL     ARBPRD        cpaprov
091500981118     c                   movel     'IT  '        cpastato
091600981118     c     arbnzd        ifne      *blank
091700981118     c     arbnzd        andne     'I  '
091800981118     c     arbnzd        andne     'IT '
091900981118     c                   movel     arbnzd        cpastato
092000981118     c                   endif
092100981118     C                   MOVEL     ARBCPI        cpapartiva
092200930513     C*
092300990518     C                   UPDATE    ndcpa000
092400950302    2C                   ENDIF
092500981118    2C                   ENDIF
092600930514     C*
092700960625    1C                   END
092800981118    1C                   END
092900930513     C*
093000950203     C                   MOVEL     §3ATB1        UNOTB1            1
093100920601     C*
093200941130     C* SE CAUSALE VARIAZIONE I0 --> AGGANCIO ARCHIVIO C/ASSEGNI
093300140407    2C*    COMCVB        IFEQ      'I0'
093400140407     C*    §3AFCA        ANDNE     0
093500140407     c                   if        (comcvb='I0' or comcvb='I7' or comcvb='I8'
093600140407     c                             or comcvb='I9') and §3afca<>0
093700930513     C*
093800920212     C                   EXSR      CONCSB
093900941130     C* ERRORE --> NON TROVATO RECORD IN CSB E NON TROVATO LEGAME
094000941130     C   25              GOTO      ENDAGI
094100950227     C* SE IL C/A E' SOLO INCASSATO, POSSO EFFETTUARE LA VARIAIZONE DI
094200950227     C*  DESTINATARIO
094300950227     C  N42CSBBNA        IFEQ      0
094400950227     C                   SETOFF                                       31
094500950227     C                   ENDIF
094600920130     C*
094700950227     C* CONTRASSEGNO ESISTENTE GIA' PAGATO
094800941130    3C     *IN31         IFEQ      '1'
094900930303     C*
095000930303     C* SE ESISTE LEGAME BOLLA (42 ON) NON AGGIORNO ANAGRAFICA C/ASS
095100930303     C* MA AGGIORNO LA BOLLA
095200930303     C*
095300930513    4C     *IN42         IFEQ      '1'
095400930303     C                   SETOFF                                       31
095500930513   X4C                   ELSE
095600941202     C* CONTROLLO SE LA RAGIONE SOCIALE DESTINATARIO E' MODIFICATA
095700941202     C*  --> ERRORE SE C/A GIA' INCASSATO-PAGATO
095800941202     C                   EXSR      CTRDES
095900930513     C*
096000930513    4C                   END
096100920601     C* STAMPO ERRORE
096200941130     C     *IN31         IFEQ      *ON
096300941130     C     *IN42         ANDEQ     *OFF
096400110214     c* ES-14 02 11 -da oggi non stampo più C/Ass pagato per la 102 ma lo stampo
096500110214     c*  per la linea di arrivo se ci fossero problemi o contestazioni la colpa...è sua
096600110214     c* FNARBM non mi serve non accendo il 36
096700130326     c****               movel     taslna        COMFEV
096800110214     C****               SETON                                        36
096900110214     C****               Z-ADD     102           COMFEV
097000130326     c                   z-add     46            COMFEV
097100941130     C                   MOVEL     ERR(10)       ARBDER
097200941130     C                   EXSR      WTRERR
097300930513    3C                   END
097400941130    3C                   END
097500920601     C*
097600941130    3C  N42*IN31         IFEQ      '0'
097700040721     C**  N27
097800040721     C**ANN24              MOVEL     ARBFIN        CSBFIN
097900920115     C                   MOVEL     ARBLOD        CSBPAR
098000920115     C                   MOVEL     ARBPRD        CSBPRO
098100920115     C                   MOVEL     ARBRSD        CSBRSD
098200941130     C                   UPDATE    TNCSB000
098300930513    3C                   END
098400930513    2C                   END
098500920115     C*
098600990625     c                   exsr      agarbi1
098700920113     C*
098800920113     C* S E C O N D A   B O L L A
098900920113     C*
099000930513    3C     §3ATB2        IFNE      '  '
099100920113     C                   MOVEL     §3ATB2        COMTBL
099200941130     C                   SETOFF                                       020304
099300990624     C     KBLP          CHAIN     TiTAS30C                           39
099400121108     c                   exsr      memo_dstitas
099500990628     c                   move      tasksc        ksc4
099600990625     c                   exsr      agarbi1
099700930513    3C                   END
099800941130     C*
099900941130     C     ENDAGI        TAG
100000920113     C*
100100920113     C                   ENDSR
100200990625     C**---------------------------------------------------------
100300990625     C** AGGIORNO CODICE VARIAZIONI IX
100400990625     C**---------------------------------------------------------
100500990630     C     AGARBI1       BEGSR
100600990628     C* I SEGUENTI CAMPI LI AGGIORNO SOLO SE 1 BOLLA NON CONTABILIZZATA
100700990628    1C     *in27         IFEQ      *off
100800990628     C                   Z-ADD     ARBPKb        TASPKB
100900990628     C                   Z-ADD     ARBPKF        TASPKf
101000151103     c* non devo mai aggiornare questo flag perchè quando si tassa fino all'imponibile
101100151103     c*  viene impostato sia qui che nel record  FAT e non deve più essere modificato
101200151103     C********           movel     'R'           TASFPF
101300990625     C*
101400990628    2C     TASFVF        IFNE      'T'
101500990625     C     TASVLF        ANDNE     ARBVLF
101600990628    3C     TASFVF        IFEQ      'Z'
101700990625     C     TASVLF        ANDLT     ARBVLF
101800990625     C     TASFVF        ORNE      'Z'
101900990625     C                   Z-ADD     ARBVLF        TASVLF
102000990625     C                   MOVEL     ARBFVF        TASFVF
102100990625     C                   Z-ADD     ARBVLB        TASVLB
102200990625     C                   MOVEL     ARBFVB        TASFVB
102300990628    3C                   END
102400990628    2C                   END
102500990625     C*
102600990630    2c     ksc42bo       ifne      9999
102700990625     C* DETERMINO IL CODICE DI TASSAZIONE
102800990630    3C     COMCVB        ifeq      'I0'
102900140407    3C     COMCVB        oreq      'I7'
103000140407    3C     COMCVB        oreq      'I8'
103100140407    3C     COMCVB        oreq      'I9'
103200990628     c* la call al tisi mi basta farla solo la prima volta e cioe quando
103300990625     c* sono sulla prima bolla
103400990630    4c     comtbl        ifeq      §3atb1
103500120322     c                   clear                   savcts
103600120322     c                   exsr      c_tisi95
103700120322     C* SE ERRORE da tisi95R LO SCRIVO NEL FILE DEGLI ERRORI E NON LO ANDRO'
103800990625     C* A SOSTITUIRE
103900990630    5C     O95ERR        IFNE      *BLANKS
104000990625     C                   MOVEL     ERR(30)       ARBDER
104100990625     C                   Z-ADD     102           COMFEV
104200990625     C                   EXSR      WTRERR
104300990630   X5C                   ELSE
104400990630     C                   MOVEL     O95CTS        SAVCTS
104500990630    5C                   END
104600990630    4c                   END
104700990630    4c     SAVCTS        IFNE      *BLANKS
104800990630     C                   MOVEL     SAVCTS        TASCTS
104900990630    4C                   END
105000990630    3C                   END
105100990625
105200990625     C                   MOVEL     ARBFIN        TASFIN
105300990625     C                   MOVEL     ARBCAD        TASCAD
105400990625     C                   MOVEL     ARBNZD        TASNZD
105500990630    2C                   END
105600990630    1C                   END
105700990625     C*
105800990625RM*  C                   MOVEL     ARBFFD        TASFFD
105900990625     C*
106000990628     c* memorizzo se sono variati inoltro,cap o nazione
106100990625     c                   clear                   flgmod
106200990628    1c     arbfin        ifne      tasfin
106300990625     c     arbcad        orne      tascad
106400990625     c     arbnzd        orne      tasnzd
106500990625     c                   move      '1'           flgmod            1
106600990628    1c                   endif
106700990625     c
106800990628     c* Se ho la seconda bolla contabilizzata 9999 non aggiornerò mai i
106900990628     c* i seguenti campi nemmeno sulla prima bolla.
107000990630     c* Dopodichè posso aggiornarli solo se la 1bolla non è contabilizzata
107100990628     c* o se non sono stati modificati inoltro cap e nazione
107200990628    1c     ksc42bo       ifne      9999
107300990628    2c     *in27         ifeq      *off
107400990628     c     flgmod        oreq      *blank
107500990625     C                   MOVEL(P)  ARBRSD        TASRSD
107600990625     C                   MOVEL(P)  ARBIND        TASIND
107700990625     C                   MOVEL(P)  ARBLOD        TASLOD
107800990625     C                   MOVEL(P)  ARBPRD        TASPRD
107900990701     c* Dalla filiale posso ricevere la partita iva del destinatario
108000990701     c* solo se la bolla è assegnato o solo in caso di doppia bolla
108100990701     c* In ogni caso vado SEMPRE in sostituzione di tascpd
108200990625     C                   MOVEL     ARBCPI        TASCPD
108300140219    2c                   endif
108400140219    1c                   endif
108500990625     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
108600130325     C***                MOVEL     ERR(9)        ARBDER
108700130325     c***                exsr      wtrerr
108800140219     c* La scrittura/aggiornamento del tivrb viene fatta sempre se la
108900140219     c* prima bolla è contabilizzata.
109000140219     c* Viene fatta a prescindere dal contenuto di FLGMOD perchè
109100140219     c* deve receprire anche eventuali variazioni che riportano
109200140219     c* i dati alla situazione originale ciè quella di titas
109300140219     c* Es: se prima vario inoltor da "P" a "I" e poi rimetto
109400140219     c* "P" devo receprie sul tivrb anche quest'ultima variazione
109500140219    1c                   if        *in27=*on
109600120321     c                   exsr      ctr_vrb
109700140219    1c                   END
109800140219   x1c*                  else
109900990628     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
110000130325     C***                MOVEL     ERR(9)        ARBDER
110100130325     c***                exsr      wtrerr
110200140219     c*                  exsr      ctr_vrb
110300140219    1c*                  END
110400990625     C* AGGIORNO TITAS
110500990625     C   02              UPDATE    TITAS000
110600990625     C   03              UPDATE    TITAS010
110700990625     C   04              UPDATE    TITASP00
110800990625     C*
110900990625     c                   endsr
111000120322     C**---------------------------------------------------------
111100120322     C** Richiamo tisi95 per determinazione codice tassazione
111200120322     C**---------------------------------------------------------
111300120322     C     C_tisi95      BEGSR
111400120322     C                   CLEAR                   DSSI95
111500120322     C                   MOVEL     ARBCAD        I95CAP
111600120322     C                   MOVEL     ARBNZD        I95NAR
111700120322     C                   MOVEL     ARBLOD        I95LOC
111800120322     C                   MOVEL     ARBPRD        I95PRV
111900121116     c                   movel     tasaas        i95dat
112000121116     c                   move      tasmgs        i95dat
112100121116     C****               MOVEL     TASDSP        I95DAT
112200120322     C                   MOVEL     '7'           I95TCN
112300120322      * Se network FedEx passo flag apposito per dire di prendere i dati
112400120322      * dalla tabella della nazione
112500120322     c     taslna        chain     azorg01l
112600120322     c                   if        %found(azorg01l) and orgfva = *blanks
112700120322     c                   eval      og143 = orgde3
112800120322     c     §ogntw        ifeq      'FED'
112900120322     c                   eval      i95fi1 = 'S'
113000120322     c                   endif
113100120322     c                   endif
113200120322
113300120322     C                   MOVEL     §3ATB1        I95TPO
113400120322    5C     §3AFCA        IFNE      0
113500120322     C                   MOVEL     'S'           I95FCA
113600120322    5C                   ENDIF
113700120322     C**
113800120322     C                   CALL      'TISI95R'
113900120322     C                   PARM                    DSSI95
114000120322     c                   endsr
114100950202     C**---------------------------------------------------------
114200950202     C** AGGIORNO CODICE VARIAZIONI MX
114300950202     C**---------------------------------------------------------
114400950202     C     AGARBM        BEGSR
114500950203     C* VEDO SE ESISTE CODICE
114600950202     C                   MOVE      ARBCCM        KSC4
114700950202     C*
114800950202     C* SE E' CODIFICATO CHAIN SU ANAGRAFICA
114900950202     C     KSC4          IFNE      8888
115000981001     C                   CLEAR                   DSBS69
115100981001     C                   MOVEL     KNSIF         I69SIF
115200981001     C                   MOVEL     KCI           I69KCC
115300981001     C                   MOVEL     ARBCCM        I69KAC
115400981001     C                   MOVEL     ARBCCM        I69KIN
115500981001     C                   CALL      'TIBS69R'
115600981001     C                   PARM                    DSBS69
115700981001     C                   PARM                    DSACO
115800981001     C                   PARM                    DSIND
115900981001     C                   PARM                    DSCLP
116000981001     C                   PARM                    DSCLS
116100950202     C*
116200950203     C* DO ERRORE SOLO SE SI TRATTA DI CLIENTE DI ALTRA FILIALE
116300981001     C     O69ERR        IFNE      ' '
116400950203     C* CONFRONTO LE 2 FILIALI ELABORATORE
116500971215     C**         ARBLNP    CHAINAZORG                30
116600971215     C**      N30          MOVELORGFEL    LNPFEL  30
116700971215     C**       30          MOVELARBLNP    LNPFEL
116800971215     C**
116900971215     C**                   MOVELARBCCM    W0030   30
117000971215     C**         W0030     CHAINAZORG                30
117100971215     C**      N30          MOVELORGFEL    KSCFEL  30
117200971215     C**       30          MOVELW0030     KSCFEL
117300971215     C*
117400971215     C**         KSCFEL    IFNE LNPFEL
117500971215     C**                   MOVELERR,27    ARBDER
117600971215     C**                   EXSR WTRERR
117700950203     C*
117800971215     C**                   ELSE
117900950203     C* SE STESSA FIL ELABORATORE: NON CANCELLO LA VARIAZIONE
118000950203     C*  E ATTENDO L'ARRIVO DEL PDC
118100950203     C                   SETON                                        37
118200971215     C**                   ENDIF
118300950203     C*
118400950202     C                   GOTO      ENDGBM
118500960919     C                   ELSE
118600960919     C* SE CLIENTE BLOCCATO CARICO LO STESSO MA SEGNALO
118700100303     C***  ACOABL        IFEQ      '8'
118800100303     C***                MOVEL     ERR(29)       ARBDER
118900100303     C***                EXSR      WTRERR
119000100303     C***                END
119100950202     C                   ENDIF
119200950202     C                   ENDIF
119300950203     C*
119400950203     C* VERIFICO ESISTENZA NAZIONE E PROVINCIA MITTENTE
119500970714     C                   CLEAR                   DSSI95
119600970714     C                   MOVEL     ARBCAM        I95CAP
119700121116     C                   MOVEL     ARBNZM        I95NAR
119800121116     c                   movel     tasaas        i95dat
119900121116     c                   move      tasmgs        i95dat
120000121116     C****               MOVEL     TASDSP        I95DAT
120100970714     C                   MOVEL     '3'           I95TCN
120200020311      * Se network FedEx passo flag apposito per dire di prendere i dati
120300020311      * dalla tabella della nazione
120400020311     c     arblnp        chain     azorg01l
120500020311     c                   if        %found(azorg01l) and orgfva = *blanks
120600020311     c                   eval      og143 = orgde3
120700020311     c     §ogntw        ifeq      'FED'
120800020311     c                   eval      i95fi1 = 'S'
120900020311     c                   endif
121000020311     c                   endif
121100020311
121200970714     C                   CALL      'TISI95R'
121300970714     C                   PARM                    DSSI95
121400950203     C*
121500970714     C     O95ERR        IFNE      *BLANKS
121600130326     C                   Z-ADD     102           COMFEV
121700950203     C                   MOVEL     ERR(26)       ARBDER
121800950203     C                   EXSR      WTRERR
121900950203     C                   GOTO      ENDGBM
122000950203     C                   ENDIF
122100950203     C*
122200950203     C* VERIFICO SE ESISTEN L'ANAGRAFICA DEI C/A E SE GIA' INCASSATO
122300950222    1C     §3AFCA        IFNE      0
122400950202     C                   EXSR      CONCSB
122500950222     C* INESISTENTE  --> NON AGGIORNO
122600950222     C   25              GOTO      ENDGBM
122700950203     C***
122800950203     C*** A G G I O R N O   C / A S S E G N O
122900950222     C* AGGIORNO SOLO SE NON E' INCASSATO
123000950222   1AC     *IN31         IFEQ      *OFF
123100950202     C     KANN          CHAIN     TNCSA000                           30
123200950202     C*
123300950202     C* NON E' 8888 :SE CREATA CANCELLO ANAGRAFICA C/A
123400950202IF  2C     KSC4          IFNE      8888
123500970916IF  2C     KSC4          ANDNE     9999
123600950202     C  N30              DELETE    TNCSA000
123700950202X   2C                   ELSE
123800950202     C                   CLEAR                   TNCSA000
123900950202     C*
124000950202     C                   MOVEL     CSBAAS        CSAAAS
124100950202     C                   MOVEL     CSBLNP        CSALNP
124200950202     C                   MOVEL     CSBNRS        CSANRS
124300950202     C                   MOVEL     CSBNSP        CSANSP
124400950202     C                   MOVEL     ARBRSM        CSARSD
124500950202     C                   MOVEL     ARBINM        CSAVID
124600950202     C                   MOVEL     ARBCAM        CSACAD
124700950202     C                   MOVEL     ARBLOM        CSACID
124800950202     C                   MOVEL     ARBPRM        CSAPRM
124900950202     C                   MOVEL     ARBNZM        CSANZM
125000950202     C*
125100950202     C  N30              UPDATE    TNCSA000
125200950202     C   30              WRITE     TNCSA000
125300950202E   2C                   ENDIF
125400950202     C*
125500950202     C                   MOVEL     ARBCCM        CSBCDI
125600950202     C*
125700950202     C* SE C/A SU 1 BOLLA IMPOSTO CODICE BOLLA AMCHE
125800950202    2C     §3AFCA        IFEQ      1
125900950202     C                   MOVEL     ARBCCM        CSBKSC
126000950202    2C                   ENDIF
126100950202     C*
126200950202     C                   UPDATE    TNCSB000
126300950222   1AC                   ENDIF
126400950222    1C                   ENDIF
126500950202     C*
126600950203     C* A G G I O R N O   L A   1  B O L L A
126700970714     C                   MOVEL     O95CTS        TASMCT
126800950202     C                   MOVEL     ARBCAM        TASCAM
126900950202     C                   MOVEL     ARBNZM        TASNZM
127000950202     C                   MOVEL     ARBFAP        TASFAP
127100950202     C                   MOVEL     ARBCCM        TASKSC
127200990628     C                   MOVEL     ARBCCM        TASccm
127300950202     C*
127400950202     C*
127500990628     c                   movel     'M'           §trc
127600990629     c     ktita         chain     titaa000                           30
127700950202IF  3C     KSC4          IFEQ      8888
127800990628     c   30              z-add     tasaas        taaaas
127900990628     c   30              z-add     taslnp        taalnp
128000990628     c   30              z-add     tasnrs        taanrs
128100990628     c   30              z-add     tasnsp        taansp
128200990628     c   30              movel     §trc          taatrc
128300990628     C                   MOVEL     ARBRSM        TAaRSc
128400990628     C                   MOVEL     ARBINM        TAaIND
128500990628     C                   MOVEL     ARBCAM        TAaCAP
128600990628     C                   MOVEL     ARBLOM        TAaLOc
128700990628     C                   MOVEL     ARBPRM        TAaPRV
128800990628     C                   MOVEL     ARBNZM        TAaNAZ
128900990628     C                   MOVEL     ARBCPI        TAacpi
129000990628     c  n30              update    titaa000
129100990628     c   30              write     titaa000
129200990628     c                   else
129300990628     c* se prima della variazione era un 8888 lo vado a cancellare da
129400990628     c* titaa00f
129500990628     c  n30              delete    titaa000
129600990628     c
129700950202E   3C                   ENDIF
129800950202     C*
129900990628     C   02              UPDATE    TiTAS000
130000071029     C***
130100950203     C*** A G G I O R N O   2   B O L L A
130200950202    3C     §3ATB2        IFNE      '  '
130300950202     C                   MOVEL     §3ATB2        COMTBL
130400950202     C                   SETOFF                                       020304
130500990628     C     KBLP          CHAIN     TiTAS30C                           39
130600950202     C*
130700950203     C     *IN39         IFEQ      *OFF
130800950202     C                   MOVEL     ARBCCM        TASCCM
130900950202     C*
131000950202     C                   MOVEL     ARBCAM        TASCAM
131100950202     C                   MOVEL     ARBNZM        TASNZM
131200950202     C                   MOVEL     ARBFAP        TASFAP
131300970714     C                   MOVEL     O95CTS        TASMCT
131400950202     C*
131500990628     C   02              UPDATE    TiTAS000
131600990628     C   03              UPDATE    TiTAS010
131700990628     C   04              UPDATE    TiTASP00
131800950202     C                   ENDIF
131900950202     C                   ENDIF
132000950202     C*
132100950202     C     ENDGBM        ENDSR
132200950202     C**---------------------------------------------------------
132300950202     C** AGGIORNO CODICE VARIAZIONI CX
132400950202     C**---------------------------------------------------------
132500950202     C     AGARBC        BEGSR
132600010126     C     arblnp        ifne      SAVLNP
132700010126     C     arblnp        chain     azorg01l                           32
132800010126     C  n32              movel     orgde3        og143
132900010126     C   32              clear                   og143
133000010126     C                   z-add     arblnp        savlnp
133100010126     C                   endif
133200010126     C* SE BOLLA POSTE ED AGGIUNTO O TOLTO IL FUORI MISURA BISOGNA
133300010126     C* AGGIORNARE tita4 TIPO RECORD "J"
133400010126     C     §ogpt         ifeq      'S'
133500010126     C                   clear                   wtcp
133600010126     C                   clear                   wsvtcp
133700010126    2C     arbtc1        ifeq      'F'
133800010126     C     arbtc2        oreq      'F'
133900010126     C                   movel     'F'           wtcp
134000010126    2C                   endif
134100010126    2C     tasftc        ifeq      'F'
134200010126     C     tastc2        oreq      'F'
134300010126     C                   movel     'F'           wsvtcp
134400010126    2C                   endif
134500010126    2C                   select
134600010126     C* SE NON E' STATA NE' AGGIUNTA NE' TOLTA LA "F" NON FACCIO NIENTE
134700010126     C     arbtc1        whenne    'F'
134800010126     C     arbtc2        andne     'F'
134900010126     C     tasftc        andne     'F'
135000010126     C     tastc2        andne     'F'
135100010126     C     wtcp          oreq      'F'
135200010126     C     wsvtcp        andeq     'F'
135300010126     C                   other
135400010126     C                   movel     'J'           §trc
135500010126     c                   setoff                                       0506
135600010126     C     ktita         chain     tita430c                           32
135700010126    3C     *in32         ifeq      *off
135800010126     C                   movel     ta4not        dsbl4j
135900010126     C* TOLTO LA 'F'
136000010126    4C     wtcp          ifeq      *blanks
136100010126     C     wsvtcp        andeq     'F'
136200010126     C                   clear                   §b4bai
136300010126   X4C                   else
136400010126     C* AGGIUNTA 'F'
136500010126    5C     wtcp          ifeq      'F'
136600010126     C     wsvtcp        andeq     *blanks
136700010126     C                   movel     'S'           §b4bai
136800010126    5C                   endif
136900010126    4C                   endif
137000010126     C                   movel     dsbl4j        ta4not
137100010126     C   05              update    tita4000
137200010126     C   06              update    tita4p00
137300010126   X3C                   else
137400010126     C* AGGIUNTA 'F' --> CREO RECORD "J"
137500010126    4C     wtcp          ifeq      'F'
137600010126     C     wsvtcp        andeq     *blanks
137700010126     C                   clear                   tita4000
137800010126     C                   z-add     tasaas        ta4AAS
137900010126     C                   z-add     taslnp        ta4LNP
138000010126     C                   z-add     tasnrs        ta4NRS
138100010126     C                   z-add     tasnsp        ta4NSP
138200010126     C                   movel     'J'           ta4TRC
138300010126     C                   clear                   dsbl4j
138400010126     C                   clear                   ta4not
138500010126     C                   movel     'S'           §B4BAI
138600010126     C                   movel     dsbl4j        ta4NOT
138700010126     C  N04              write     tita4000
138800010126     C   04              write     tita4p00
138900010126    4C                   endif
139000010126    3C                   endif
139100010126    2C                   endsl
139200010126     C                   endif
139300950202     C* AGGIORNO SEMPRE  LE CONSEGNE PARTICOLARI SOLO SU  1 BOLLA
139400991018     c* idem per gg chiusura e data consegna richiesta
139500950202     C                   MOVEL     ARBTC1        TASFTC
139600950202     C                   MOVEL     ARBTC2        TASTC2
139700991018     c                   movel     arbtcr        tastcr
139800991018     c                   z-add     arbdcr        tasdcr
139900991018     c                   z-add     arbhcr        tashcr
140000991018     C                   MOVEL     arbgC1        tasgc1
140100991018     C                   MOVEL     arbgC2        tasgc2
140200990628     C   02              UPDATE    TiTAS000
140300990628     C   03              UPDATE    TiTAS010
140400990628     C   04              UPDATE    TiTASP00
140500950202     C                   ENDSR
140600950202     C*
140700920113     C**---------------------------------------------------------
140800920113     C** AGGIORNO CODICE VARIAZIONI TX
140900920113     C**---------------------------------------------------------
141000920113     C     AGARBT        BEGSR
141100920113     C*
141200920226     C                   EXSR      CTR2
141300920226     C*
141400941130    1C  N29              DO
141500950202     C*
141600991102     c
141700990419     c* controllo l'importo da assicurare se la spedizione e' tassata
141800990419     c*  o con tot.imponibile
141900011029     c                   if        tasias > 0  and tasvas = *blanks
142000011029     c                   movel     'ITL'         tasvas
142100011029     c                   endif
142200990419     c                   if        arbias <> tasias or
142300990419     c                             arbvas <> tasvas
142400990419     c*
142500990419     c                   exsr      AGGIAS
142600990419     c                   endif
142700950202     C*
142800950202     C                   Z-ADD     ARBIAS        TASIAS
142900950202     C                   MOVEL     ARBVAS        TASVAS
143000990824     c                   clear                   TASFCM
143100920113     C*
143200941130    2C     §3ATB2        IFEQ      '  '
143300970514     C     COMCVB        OREQ      'TP'
143400950222     C* AGGIORNO KSC SOLO SE VARIAZIONE DA ARRIVO E NON DA PARTENZA
143500950222     C*  PERCHE' SI TRATTA DI VARIAZIONE DI MITTENTE E NON DI TASSAZION
143600950222     C     ARBCVB        IFNE      'TP'
143700920113     C                   Z-ADD     ARBKSC        TASKSC
143800950222     C                   ENDIF
143900051219     c* Non vado sopra al codice tariffa se bolla in assegnato tassata con
144000051219     c* tariffa mittente
144100051219     c     tasftm        ifeq      *blanks
144200051219     C     comcvb        oreq      'TP'
144300950222     C                   MOVEL     ARBCTR        TASCTR
144400051219     c                   endif
144500950202     C                   Z-ADD     ARBQFT        TASQFT
144600991015     c                   MOVEL     ARBDIV        TASDIV
144700920113     C                   Z-ADD     ARBPOR        TASPOR
144800920113     C                   MOVEL     ARBSV1        TASSV1
144900920113     C                   Z-ADD     ARBVA1        TASVA1
145000920113     C                   MOVEL     ARBSV2        TASSV2
145100941130     C                   Z-ADD     ARBVA2        TASVA2
145200950202     C                   MOVEL     ARBSV3        TASSV3
145300950202     C                   Z-ADD     ARBVA3        TASVA3
145400941130     C*
145500990628     C* SE ESITONO GIA' DELLE VARIE IN TITA700F LE CANCELLO
145600990628     C     KBLP          SETLL     TITA7000
145700990628     C     KBLP          READE     TITA7000                               30
145800941130     C*
145900941130    3C     *IN30         DOWEQ     *OFF
146000991015     C                   DELETE    TITA7000
146100991015     C     KBLP          READE     TITA7000                               30
146200941130    3C                   ENDDO
146300991015     C     KARBU         SETLL     FIARBU1R
146400991015     C     KARBU         READE     FIARBU1R                               30
146500950202     C*
146600950202     C     *IN30         DOWEQ     *OFF
146700990628     C                   CLEAR                   TiTA7000
146800950202     C                   Z-ADD     ARBAAS        TA7AAS
146900950202     C                   Z-ADD     ARBLNP        TA7LNP
147000950202     C                   Z-ADD     ARBNRS        TA7NRS
147100950202     C                   Z-ADD     ARBNSP        TA7NSP
147200950202     C                   MOVEL     TASTBL        TA7TBL
147300950202     C                   MOVEL     ARBSVN        TA7SVN
147400950202     C                   Z-ADD     ARBVAN        TA7VAN
147500990629     C                   WRITE     TITA7000
147600950202     C*
147700950202     C                   ADD       1             NUMRBU            7 0
147800991015     C                   DELETE    FIARBURR
147900950202     C*
148000991015     C     KARBU         READE     FIARBU1R                               30
148100950202     C                   ENDDO
148200990628     c
148300941130     C*
148400920113     C                   Z-ADD     ARBDFT        TASDFT
148500920113     C                   Z-ADD     ARBNFT        TASNFT
148600920113     C                   Z-ADD     ARBFIV        TASFIV
148700941130     C                   MOVEL     ARBCEI        TASFEI
148800920113     C*
148900990624     C                   z-add     arbimv        TASimv
149000151120     c* Totale imponibile = 0 --> Cancello record FAT se presente e imposto fpf='R'
149100151120    3c                   if        tasimv=0
149200151120     c                   Eval      kar5trd='FAT'
149300151120     c     kar5          Chain     Fiar531c
149400151120     c                   if        %found(fiar531c)
149500151120     c   02              delete    fiar5000
149600151120     c                   endif
149700151118     c                   eval      tasfpf='R'
149800151120   x3c                   else
149900151120     c* Totale imponibile > 0 --> Scrivo record FAT se manca imposto in base a record FAT
150000151120     c*                           Imposto TASFPF in base al record FAT
150100151118     c                   clear                   dar5fat
150200151118     c                   Eval      kar5trd='FAT'
150300151118     c     kar5          Chain     Fiar531c
150400151120    4c                   If        %Found(Fiar531c)
150500151118     c                   Movel     ar5uni        dar5fat
150600151120   x4c                   else
150700151120     c                   Eval      Ar5Aas = ArbAas
150800151120     c                   Eval      Ar5Lnp = ArbLnp
150900151120     c                   Eval      Ar5Nrs = ArbNrs
151000151120     c                   Eval      Ar5Nsp = ArbNsp
151100151120     c                   Eval      Ar5Trd = 'FAT'
151200151120     c                   movel     tasvlf        §ar5vltas
151300151120     c                   movel     tasfvf        §ar5fvtas
151400151120     c                   movel     taspkf        §ar5pktas
151500151120     c                   movel     'R'           §ar5fptas
151600151120     c                   eval      §AR5DATAS=%dec(%date())
151700151120     c                   eval      §AR5HMTAS=%dec(%time())
151800151120     c                   eval      §AR5PRTAS=knmus
151900151120     c                   eval      §AR5NJTAS=knmtd
152000151120     c                   Movel(p)  dar5fat       Ar5uni
152100151120     c                   eval      ar5dac=%dec(%date())
152200151120     c                   eval      ar5orc=%dec(%time())
152300151120     c                   Eval      AR5FT1='T'
152400151120     c                   Eval      AR5DT1=99999999
152500151120     c                   Eval      AR5FT2='T'
152600151120     c                   Eval      AR5DT2=99999999
152700151120     c                   Eval      AR5FT3='T'
152800151120     c                   Eval      AR5DT3=99999999
152900151120     c   02              write     fiar5000
153000151120    4c                   endif
153100151120     c* Imposto TASFPF
153200151118     c                   select
153300151118     c                   when      §AR5FPTAS='R'
153400151118     c                   eval      tasfpf='R'
153500151118     c                   when      §AR5FPTAS='D'
153600151118     c                   eval      tasfpf='D'
153700151118     c                   other
153800151118     c                   eval      tasfpf='V'
153900151118     c                   endsl
154000151120    3c                   endif
154100941130    3C     ARBIFT        IFGT      0
154200941130     C                   MOVEL     '1'           TASFTS
154300920113     C                   ELSE
154400941130     C                   MOVEL     ' '           TASFTS
154500941130    3C                   END
154600941130    2C                   END
154700920113     C* AGGIORNO
154800990628     C   02              UPDATE    TiTAS000
154900941130    1C                   ENDDO
155000920113     C*
155100920113     C                   ENDSR
155200920113     C**---------------------------------------------------------
155300920113     C** AGGIORNO CODICE VARIAZIONI 2T
155400920113     C**---------------------------------------------------------
155500920113     C     AGAR2T        BEGSR
155600920113     C*
155700920226     C* SECONDA BOLLA
155800920113     C     §3ATB2        IFNE      '  '
155900920226     C                   EXSR      CTR2
156000920114     C*
156100920226     C  N29              DO
156200991015     C                   Z-ADD     ARBKSC        TASKSC
156300991015     C                   MOVEL     ARBCTR        TASCTR
156400991015     C                   movel     arbdiv        tasdiv
156500991015     C                   MOVEL     ARBSV1        TASSV1
156600991015     C                   Z-ADD     ARBVA1        TASVA1
156700991015     C                   MOVEL     ARBSV2        TASSV2
156800991015     C                   Z-ADD     ARBVA2        TASVA2
156900040114     c                   Movel     ArbSv3        TasSv3
157000040114     c                   Z-Add     ArbVa3        TasVa3
157100040114      * Se esistono già della varie su Tita7 le devo eliminare
157200040114     c     Kblp          Setll     Tita7000
157300040114     c                   Do        *Hival
157400040114     c     Kblp          Reade     Tita7000
157500040114     c                   If        %Eof
157600040114     c                   Leave
157700040114     c                   EndIf
157800040114     c                   Delete    Tita7000
157900040114     c                   EndDo
158000040114      * Scrivo il nuovo Tita7 e cancello il file di ricezione
158100040114     c     Karbu         Setll     Fiarbu1r
158200040114     c                   Do        *Hival
158300040114     c     Karbu         Reade     Fiarbu1r
158400040114     c                   If        %Eof(Fiarbu1r)
158500040114     c                   Leave
158600040114     c                   EndIf
158700040114     c                   Clear                   Tita7000
158800040114     c                   Z-add     ArbAas        Ta7Aas
158900040114     c                   Z-add     ArbLnp        Ta7Lnp
159000040114     c                   Z-add     ArbNrs        Ta7Nrs
159100040114     c                   Z-add     ArbNsp        Ta7Nsp
159200040114     c                   Movel     TasTbl        Ta7Tbl
159300040114     c                   Movel     ArbSvn        Ta7Svn
159400040114     c                   Z-add     ArbVan        Ta7Van
159500040114     c                   Write     Tita7000
159600040114     c                   Add       1             NumRbu
159700040114     c                   Delete    Fiarburr
159800040114     c                   EndDo
159900040114
160000991015     C                   Z-ADD     ARBDFT        TASDFT
160100991015     C                   Z-ADD     ARBNFT        TASNFT
160200991015     C                   Z-ADD     ARBFIV        TASFIV
160300991015     C                   MOVEL     ARBCEI        TASFEI
160400040114     C**!!!              z-add     arbva1        TASimv
160500040114     c                   Z-add     ArbImv        TasImv
160600991015     C     ARBIFT        IFGT      0
160700941130     C                   MOVEL     '1'           TASFTS
160800920114     C                   ELSE
160900941130     C                   MOVEL     ' '           TASFTS
161000920114     C                   END
161100920113     C*
161200040114      * Aggiorno Titas
161300990628     C   02              UPDATE    TiTAS000
161400920113     C                   END
161500920130     C                   END
161600920113     C*
161700920113     C                   ENDSR
161800920127     C**---------------------------------------------------------
161900920127     C** AGGIORNO CODICE VARIAZIONI KX
162000920127     C**---------------------------------------------------------
162100920127     C     AGARBK        BEGSR
162200121109     c                   if        *in11='0' and *in16='0'
162300121109     c                   exsr      memo_csb
162400121109     c                   endif
162500121205     c                   clear                   wanncas
162600920212     C                   SETOFF                                       31
162700941130     C                   CLEAR                   TNCSB000
162800950202     C* NON AGGIORNO SE HO LETTO
162900950202    1C     *IN11         IFEQ      *OFF
163000950202     C     *IN16         ANDEQ     *OFF
163100920127     C* VARIA CONTRASSEGNO
163200941130    2C     ARBCBP        IFEQ      ARBCBN
163300920127     C* VEDO CHE TIPO BOLLA AGGANCIARE DAL FLAG C/ASSEGNO
163400920131     C* RIAGGANCIO SOLO LA SECONDA PERCHE' LA PRIMA L'HO GIA' AGGANCIAT
163500941130    3C     §3AFCA        IFEQ      2
163600920127     C                   MOVEL     §3ATB2        COMTBL
163700941130     C                   SETOFF                                       020304
163800990625     C     KBLP          CHAIN     TITAS30C                           39
163900121108     c                   exsr      memo_dstitas
164000941130    3C                   END
164100920228     C*
164200920228     C* CONTROLLO SPEDIZIONE SE OK
164300990414     C                   EXSR      CTR3
164400920210     C*
164500920212     C* CONTROLLO ARCHIVIO C/ASSEGNO SE ERA PREVISTO
164600941130    3C     §3AFCA        IFNE      0
164700920212     C                   EXSR      CONCSB
164800941130    3C                   END
164900920210     C* SE NO ERRORE
165000950324    3C  N31CSBDDC        IFEQ      0
165100950324     C     CSBBNA        ANDEQ     0
165200941130     C                   MOVEL     *BLANKS       ARBDER
165300941130     C*
165400920228     C*** AGGIORNO CONTRASSEGNO
165500920228     C*** PRIMA     ESISTEVA ORA     ESISTE: V A R I A Z I O N E
165600941130    4C     CSBCAS        IFNE      0
165700950202     C     ARBCAS        ANDNE     0
165800950202     C* SCRIVO SOLO SE IMPORTO DIVERSO
165900950202    5C     ARBCAS        IFNE      CSBCAS
166000991103    5C     ARBVCA        ORNE      WCSBVCA
166100000703     c*
166200000703     c     csbsta        oreq      9
166300920212     C* SCRIVO STORICO
166400920212     C                   MOVEL     'VACA'        CSVCAV
166500920212     C                   EXSR      CSVWTR
166600950202     C*
166700950202     C                   EXSR      AGGCSB
166800950202     C*
166900941130     C                   UPDATE    TNCSB000
167000920228     C*
167100120420     C* E R R O R E : NON MODIFICABILE IMPORTO VARIA G
167200130325     C***29              MOVEL     ERR(22)       ARBDER
167300120322     C   29              exsr      ctr_vrb
167400941130    5C                   END
167500941130    4C                   END
167600920228     C***
167700920228     C*** PRIMA     ESISTEVA ORA NON ESISTE: A N N U L L O
167800941130    4C     CSBCAS        IFNE      0
167900950202     C     ARBCAS        ANDEQ     0
168000920212     C                   EXSR      ANNCSB
168100920226     C*
168200920228     C* E R R O R E : NON ELIMINABILE IMPORTO VARIA G
168300920316     C                   MOVEL     *BLANKS       ARBDER
168400990505    5C   29              DO
168500941130     C* CONTROLLO SE ESISTE VARIA "G"
168600130326     C****               EXSR      ESVARG
168700130325    6C***  WSIVAR        IFEQ      'S'
168800130325     C***                MOVEL     ERR(14)       ARBDER
168900130325    6C***                END
169000121205     c                   eval      wanncas='S'
169100121203     c                   exsr      ctr_vrb
169200941130    5C                   END
169300941130    4C                   END
169400920228     C***
169500920228     C*** PRIMA NON ESISTEVA ORA     ESISTE: C R E O  RECORD C/ASSEGNO
169600941130    4C     CSBCAS        IFEQ      0
169700950202     C     ARBCAS        ANDNE     0
169800920212     C                   MOVEL     §3ATB1        TBLCA
169900941130     C                   MOVEL     §3ATB1        FLGTB1
170000941130     C*
170100941130     C                   MOVEL     TASLOD        DESPAR
170200990625     C                   MOVEL     TASPRD        DESPRV
170300950913     C*
170400920212     C                   EXSR      CRTCSB
170500920228     C* E R R O R E : NON POSSO AGGIUNGERE IMPORTO VARIA G
170600130325     C***29              MOVEL     ERR(18)       ARBDER
170700130325     c***29              setoff                                       36
170800120322     C   29              exsr      ctr_vrb
170900941130    4C                   END
171000920228     C***
171100920228     C*** AGGIORNO SPEDIZIONE
171200941130     C                   MOVEL     'S'           TASFCA
171300920228     C* SE GIA' TASSATA METTO A POSTO PER VARIA G
171400110203     c                   if        *in38
171500110203     c                   Z-ADD     arbcas        wcas
171600110203     C                   EXSR      VARIAG
171700110203     c
171800110203      * riorganizzo le varie se è necessario
171900110203     c                   if        flgriorg = '1'
172000110203     c                   exsr      sr_riorg
172100110203     c                   endif
172200110203     c                   endif
172300110203
172400950202     C* SCRIVO ERRORE SE GIA' CONTABILIZZATA/STORICIZZATA
172500941130    4C     ARBDER        IFNE      *BLANKS
172600920309     C                   EXSR      WTRERR
172700941130    4C                   END
172800920228     C*
172900990625     C   02              UPDATE    TITAS000
173000990625     C   03              UPDATE    TITAS010
173100990625     C   04              UPDATE    TITASP00
173200941130    3C                   END
173300920127     C*
173400941130   X2C                   ELSE
173500920228     C******
173600920228     C****** M O D I F I C A T O   T I P O   B O L L A
173700920228     C******
173800920212     C                   SETOFF                                       31
173900920127     C                   MOVEL     §3ATB1        SAVTB1            2
174000920127     C                   MOVEL     §3ATB2        SAVTB2            2
174100920227     C                   MOVEL     §3AFCA        SAVFCA            1 0
174200920227     C                   MOVEL     §3AFTD        SAVFTD            1 0
174300941130    3C     §3ABCM        IFNE      ' '
174400920324     C                   MOVEL     0             SAVFCA
174500941130    3C                   END
174600920127     C* NUOVO TIPO BOLLA
174700920127     C                   MOVEL     '3A'          COD
174800941130     C                   MOVEL(P)  ARBCBN        KEY
174900920212     C     KTAB          CHAIN     TABEL                              34
175000941130     C*
175100941130     C* COD BOLLA NUOVO NON TROVATO
175200941130    3C     *IN34         IFEQ      *ON
175300941130     C                   MOVEL     ERR(6)        ARBDER
175400920212     C*
175500941130     C                   SETON                                        37
175600941130     C                   Z-ADD     102           COMFEV
175700941130     C                   EXSR      WTRERR
175800941130     C**
175900941130   X3C                   ELSE
176000941130     C**
176100941130     C* COD BOLLA NUOVO TROVATO
176200920131     C                   MOVEL     TBLUNI        DS3A
176300941130    4C     §3ABCM        IFNE      ' '
176400920324     C                   Z-ADD     0             §3AFCA
176500941130    4C                   END
176600920131     C*
176700920212     C* ANNULLAMENTO CONTRASSEGNO
176800941130    4C     SAVFCA        IFNE      0
176900920212     C     §3AFCA        ANDEQ     0
177000920212     C                   EXSR      CONCSB
177100920212     C*
177200941209    5C     *IN31         IFEQ      *OFF
177300941209     C* IMPOSTO IL NUOVO TIPO BOLLA
177400941209    6C     SAVFCA        IFEQ      2
177500941209     C     §3ATB2        ANDNE     *BLANKS
177600941209     C                   MOVEL     §3ATB2        CSBTBL
177700941209     C                   ELSE
177800941209     C                   MOVEL     §3ATB1        CSBTBL
177900941209    6C                   ENDIF
178000941209     C*
178100941209     C                   EXSR      ANNCSB
178200941209    5C                   END
178300941209    4C                   END
178400950202     C***************
178500920212     C* SE C/A PASSA A  "F"  A  "A"  O VICEVERSA POSSO FARLO ANCHE SE
178600920212     C*  INCASSATO - BASTA CHE NON SIA MODIFICATO L'IMPORTO
178700941130    4C     SAVFCA        IFEQ      1
178800920212     C     §3AFCA        ANDEQ     2
178900920212     C                   EXSR      CONCSB
179000950630     C* Se da bolla singola a bolla doppia e C/A sulla 2 bolla non
179100950630     C* aggiorno adesso TNCSB ma solo dopo scrittura della 2 bolla su
179200950630     C* TNTAS
179300950630    5C     SAVTB2        IFEQ      '  '
179400950630     C     §3ATB2        ANDNE     '  '
179500950630     C                   UNLOCK    TNCSB03L                             99
179600950630     C                   ELSE
179700941130     C* ANAGRAFICA C/A TROVATA
179800950630    6C     *IN31         IFEQ      *OFF
179900941130     C* CREO STORICO SE MODIFICATO IMPORTO
180000950630    7C     CSBCAS        IFNE      ARBCAS
180100991103    7C     WCSBVCA       ORNE      ARBVCA
180200920212     C                   MOVEL     'VACA'        CSVCAV
180300920212     C                   EXSR      CSVWTR
180400950630    7C                   END
180500920212     C*
180600941130     C                   Z-ADD     9999          CSBKSC
180700941130     C                   MOVEL     TASLNA        CSBKSC
180800950202     C                   MOVEL     §3ATB2        CSBTBL
180900950202     C*
181000950202     C                   EXSR      AGGCSB
181100941130     C                   UPDATE    TNCSB000
181200950630    6C                   END
181300950630    5C                   END
181400941130    4C                   END
181500950202     C***************
181600941130    4C     SAVFCA        IFEQ      2
181700920212     C     §3AFCA        ANDEQ     1
181800920228     C                   EXSR      CHTB2
181900920212     C*
182000920228     C                   EXSR      CONCSB
182100941130     C*
182200941130    5C     *IN31         IFEQ      *OFF
182300920212     C* CREO STORICO SE MODIFICATO IMPORTO
182400950202    6C     ARBCAS        IFNE      CSBCAS
182500991102    6C     ARBVCA        ORNE      WCSBVCA
182600920212     C                   MOVEL     'VACA'        CSVCAV
182700920212     C                   EXSR      CSVWTR
182800941130    6C                   END
182900920212     C*
183000941130     C                   MOVEL     CSBCDI        CSBKSC
183100941130     C                   MOVEL     §3ATB1        CSBTBL
183200950202     C                   EXSR      AGGCSB
183300941130     C                   UPDATE    TNCSB000
183400941130    5C                   END
183500941130    4C                   END
183600950202     C***************
183700950202     C* MODIFICATO IMPORTO C/ASSEGNO
183800941130    4C     SAVFCA        IFEQ      1
183900920227     C     §3AFCA        ANDEQ     1
184000941130     C* CONTROLLO IL C/A
184100920227     C                   EXSR      CONCSB
184200941130     C*
184300950202    5C  N31CSBCAS        IFNE      ARBCAS
184400991103    5C     WCSBVCA       ORNE      ARBVCA
184500920227     C                   MOVEL     'VACA'        CSVCAV
184600920227     C                   EXSR      CSVWTR
184700941209    5C                   END
184800950202     C*
184900950202     C                   EXSR      AGGCSB
185000950202     C*
185100941209     C* IMPOSTO IL NUOVO TIPO BOLLA
185200941209     C                   MOVEL     §3ATB1        CSBTBL
185300941209     C  N31              UPDATE    TNCSB000
185400941130    4C                   END
185500950202     C***************
185600941130    4C     SAVFCA        IFEQ      2
185700920227     C     §3AFCA        ANDEQ     2
185800920228     C                   EXSR      CHTB2
185900920227     C*
186000920227     C                   EXSR      CONCSB
186100941130     C*
186200950202    5C  N31CSBCAS        IFNE      ARBCAS
186300991103    5C     WCSBVCA       ORNE      ARBVCA
186400920227     C                   MOVEL     'VACA'        CSVCAV
186500920227     C                   EXSR      CSVWTR
186600941209    5C                   END
186700950202     C*
186800950202     C                   EXSR      AGGCSB
186900950202     C*
187000941209     C* IMPOSTO IL NUOVO TIPO BOLLA
187100941209     C                   MOVEL     §3ATB2        CSBTBL
187200941209     C  N31              UPDATE    TNCSB000
187300941130    4C                   END
187400920212     C***
187500920227     C*** A  G  G  I  O  R  N  O       B  O  L  L  A
187600920212     C***
187700080408     c  N31              if        VariataSede=' '
187800941130     C                   CLEAR                   DESPAR
187900970916     C                   CLEAR                   DESPRV
188000920228     C***
188100920228     C*** DA BOLLA SINGOLA A BOLLA SINGOLA
188200941130    5C     SAVTB2        IFEQ      '  '
188300920227     C     §3ATB2        ANDEQ     '  '
188400110315     c
188500110315     c* Non lo faccio se devo aggiungere solo il C/Ass perchè la bolla
188600110315     c*  non la trovo col cod bolla vecchio ma col cod bolla nuovo
188700110315    6c                   if        SoloCAss=' '
188800110315     c
188900941130     C                   MOVEL     'N'           VARFCA            1
189000920228     C                   EXSR      AGGTB1
189100941130     C*
189200941130     C* IMPOSTO IL PAESE DI ARRIVO
189300941130     C                   MOVEL     TASLOD        DESPAR
189400990625     C                   MOVEL     TASPRD        DESPRV
189500110315    6C                   END
189600110315    5C                   END
189700920228     C***
189800920228     C*** DA BOLLA DOPPIA A BOLLA DOPPIA
189900941130    5C     SAVTB2        IFNE      '  '
190000920227     C     §3ATB2        ANDNE     '  '
190100920228     C* AGGIORNO 1 BOLLA
190200941130     C                   MOVEL     'N'           VARFCA            1
190300920228     C                   EXSR      AGGTB1
190400941130     C* AGGANCIO 2 BOLLA
190500941130     C                   EXSR      CHTB2
190600941130     C* IMPOSTO IL PAESE DI ARRIVO
190700941130     C                   MOVEL     TASLOD        DESPAR
190800990625     C                   MOVEL     TASPRD        DESPRV
190900920227     C*
191000941130     C   02
191100941130    6CANN29              DO
191200920227     C*
191300941130    7C   38SAVFCA        IFEQ      2
191400920228     C     §3AFCA        OREQ      2
191500990505     c* Se il C/A si trova sulla 2 bolla, li imposto, altrimenti lo azzero
191600990505     c*  per il controllo della varia G
191700990505     c                   if        §3afca=2
191800990505     c                   eval      wcas=arbcas
191900990505     c                   else
192000990505     c                   clear                   wcas
192100990505     c                   endif
192200990505     c
192300920228     C                   EXSR      VARIAG
192400941130    7C                   END
192500920227     C*
192600920227     C                   MOVEL     §3ATB2        TASTBL
192700990629     C                   MOVEL     arbcbn        TASCBO
192800011218
192900011218      * riorganizzo le varie se è necessario
193000011218     c                   if        flgriorg = '1'
193100011218     c                   exsr      sr_riorg
193200011218     c                   endif
193300011218
193400920227     C*
193500941201     C* ALTRIMENTI CAMBIO IL TIPO BOLAA ANCHE NEL TA7
193600990625     C     KBLP          SETLL     TiTA7000
193700990625     C     KBLP          READE     TiTA7000                               30
193800941201    8C     *IN30         DOWEQ     *OFF
193900941201     C                   MOVEL     §3ATB2        TA7TBL
194000990625     C                   UPDATE    TITA7000
194100990625     C     KBLP          READE     TITA7000                               30
194200941201    8C                   ENDDO
194300941130    6C                   END
194400920227     C*
194500990625     C   02              UPDATE    TITAS000
194600990625     C   03              UPDATE    TITAS010
194700990625     C   04              UPDATE    TITASP00
194800920227     C*
194900920227     C* ERRORI
195000941130     C  N02
195100941130     COR 02
195200941130    6CAN 29              DO
195300941130    7C     SAVTB2        IFNE      §3ATB2
195400130410     c* solo se tipo bolla nuovo non è uno storno
195500130410     c                   movel     'TB'          cod
195600130410     c                   movel(p)  §3atb2        key
195700130410     c                   clear                   dstb
195800130410     C     KTAB          CHAIN     TABEL
195900130410    8c                   if        %found(tabel00f)
196000130410     c                   movel     tbluni        dstb
196100130410    8c                   endif
196200130410    8c                   if        §tbfcb='1'
196300130326     c                   z-add     46            comfev
196400930513     C                   MOVEL     ERR(16)       ARBDER                          MODIF BOLLA
196500130410    8c                   endif
196600941130   X7C                   ELSE
196700920228     C*
196800130326     c                   z-add     46            comfev
196900930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF A3
197000941130    7C                   END
197100920228     C*
197200920227     C                   EXSR      WTRERR
197300941130    6C                   END
197400941130    5C                   END
197500920228     C***
197600920228     C*** DA BOLLA DOPPIA A BOLLA SINGOLA
197700941130    5C     SAVTB2        IFNE      '  '
197800920227     C     §3ATB2        ANDEQ     '  '
197900950630     C*
198000920228     C* AGGANCIO 2 BOLLA
198100920228     C                   EXSR      CHTB2
198200941130     C* IMPOSTO IL PAESE DI ARRIVO
198300941130     C                   MOVEL     TASLOD        DESPAR
198400990625     C                   MOVEL     TASPRD        DESPRV
198500920227     C*
198600941130     C   02
198700941130    6CANN29              DO
198800920227     C                   MOVEL     'AP'          TASTBL
198900991021     c                   movel     arbcbn        tascbo
199000941130     C*
199100941130     C* TOLGO FLAG C/A SOLO SE DALLA 2 BOLLA E' PASSATO ALLA 1
199200941130     C     SAVFCA        IFEQ      2
199300941130     C     §3AFCA        ANDEQ     1
199400941130     C                   MOVEL     ' '           TASFCA
199500941130     C                   ENDIF
199600941130     C*
199700990625     C                   UPDATE    TITAS000
199800941130    6C                   END
199900920227     C*
200000920227     C* ERRORI
200100990505    6C   29              DO
200200130326     c                   z-add     46            comfev
200300930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF BOLLA
200400920227     C                   EXSR      WTRERR
200500941130    6C                   END
200600920227     C**
200700920228     C** AGGIORNO 1 BOLLA
200800941130     C                   MOVEL     'S'           VARFCA
200900920228     C                   EXSR      AGGTB1
201000941130    5C                   END
201100920228     C***
201200920228     C*** DA BOLLA SINGOLA A BOLLA DOPPIA
201300941130    5C     SAVTB2        IFEQ      '  '
201400920227     C     §3ATB2        ANDNE     '  '
201500950630     C*
201600920228     C* AGGIORNO PRIMA BOLLA
201700941130     C                   MOVEL     'S'           VARFCA
201800920228     C                   EXSR      AGGTB1
201900920227     C*
202000000816     c* Prima di creare la seconda bolla cancello eventualmente bolla tipo
202100000816     c* "AP" che potrei avere in seguito ad un precedente cambio da bolla
202200000816     c* doppia a bolla singola
202300000816     c                   movel     'AP'          comtbl
202400000816     c                   setoff                                       020304
202500000816     c     kblp          chain     titas30c                           39
202600000816     c     *in39         ifeq      *off
202700000816     c     *in02         andeq     *on
202800000816     c     tasdft        ifeq      *zeros
202900000816     c     tasfic        oreq      ' '
203000000816     c                   delete    titas000
203100000816     c                   endif
203200000816     c                   endif
203300920227     C* CREO SECONDA BOLLA
203400920227     C*
203500950201     C* AGGANCIO FILE FNARBD1R
203600920227     C     ARBORV        ADD       1             COMORV
203700950201     C     KARBD         CHAIN     FNARBDRR                           32
203800920304     C  N32              ADD       1             NUMRBD
203900920227     C* 32 ON - INESISTENTE: NON SCRIVO DESTINATARIO E SEGNALO ERRORE
204000941130    6C   32              DO
204100920227     C                   MOVEL     *BLANKS       ARBRSD
204200950202     C                   MOVEL     *BLANKS       ARBRD2
204300920227     C                   MOVEL     *BLANKS       ARBIND
204400920227     C                   MOVEL     *BLANKS       ARBLOD
204500950202     C                   MOVEL     *BLANKS       ARBCAD
204600941130     C                   MOVEL     *BLANKS       ARBPRD
204700950202     C                   MOVEL     *BLANKS       ARBNZD
204800920227     C*
204900920227     C                   MOVEL     ERR(12)       ARBDER
205000920227     C                   Z-ADD     102           COMFEV
205100920227     C                   EXSR      WTRERR
205200941130    6C                   END
205300920227     C*
205400920227     C*
205500941130     C                   MOVEL     §3ATB2        TASTBL
205600990629     C                   MOVEL     ARBCBN        TASCBO
205700941130     C                   MOVE      TASKSC        KSC4
205800941130     C*
205900941130     C                   CLEAR                   TASRSD
206000941130     C                   CLEAR                   TASIND
206100990625     C                   CLEAR                   TASCAd
206200941130     C                   CLEAR                   TASLOD
206300990625     C                   CLEAR                   TASPRd
206400990625     C                   CLEAR                   TAScpd
206500941130     C*
206600941130     C* MUOVO L'INIDIRIZZO COMPLETO DEL DESTINATARIO
206700941130     C                   MOVEL     ARBRSD        TASRSD
206800941130     C                   MOVEL     ARBIND        TASIND
206900990625     C                   MOVEL     ARBCAD        TASCAD
207000941130     C                   MOVEL     ARBLOD        TASLOD
207100941130     C                   MOVEL     ARBLOD        DESPAR
207200970916     C                   MOVEL     ARBPRD        DESPRV
207300990625     C                   MOVEL     ARBPRD        TASPRD
207400990625     C                   MOVEL     ARBNZD        TASNZD
207500990701     c* Muovo comunque la p.iva in tascpd anche se in questo caso
207600990701     c* sarà sempre vuota perchè visto che prima della variazione
207700990701     c* la bolla era franco non poteva esserci in filiale la p.iva
207800990701     c* del destinatario memoziazzata su fnarb
207900990701     C                   MOVEL     ARBCPI        TASCPD
208000941130     C*
208100941130     C                   CLEAR                   TASPVL
208200941130     C                   CLEAR                   TASFTC
208300950202     C                   CLEAR                   TASTC2
208400941130     C                   CLEAR                   TASIAS
208500990824     C                   CLEAR                   TASFCM
208600950202     C                   CLEAR                   TASVAS
208700941130     C                   MOVEL     *ZEROS        TASCTR
208800930604     C                   MOVEL     *BLANKS       TASFTM
208900991018     C                   MOVEL     *BLANKS       TASDIV
209000920227     C                   MOVE      *ZERO         TASPOR
209100920227     C                   MOVE      *BLANK        TASSV1
209200920227     C                   MOVE      *ZERO         TASVA1
209300920227     C                   MOVE      *BLANK        TASSV2
209400920227     C                   MOVE      *ZERO         TASVA2
209500930212     C                   MOVE      *BLANK        TASSV3
209600930212     C                   MOVE      *ZERO         TASVA3
209700920227     C                   Z-ADD     0             TASDFT
209800920227     C                   Z-ADD     0             TASNFT
209900920227     C                   Z-ADD     0             TASFIV
210000930212     C                   MOVE      ' '           TASFA2
210100920227     C                   Z-ADD     9999          TASKSC
210200920227     C                   MOVEL     TASLNA        TASKSC
210300920227     C* CONTRASSEGNO A SECONDA DEL FLAG
210400941130    6C     §3AFCA        IFEQ      2
210500941130     C                   MOVEL     'S'           TASFCA
210600920227     C                   MOVEL     §3ATB2        TBLCA             2
210700920227     C                   ELSE
210800941130     C                   MOVEL     ' '           TASFCA
210900941130    6C                   END
211000920227     C*
211100990624     C                   MOVE      *zeros        TASimv
211200941130     C                   MOVEL     ' '           TASFEI
211300941130     C                   MOVEL     ' '           TASFTS
211400941130     C                   MOVEL     ' '           TASFIC
211500920227     C*
211600990625     C                   WRITE     TITAS000                             99
211700950630     C*
211800950630     C     SAVFCA        IFEQ      1
211900950630     C     §3AFCA        ANDEQ     2
212000950630     C     KANN          CHAIN     TNCSB000                           31
212100991103     c                   move      csbvca        wcsbvca
212200991103     c     csbcas        ifgt      0
212300991103     c     wcsbvca       andeq     *blanks
212400991103     c                   movel     'ITL'         wcsbvca
212500991103     c                   endif
212600950630     C     CSBCAS        IFNE      ARBCAS
212700991103     C     wcsbvca       ORNE      ARBVCA
212800950630     C                   MOVEL     'VACA'        CSVCAV
212900950630     C                   EXSR      CSVWTR
213000950630     C                   END
213100950630     C*
213200950630     C                   Z-ADD     9999          CSBKSC
213300950630     C                   MOVEL     TASLNA        CSBKSC
213400950630     C                   MOVEL     §3ATB2        CSBTBL
213500950630     C*
213600950630     C                   EXSR      AGGCSB
213700950630     C                   UPDATE    TNCSB000
213800950630     C                   END
213900920227     C*
214000941130     C* REIMPOSTO ORA DI VARIAZIONE DI ARBK: ORA DI ARBD - 1
214100920227     C  N32              SUB       1             ARBORV
214200950201     C  N32              DELETE    FNARBDRR
214300941130    5C                   END
214400920227     C*
214500920227     C* CREO SE NECESSARIO RECORD IN FILE CONTRASSEGNI
214600941130    5C     SAVFCA        IFEQ      0
214700941130    6C     §3AFCA        IFEQ      1
214800920227     C                   MOVEL     §3ATB1        TBLCA
214900941130     C* RICHAINO LA BOLLA
215000941130     C                   MOVEL     §3ATB1        COMTBL
215100941130     C                   SETOFF                                       020304
215200990625     C     KBLP          CHAIN     TITAS30C                           30
215300110315     c* Se solo C/Ass--> aggiungo il flag
215400110315     c                   if        SoloCAss='S'
215500110315     c                   eval      TASFCA='S'
215600110315     C                   MOVEL     TASLOD        DESPAR
215700110315     C                   MOVEL     TASPRD        DESPRV
215800110315     C   02              UPDATE    TITAS000
215900110315     C   03              UPDATE    TITAS010
216000110315     C   04              UPDATE    TITASP00
216100110315     c                   endif
216200950913     C*
216300920227     C                   EXSR      CRTCSB
216400941130     C*
216500920227     C                   ELSE
216600941130     C*
216700941130    7C     §3AFCA        IFEQ      2
216800920227     C                   MOVEL     §3ATB2        TBLCA
216900941130     C* RICHAINO LA BOLLA
217000941130     C                   MOVEL     §3ATB2        COMTBL
217100941130     C                   SETOFF                                       020304
217200990625     C     KBLP          CHAIN     TITAS30C                           30
217300920227     C                   EXSR      CRTCSB
217400941130    7C                   END
217500941130    6C                   END
217600941130    5C                   END
217700920227     C*
217800941130    4C                   END
217900941130    3C                   END
218000941130    2C                   END
218100930513     C*
218200930513     C* 11 ON - RECORD DESTINATARIO PER CREARE 2 BOLLA
218300941130   X1C                   ELSE
218400950202     C* NON CANCELLO IL RECORD : IN ARBM LO LASCIO LI'
218500950202     C                   SETON                                        37
218600950202     C*
218700950202     C     *IN11         IFEQ      *ON
218800930513     C                   MOVEL     102           COMFEV
218900930513     C                   MOVEL     ERR(15)       ARBDER
219000930513     C                   EXSR      WTRERR
219100950202     C                   ENDIF
219200950202    1C                   ENDIF
219300920227     C*
219400920227     C                   ENDSR
219500060317     C**---------------------------------------------------------
219600060317     C** AGGIORNO CODICE VARIAZIONE RM
219700060317     C**---------------------------------------------------------
219800060317     C     AGARBR        BEGSR
219900060317     c* rma in tita4 record "A"
220000060317     c                   setoff                                       0506
220100060317     C                   movel     'A'           §trc
220200060317     C     ktita         chain     tita430c                           32
220300060317    3C     *in32         ifeq      *off
220400060317     C                   movel     ta4not        dta4a
220500060317     c                   movel     arbrsd        §TA4ARMA
220600160121     C                   MOVEL     ARBrd2        §TA4anas
220700060317     C                   movel     dta4a         ta4not
220800060317     C   05              update    tita4000
220900060317     C   06              update    tita4p00
221000060317     c                   endif
221100060317     c*
221200160412     C****               MOVEL     ARBrd2        TASnas
221300160412     C** 02              UPDATE    TiTAS000
221400160412     C** 03              UPDATE    TiTAS010
221500160412     C** 04              UPDATE    TiTASP00
221600160412     c**
221700160412     c                   unlock    titas30c
221800060317     c*
221900060317     C                   ENDSR
222000920228     C**---------------------------------------------------------
222100920228     C** AGGIORNO PRIMA BOLLA
222200920228     C**---------------------------------------------------------
222300920228     C     AGGTB1        BEGSR
222400920228     C* AGGANCIO 1 BOLLA
222500920228     C                   EXSR      CHTB1
222600920228     C*
222700941201     C* SE E' DA AGGIUNGERE LO AGGIUNGO
222800920228     C     §3AFCA        IFEQ      1
222900941130     C                   MOVEL     'S'           TASFCA
223000941201     C*
223100920228     C                   ELSE
223200941201     C* LO TOLGO SOLO SE FALG IMPOSTATO = "S"
223300941201     C*
223400941201     C     VARFCA        IFEQ      'S'
223500941130     C                   MOVEL     ' '           TASFCA
223600920228     C                   END
223700941130     C                   END
223800920228     C*
223900941130     C   02
224000941130     CANN29              DO
224100920228     C   38SAVFCA        IFEQ      1
224200920228     C     §3AFCA        OREQ      1
224300990505     c* Devo impostare l'importo C/A nella routine varia G :
224400990505     c*  se il C/A e' stato tolto dalla 1 bolla, richiamo con importo=0
224500990505     c                   if        §3afca=1
224600990505     c                   eval      wcas=arbcas
224700990505     c                   else
224800990505     c                   clear                   wcas
224900990505     c                   endif
225000990505     c
225100920228     C                   EXSR      VARIAG
225200920228     C                   END
225300920228     C*
225400920228     C                   MOVEL     §3ATB1        TASTBL
225500990629     C                   MOVEL     ARBCBN        TASCBO
225600011219
225700011219      * riorganizzo le varie se è necessario
225800011219     c                   if        flgriorg = '1'
225900011219     c                   exsr      sr_riorg
226000011219     c                   endif
226100011219
226200000209     C* AGGIORNO TITA7000 SE ESISTE
226300000209     C     KBLP          SETLL     TITA7000
226400000209     C     KBLP          READE     TITA7000                               30
226500000209     C*
226600000209     C     *IN30         DOWEQ     *OFF
226700000209     C                   MOVEL     §3ATB1        TA7TBL
226800000209     C                   UPDATE    TITA7000
226900000209     C     KBLP          READE     TITA7000                               30
227000000209     C                   ENDDO
227100920228     C                   END
227200941130     C*
227300920228     C*
227400990625     C   02              UPDATE    TITAS000
227500990625     C   03              UPDATE    TITAS010
227600990625     C   04              UPDATE    TITASP00
227700920228     C*
227800990505     C* spedizione gia' contabilizzata
227900990505     C   29              DO
228000920228     C*
228100920312     C                   MOVEL     *BLANKS       ARBDER
228200920228     C     §3ATB1        IFNE      SAVTB1
228300130410     c* solo se tipo bolla nuovo non è uno storno
228400130410     c                   movel     'TB'          cod
228500130410     c                   movel(p)  §3atb1        key
228600130410     c                   clear                   dstb
228700130410     C     KTAB          CHAIN     TABEL
228800130410    3c                   if        %found(tabel00f)
228900130410     c                   movel     tbluni        dstb
229000130410    3c                   endif
229100130410    3c                   if        §tbfcb='1'
229200130326     c                   z-add     46            comfev
229300930513     C                   MOVEL     ERR(16)       ARBDER                         MODIF BOLLA
229400130410    3c                   endif
229500920228     C                   ELSE
229600920228     C*
229700920228     C     SAVFCA        IFNE      1
229800920228     C     §3AFCA        ANDEQ     1
229900130325     C***                MOVEL     ERR(18)       ARBDER                         AGGIUN VARIA
230000130325     c***                setoff                                       36
230100120423     c                   exsr      ctr_vrb
230200920228     C                   END
230300920228     C*
230400920228     C     SAVFCA        IFEQ      1
230500920228     C     §3AFCA        IFNE      1
230600941130     C*
230700941130     C* CONTROLLO SE ESISTE VARIA "G"
230800130326     C***                EXSR      ESVARG
230900130325     C***  WSIVAR        IFEQ      'S'
231000130325     C***                MOVEL     ERR(14)       ARBDER                         TOLTA VARIA
231100130325     c***                setoff                                       36
231200130325     C***                END
231300121205     c                   eval      wanncas='S'
231400121203     c                   exsr      ctr_vrb
231500941130     C*
231600920228     C                   ELSE
231700130325     C****               MOVEL     ERR(22)       ARBDER                         MODIF VARIA
231800130325     c****               setoff                                       36
231900120423     c                   exsr      ctr_vrb
232000920228     C                   END
232100920228     C                   END
232200920228     C                   END
232300920228     C*
232400920312     C     ARBDER        IFNE      *BLANKS
232500920228     C                   EXSR      WTRERR
232600920228     C                   END
232700920312     C                   END
232800920228     C                   ENDSR
232900920227     C**---------------------------------------------------------
233000920227     C** CALCOLO O ANNULLO VARIA  G
233100120322     C**---------------------------------------------------------
233200920227     C     VARIAG        BEGSR
233300920228     C                   MOVE      TASKSC        KSC4
233400990505     c                   clear                   ARBDER
233500011218
233600011218     c                   clear                   flgriorg
233700941130     C*
233800990414     c* Se data fattura a zero e cliente codificato vado a togliere la varia G
233900990505     C     TASDFT        IFEQ      *ZEROS
234000990414    1C     KSC4          ANDNE     9999
234100920303     C     KSC4          ANDNE     8888
234200920228     C*
234300990505     c* C/A tolto
234400990505     c                   if        wcas=0
234500941130     C                   SELECT
234600941130     C     TASSV1        WHENEQ    'G'
234700011218     c                   eval      flgriorg = '1'
234800991019     c     tasimv        ifeq      tasva1
234900991019     c                   clear                   tasdiv
235000991019     c                   endif
235100920227     C                   MOVEL     ' '           TASSV1
235200920227     C                   Z-ADD     0             TASVA1
235300990624     C                   MOVE      *zeros        TASimv
235400941130     C     TASSV2        WHENEQ    'G'
235500011218     c                   eval      flgriorg = '1'
235600991019     c     tasimv        ifeq      tasva2
235700991019     c                   clear                   tasdiv
235800991019     c                   endif
235900920227     C                   MOVEL     ' '           TASSV2
236000920227     C                   Z-ADD     0             TASVA2
236100990624     C                   MOVE      *zeros        TASimv
236200941130     C     TASSV3        WHENEQ    'G'
236300011218     c                   eval      flgriorg = '1'
236400991019     c     tasimv        ifeq      tasva3
236500991019     c                   clear                   tasdiv
236600991019     c                   endif
236700930212     C                   MOVEL     ' '           TASSV3
236800930212     C                   Z-ADD     0             TASVA3
236900990624     C                   MOVE      *zeros        TASimv
237000941130     C*
237100941130     C                   OTHER
237200941130     C*
237300941130     C                   MOVEL     'G'           WSV1
237400990625     C     KBLPV         DELETE    TITA7000                           30
237500990624     C  n30              MOVE      *zeros        TASimv
237600941130     C                   ENDSL
237700990419     c                   else
237800990419     c* C/A aggiunto o midificato
237900990419     c                   EXSR      ESVARG
238000990419     c* Se la varia non c'e' tolto totale imponibile cosi' la calcola
238100990419     c                   if        wsivar=' '
238200990624     c                   clear                   TASimv
238300990419     c                   else
238400990419     c* Se gia' esiste stampo errore --> no modificata varia G
238500130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
238600130326     C***                EXSR      WTRERR
238700130326     c***                eval      wcntab=' '
238800130326     c***                exsr      ctr_vrb
238900990419     c                   endif
239000990419     c                   endif
239100920228     C*
239200941130   X1C                   ELSE
239300990414     C* CLIENTE NON CODIFICATO O CON DATA FATTURA VALORIZZATA
239400991019     C* COMUNQUE SE ANNULLATO C/A ELIMINO VARIA G  E ADEGUO TOT.IMPONIBILE
239500990414     C* Se c'è importo del contrassegno e la data fattura è uguale a zero posso
239600990414     c* togliere la varia "G"
239700990505    2C     WCAS          IFEQ      0
239800990505     C     TASDFT        ANDEQ     0
239900990414     c*
240000941130    3C                   SELECT
240100941130     C     TASSV1        WHENEQ    'G'
240200011218     c                   eval      flgriorg = '1'
240300991019     c                   sub       tasva1        tasimv
240400991019     c     tasimv        ifeq      0
240500991019     c                   clear                   tasdiv
240600991019     c                   endif
240700920228     C                   MOVEL     ' '           TASSV1
240800920228     C                   Z-ADD     0             TASVA1
240900941130     C     TASSV2        WHENEQ    'G'
241000011218     c                   eval      flgriorg = '1'
241100991019     c                   sub       tasva2        tasimv
241200991019     c     tasimv        ifeq      0
241300991019     c                   clear                   tasdiv
241400991019     c                   endif
241500920228     C                   MOVEL     ' '           TASSV2
241600920228     C                   Z-ADD     0             TASVA2
241700941130     C     TASSV3        WHENEQ    'G'
241800011218     c                   eval      flgriorg = '1'
241900991019     c                   sub       tasva3        tasimv
242000991019     c     tasimv        ifeq      0
242100991019     c                   clear                   tasdiv
242200991019     c                   endif
242300930212     C                   MOVEL     ' '           TASSV3
242400930212     C                   Z-ADD     0             TASVA3
242500941130     C                   OTHER
242600941130     C                   MOVEL     'G'           WSV1
242700991019     C     KBLPV         chain     TITA7000                           30
242800991019     c  n30              sub       ta7van        tasimv
242900991019     c  n30              delete    tita7000
243000941130    3C                   ENDSL
243100920228     C*
243200941130   X2C                   ELSE
243300990414     c*
243400990414     c* Se è già stato fatturato oppure arbcas > 0
243500920228     C*
243600941130     C* VEDO SE LA VARIA "G" C'ERA
243700130326     C*****              EXSR      ESVARG
243800941130     C*
243900991019     c* se c'è  l'importo del contrassegno per non codificati
244000991019     c*                                      o data fattura maggiore di zero
244100130326    3c***  wcas          ifgt      *zeros
244200990414     c*
244300130326    4C***  WSIVAR        IFEQ      ' '
244400130326     C***                MOVEL     ERR(24)       ARBDER                         AGGIUN VARIA
244500130326     C***                EXSR      WTRERR
244600130326     c***                eval      wcntab=' '
244700130326     c***                exsr      ctr_vrb
244800130326     C***                ELSE
244900130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
245000130326     C***                EXSR      WTRERR
245100130326     c***                eval      wcntab=' '
245200130326     c***                exsr      ctr_vrb
245300130326    4C***                END
245400130326    3c****               endif
245500990505     c*
245600990414     c* se l'importo è uguale a zero e prima c'era la varia "G"
245700130326    3c***  wcas          ifeq      *zeros
245800130326     c***  wsivar        andne     ' '
245900130326     C***                MOVEL     ERR(23)       ARBDER                          no tolta varia
246000130326     C***                EXSR      WTRERR
246100130326    3C***                END
246200941130    2C                   END
246300941130    1C                   END
246400920227     C*
246500920227     C                   ENDSR
246600011218      *----------------------------------------------------------
246700011218      * Riorganizzo le varie sul titas
246800011218      *----------------------------------------------------------
246900011218     c     sr_riorg      begsr
247000011218
247100011218     c                   clear                   sksv
247200011218     c                   clear                   skva
247300011218     c                   eval      xx = 1
247400011218
247500011219      * carico la sk delle varie dal titas
247600011218     c                   if        tassv1 <> *blanks
247700011218     c                   eval      sksv(xx) = tassv1
247800011218     c                   eval      skva(xx) = tasva1
247900020107     c                   clear                   tassv1
248000020107     c                   clear                   tasva1
248100011218     c                   add       1             xx
248200011218     c                   endif
248300011218     c                   if        tassv2 <> *blanks
248400011218     c                   eval      sksv(xx) = tassv2
248500011218     c                   eval      skva(xx) = tasva2
248600020107     c                   clear                   tassv2
248700020107     c                   clear                   tasva2
248800011218     c                   add       1             xx
248900011218     c                   endif
249000011218     c                   if        tassv3 <> *blanks
249100011218     c                   eval      sksv(xx) = tassv3
249200011218     c                   eval      skva(xx) = tasva3
249300020107     c                   clear                   tassv3
249400020107     c                   clear                   tasva3
249500011218     c                   add       1             xx
249600011218     c                   endif
249700011218
249800011219      * carico la sk delle varie dal tita7
249900011219     c     kblp          setll     tita7000
250000011218     c                   do        *hival
250100011219     c     kblp          reade     tita7000
250200011219     c                   if        %eof
250300011218     c                   leave
250400011219     c                   endif
250500011218     c                   eval      skva(xx) = ta7van
250600011218     c                   eval      sksv(xx) = ta7svn
250700011218     c                   add       1             xx
250800011219     c                   delete    tita7000
250900011218     c                   enddo
251000011219
251100011219      * scrivo le varie su titas
251200011219     c                   if        sksv(1) <> *blanks
251300011219     c                   eval      tassv1 = sksv(1)
251400011219     c                   eval      tasva1 = skva(1)
251500011219     c                   endif
251600011219     c                   if        sksv(2) <> *blanks
251700011219     c                   eval      tassv2 = sksv(2)
251800011219     c                   eval      tasva2 = skva(2)
251900011219     c                   endif
252000011219     c                   if        sksv(3) <> *blanks
252100011219     c                   eval      tassv3 = sksv(3)
252200011219     c                   eval      tasva3 = skva(3)
252300011219     c                   endif
252400011219
252500011219      * scrivo le varie su tita7
252600011219     c     4             do        20            xx
252700011219     c                   if        sksv(xx) <> *blanks
252800011219     c                   eval      ta7aas = arbaas
252900011219     c                   eval      ta7lnp = arblnp
253000011219     c                   eval      ta7nrs = arbnrs
253100011219     c                   eval      ta7nsp = arbnsp
253200011219     c                   eval      ta7tbl = comtbl
253300011219     c                   eval      ta7svn = sksv(xx)
253400011219     c                   eval      ta7van = skva(xx)
253500011219     c                   write     tita7000
253600011219     c                   endif
253700011219     c                   enddo
253800011218
253900011218     c                   endsr
254000011218
254100920210     C**---------------------------------------------------------
254200920210     C** CREO RECORD CONTRASSEGNO
254300920210     C**---------------------------------------------------------
254400920210     C     CRTCSB        BEGSR
254500941130     C                   MOVEL     ARBDER        SAVDER
254600920303     C*
254700920303     C* SE ESISTE ANNULLATO LO DELETO E RISCRIVO
254800941130     C     KANN          CHAIN     TNCSB000                           41
254900941130    1C     *IN41         IFEQ      *ON
255000941130     C                   CLEAR                   TNCSB000
255100941130     C                   ELSE
255200941130     C     KANN          CHAIN     TNCSA000                           39
255300941130     C  N39              DELETE    TNCSA000
255400941130    1C                   ENDIF
255500920303     C*
255600950201     C* AGGANCIO FILE FNARBM0R
255700950202     C     KARBM         CHAIN     FNARBMRR                           35
255800920304     C  N35              ADD       1             NUMRBM            7 0
255900920212     C* INESISTENTE: STAMPO MA CREO UGUALMENTE ARCHIVIO (DA COMPLETARE)
256000941130    1C   35              DO
256100920212     C                   MOVEL     ERR(11)       ARBDER
256200920212     C                   Z-ADD     102           COMFEV
256300920212     C                   EXSR      WTRERR
256400920212     C* AZZERO CAMPI MITTENTE
256500920212     C                   MOVEL     *BLANKS       ARBRSM
256600920212     C                   MOVEL     *BLANKS       ARBINM
256700920212     C                   MOVEL     *BLANKS       ARBCAM
256800920212     C                   MOVEL     *BLANKS       ARBLOM
256900941130     C                   MOVEL     *BLANKS       ARBPRD
257000920212     C                   MOVEL     *ZEROS        ARBCCM
257100941130    1C                   END
257200920212     C*
257300920210     C                   MOVEL     TASLNP        CSALNP
257400920210     C                   MOVEL     TASNRS        CSANRS
257500920210     C                   MOVEL     TASAAS        CSAAAS
257600920210     C                   MOVEL     TASNSP        CSANSP
257700920210     C*
257800920210     C                   MOVEL     TBLCA         CSBTBL
257900920210     C                   Z-ADD     TASLNP        CSBLNP
258000920210     C                   Z-ADD     TASAAS        CSBAAS
258100920210     C                   Z-ADD     TASNRS        CSBNRS
258200920210     C                   Z-ADD     TASNSP        CSBNSP
258300941130     C                   Z-ADD     TASMGS        CSBMGS
258400990629     C                   Z-ADD     TASTFP        CSBFLE
258500941130     C                   MOVEL     DESPAR        CSBPAR                         PAESE ARRIVO
258600970916     C                   MOVEL     DESPRV        CSBPRO                         PROV.DEST.
258700030310     C                   MOVE      *ZERO         CSBSTA
258800950202     C                   EXSR      AGGCSB
258900920304     C*
259000941130     C                   MOVEL     TBLCA         FLGTB1
259100920304     C*
259200920304     C* SE BLANK E CODIFICATO DESTINATARIO CHIAN
259300941130    2C     TASRSD        IFEQ      *BLANKS
259400990625     c* N.B.la call al tibs69 sarà da togliere quando tutte le bolla avranno
259500990625     c* il destinatario pieno
259600990625     c     flgtb1        ifeq      'A'
259700981001     C                   CLEAR                   DSBS69
259800981001     C                   MOVEL     KCI           I69KCC
259900981001     C                   MOVEL     KNSIF         I69SIF
260000981001     C                   MOVEL     TASKSC        I69KAC
260100981001     C                   CALL      'TIBS69R'
260200981001     C                   PARM                    DSBS69
260300981001     C                   PARM                    DSACO
260400981001     C                   PARM                    DSIND
260500981001     C                   PARM                    DSCLP
260600981001     C                   PARM                    DSCLS
260700981001     C**
260800981001     C     O69ERR        IFEQ      ' '
260900981001     C                   MOVEL     ACORAG        CSBRSD
261000981001     C                   ELSE
261100981001     C                   MOVEL     *BLANKS       CSBRSD
261200981001     C                   ENDIF
261300990625     C                   ENDIF
261400981001     C                   ELSE
261500981001     C                   MOVEL     TASRSD        CSBRSD
261600941130    2C                   END
261700920304     C*
261800920304     C*
261900920210     C                   Z-ADD     TASRMN        CSBRMN
262000990625     c* prendo il riferimento mittente alfabetico da estensione (tita4)
262100990625     c                   movel     'A'           §trc
262200010126     c     ktita         chain(N)  tita430c                           39
262300990625     c  n39              movel     ta4not        csbrma
262400990625     c   39              clear                   csbrma
262500941207     C                   Z-ADD     TASLNA        CSBLNA
262600040721     C**                   MOVEL     TASFIN        CSBFIN
262700920210     C                   Z-ADD     TASKSC        CSBKSC
262800920210     C                   MOVE      CSBKSC        CSBCDI
262900931108     C   41              MOVEL     'R'           CSBFTR
263000931108     C   41              MOVE      DATEU         CSBDTR
263100920303     C*
263200941130    1C     FLGTB1        IFEQ      'A'
263300941130     C                   Z-ADD     99999         CSBKSC
263400941130     C                   MOVEL     TASLNA        CSBKSC
263500941206     C                   MOVE      CSBKSC        CSBCDI                         WB/LB
263600941206    1C                   END                                                    WB/LB
263700941130     C*
263800941130     C                   MOVEL     TASCCM        CSBCDI
263900941206    1C*************        END                             WB/LB
264000941130     C*
264100920210     C** RAGGRUPPAMENTO C/ASSEGNI
264200920210     C     TASLNA        CHAIN     AZORG                              51
264300920210     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
264400920210     C   51              Z-ADD     TASLNA        CSBRGF
264500941130     C*
264600941130     C* VEDO SE CSBCDI E' INTESTAZIONE GENERICA
264700941130     C                   MOVE      CSBCDI        W0040             4 0
264800920210     C*
264900920210     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
265000941130     C     W0040         IFEQ      8888
265100941130     C     W0040         OREQ      9999
265200920212     C                   MOVEL     ARBRSM        CSARSD                         MITTENTE
265300920212     C                   MOVEL     ARBINM        CSAVID                            '
265400920212     C                   MOVEL     ARBCAM        CSACAD                            '
265500920212     C                   MOVEL     ARBLOM        CSACID                            '
265600970916     C                   MOVEL     ARBPRM        CSAPRM
265700970916     C                   MOVEL     ARBNZM        CSANZM
265800920212     C*
265900941201     C                   WRITE     TNCSA000                             99
266000920212     C                   END
266100920212     C**
266200941201     C   41              WRITE     TNCSB000                             99
266300941130     C  N41              UPDATE    TNCSB000
266400950914     C*
266500950914     C* CREO RECORD DI INSERIMENTO C/ASSEGNO NELLE VARIAZIONI
266600950914     C                   MOVEL     'IMCA'        CSVCAV
266700950914     C                   EXSR      CSVWTR
266800941130     C*
266900941130     C** SE ERA IMPOSTATO LO RIMETTO COME ERA PRIMA
267000941130     C                   MOVEL     SAVDER        ARBDER
267100920210     C                   ENDSR
267200920210     C**---------------------------------------------------------
267300920210     C** CONTROLLO ESISTENZA ARCHIVIO CONTRASSEGNI
267400920210     C**---------------------------------------------------------
267500920210     C     CONCSB        BEGSR
267600930513     C                   SETOFF                                           42    25
267700920212     C* VARIO  CONTRASSEGNO SOLO SE NON PAGATO-INCASSATO
267800941130     C     KANN          CHAIN     TNCSB000                           31
267900930303     C*
268000941130     C   31KANN          SETLL     FNLBL01L                               42
268100920212     C*
268200930513     C* PER CASUALE VARIAZIONE "K" SEMPRE ERRORE
268300930303     C   31FLGCVB        IFEQ      'K'
268400930513     C                   SETON                                        25
268500930303     C                   MOVEL     ERR(5)        ARBDER
268600930303     C                   Z-ADD     102           COMFEV
268700930303     C                   EXSR      WTRERR
268800930513     C*
268900930303     C                   ELSE
269000930513     C*
269100930513     C* ALTRIMENTI SOLO SE NON C'E' IL LEGAME BOLLA
269200930513     C     *IN42         IFEQ      '0'
269300930513     C                   SETON                                        25
269400930513     C                   MOVEL     ERR(5)        ARBDER
269500930513     C                   Z-ADD     102           COMFEV
269600930513     C                   EXSR      WTRERR
269700930303     C                   END
269800930513     C                   END
269900991103     c*
270000991103     c     *in31         ifeq      *off
270100991103     c                   movel     csbvca        wcsbvca
270200991103     c     csbcas        ifgt      0
270300991103     c     csbvca        andeq     *blanks
270400991103     c                   movel     'ITL'         wcsbvca
270500991103     c                   endif
270600991103     c                   endif
270700930513     C*
270800920302     C* GIA' INCASSATO O PAGATO
270900950202    1C  N31CSBDDC        IFNE      0
271000920210     C     CSBBNA        ORNE      0
271100920212     C*
271200950324     C* SE NON MODIFICATO L'IMPORTO DEL C/A OK, SE VARIAZIONE "K"
271300950324     C     FLGCVB        IFEQ      'K'
271400950202    3C     ARBCAS        IFNE      CSBCAS
271500991102     C     ARBVCA        ORNE      WCSBVCA
271600920212     C                   SETON                                        31
271700110214     C                   Z-ADD     102           COMFEV
271800950202    2C                   END
271900110214     c
272000950324     C* ALTRIMENTI SEMPRE ERRORE
272100950324     C                   ELSE
272200950324     C                   SETON                                        31
272300110214     C                   Z-ADD     102           COMFEV
272400950324    2C                   END
272500920601     C*
272600950202    2C   31FLGCVB        IFNE      'I'
272700950222    2C     FLGCVB        ANDNE     'M'
272800920601     C                   SETON                                        36
272900920601     C                   MOVEL     ERR(10)       ARBDER
273000920601     C                   EXSR      WTRERR
273100950324     C                   ELSE
273200950324     C                   CLEAR                   COMFEV
273300950202    2C                   END
273400950202    1C                   END
273500920210     C                   ENDSR
273600950202     C*
273700950202     C**---------------------------------------------------------
273800950202     C** AGGIORNO RECORD CONTRASSEGNO
273900950202     C**---------------------------------------------------------
274000950202     C     AGGCSB        BEGSR
274100950202     C                   Z-ADD     ARBCAS        CSBCAS
274200950202     C                   MOVEL     ARBVCA        CSBVCA
274300950202     C                   MOVEL     ARBGCA        CSBGCA
274400950203     C                   Z-ADD     ARBCMB        CSBCMB
274500030310     c                   if        csbsta <> 3
274600000703     c                   movel     0             csbsta
274700030310     c                   end
274800950202     C* TIPO ASSEGNO: MITTENTE / BARTOLINI
274900950202     C                   MOVEL     '1A'          COD
275000950202     C                   MOVEL(P)  ARBTIC        KEY
275100950202     C     KTAB          CHAIN     TABEL                              30
275200950202     C  N30              MOVEL     TBLUNI        DS1A
275300950202     C  N30              MOVEL     §1AFMB        CSBFUS
275400950202     C   30              CLEAR                   CSBFUS
275500950202     C                   ENDSR
275600920212     C**---------------------------------------------------------
275700920212     C** ANNULLO RECORD CONTRASSEGNO
275800920212     C**---------------------------------------------------------
275900920212     C     ANNCSB        BEGSR
276000941209     C* CREO STORICO VARIAZIONE
276100920602     C     CSBSTA        IFNE      9
276200920212     C                   MOVEL     'ANCA'        CSVCAV
276300920212     C                   EXSR      CSVWTR
276400920212     C* ANNULLO
276500920212     C                   MOVEL     9             CSBSTA
276600941130     C                   UPDATE    TNCSB000
276700920212     C                   END
276800920212     C                   ENDSR
276900920212     C**---------------------------------------------------------
277000920212     C** ESEGUE WRITE BLCSV00F                                  -
277100920212     C**---------------------------------------------------------
277200920212     C     CSVWTR        BEGSR
277300950202     C* COME DATA E ORA VARIAZIONE METTO QUELLA DELLA FILIALE
277400941130     C                   Z-ADD     ARBDTV        CSVDTV
277500941130     C                   Z-ADD     ARBORV        CSVORV
277600020604     c*
277700021112     c                   clear                   tibs36ds
277800021112     c                   movel     arbpru        i36ute
277900140407     c                   eval      %subst(i36ute:10:1)=' '
278000021112     C                   CALL      'TIBS36R'
278100021112     C                   PARM                    tibs36ds
278200020604     c* non tovato profilo su azute
278300070420   1 c     o36err        ifne      *blanks
278400020604     c* richiamo fnlv55r per sapere se lna è gestita da altro p.o. e da chi
278500020604     c                   clear                   dslv55
278600020604     c                   movel     '6'           d55tpt
278700020604     c                   z-add     taslna        d55lin
278800020604     c                   z-add     csvdtv        d55drf
278900020604     c                   call      'FNLV55R'
279000020604     c                   parm                    dslv55
279100020604     C                   MOVEL     d55tfa        CSVFEV
279200070420   x1c                   else
279300020604     c* trovato profilo su azute
279400021112     c                   movel     o36pou        csvfev
279500070420    1c                   endif
279600020604     c*
279700920212     C                   MOVEL     CSBDS         CSVDS
279800941130     C* COME DATA TRSMISSIONE QUANDO HO CARICATO
279900941130     C                   Z-ADD     DATEU         CSVDTR
280000941130     C                   WRITE     TNCSV000
280100920212     C                   ENDSR
280200920130     C**---------------------------------------------------------
280300920130     C** CONTROLLO ESISTENZA BOLLA
280400920130     C**---------------------------------------------------------
280500990628     C     CTRSPE        BEGSR
280600920302     C                   SETOFF                                       332940
280700941130     C                   SETOFF                                       4217
280800941121     C                   SETOFF                                       020304
280900930514     C                   SETON                                        23
281000080408     c                   clear                   VariataSede       1
281100110315     c                   clear                   SoloCASS          1
281200941121     C*
281300991015     C     KBLP          CHAIN     TITAS30C                           29
281400920220     C*
281500941121     C* 29 ON  - S P E D I Z I O N E   I N E S I S T E N T E
281600941121    1C     *IN29         IFEQ      *ON
281700941121     C*
281800941121     C     KBLP          CHAIN     TNBLA000                           40
281900941121     C   40KANN          CHAIN     TNBLA000                           40
282000920602     C*
282100930804     C* 40 OFF - S P E D I Z I O N E   A N N U L L A T A
282200991015     C  N40              MOVEL     ERR(28)       ARBDER
282300070119     c
282400070119     c* Verifico se lnp estera, se record bolla chiuso in partenza
282500070119     c*  come merce mai affidata
282600070119    2c                   if        *in40
282700070119     c
282800070119     c     ArbLnp        Chain     Azorg01l
282900070119    3c                   If        %Found(Azorg01l)
283000070119     c                   Eval      Og143 = OrgDe3
283100070119    4c                   If        §OgNtw = 'DPD' or §ogntw='EEX' or
283200070119     c                             §ogntw='FED'
283300070119     c* controllo bolla se presente in fnblp
283400070119     c     kann          chain     fnblp01l
283500070119    5c                   if        %found(fnblp01l) and blpft1='N'
283600070119     c                             and blpcca='7'
283700070119     c                   setoff                                       23
283800070119    5c                   endif
283900070119    4c                   endif
284000070119    3c                   endif
284100070119    2c                   endif
284200920302     C*
284300920602     C* 40 ON - INESISTENTE: VEDO SE GIA' TRASFORMATO COD BOLLA
284400070119     c   23
284500070119    2Can 40FLGCVB        IFEQ      'K'
284600941121    3C     COMTBL        IFEQ      §3ATB1
284700930108     C                   MOVEL     ERR(1)        ARBDER
284800930108     C                   ELSE
284900930108     C                   MOVEL     ERR(7)        ARBDER
285000941121    3C                   END
285100930108     C*
285200920602     C                   MOVEL     §3ATB1        SAVTB1            2
285300920602     C                   MOVEL     §3ATB2        SAVTB2            2
285400080408     c                   movel     ds3a          savds3a
285500920602     C*
285600920602     C                   MOVEL     '3A'          COD
285700070119     c                   MOVEL(P)  ARBCBN        KEY
285800920602     C     KTAB          CHAIN     TABEL                              32
285900920602     C  N32              MOVEL     TBLUNI        DS3A
286000080408     c   32              clear                   ds3a
286100080408     c                   movel     §3afca        newfca            1 0
286200920602     C*
286300941121     C* 1BOLLA MODIFICATA
286400941121    3C  N32SAVTB1        IFNE      §3ATB1
286500920602     C                   MOVEL     §3ATB1        COMTBL
286600990624     C     KBLP          SETLL     TITAS30C                               42
286700920602     C  N42              MOVEL     ERR(1)        ARBDER
286800941121    3C                   END
286900920602     C*
287000941121     C* 2 BOLLA MODIFICATA
287100920602     C* SOLO SE ESISTEVA CONTROLLO
287200941121    3C  N32SAVTB2        IFNE      §3ATB2
287300920602     C     SAVTB2        ANDNE     '  '
287400920602     C*
287500941121    4C     §3ATB2        IFEQ      '  '
287600920602     C                   MOVEL     'AP'          COMTBL
287700941121     C                   ELSE
287800920602     C                   MOVEL     §3ATB2        COMTBL
287900941121    4C                   END
288000990624     C     KBLP          SETLL     TITAS30C                               42
288100070119     C  N42              MOVEL     ERR(7)        ARBDER
288200941121    3C                   END
288300080408     C***                MOVEL     SAVTB1        §3ATB1
288400080408     C***                MOVEL     SAVTB2        §3ATB2
288500080408     c                   movel     savds3a       ds3a
288600070119     c
288700070119     c* Se codice bolla vecchio = codice bolla nuovo, anche se
288800070119     c*  inesistente è inutile ristamparlo: già fatto precedentemente
288900080408    3c  n42              if        arbcbp=arbcbn and §3afca=0
289000070119     c                   setoff                                       23
289100080408    3c                   endif
289200080408     c* Se si tratta di tipo bolla con C/ASS prima e senza dopo
289300080408     c*  o una KB che toglie solo in C/ASS,
289400080408     c*  se bolla modificata in C/S --> vado avanti per il C/ASS
289500090504    3c***                if        not *in42 and *in23  and §3atb2=*blanks
289600090504    3c                   if        *in23  and §3atb2=*blanks
289700080408    4c                   if        (newfca=0 and §3afca=1) or
289800080408     c                             (arbcbp=arbcbn and §3afca=1 and arbcas=0)
289900080408     C                   MOVEL     'F7'          COMTBL
290000080408     C     KBLP          chain     TITAS30C
290100080408    5c                   if        %found(titas30c)
290200080408     c                   setoff                                       2923
290300080408     c                   eval      VariataSede='S'
290400080408    5c                   endif
290500080408    4c                   endif
290600080408    3c                   endif
290700110315     c
290800110315     c* se ho trovato la bolla con cod bolla nuovo ma era stato annullato il C/Ass  o
290900110315     c*   non c'era e ora c'e' --> lo aggiungo o riabilito
291000110315    3c                   if        *in23  and *in42 and §3atb2=*blanks  and
291100110315    4c                              newfca=1 and §3afca=0
291200110315     c                   setoff                                       2923
291300110315     c                   eval       soloCAss='S'
291400110315     c                   endif
291500920602     C*
291600941121   X2C                   ELSE
291700920602     C*
291800941130     C* CONTROLLO SE LA SPEDIZIONE E' STORNATA
291900930514     C                   EXSR      STORNO
292000991019     c* se causale variazione 'Cx' e la spedizione e' stornata
292100991019     c* spengo in29 per caricare ugualmente la variazione
292200991021    3c     flgcvb        ifeq      'C'
292300060317    3c     flgcvb        oreq      'R'
292400991021    4c     *in23         ifeq      *off
292500991021     c                   setoff                                       29
292600991021   x4c                   else
292700991021     c* altrimenti cerco la spedizione
292800991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
292900991021     c* (cioè il franco) faccio setgt e readpe invece che chain
293000991021     c     kann          setgt     titas30c
293100991021     c     kann          readpe    titas30c                               23
293200991019     c  n23              setoff                                       29
293300991021    4c                   endif
293400991021    3c                   endif
293500080408     c
293600080408     c* per "I0" e codice bolla "Z" cerco la bolla come A2 e poi
293700080408     c*  aggiorno secondo i modi già previsti in AGARBI
293800140407    3c                   if        (comcvb='I0' or
293900140407     c                              comcvb='I7' or
294000140407     c                              comcvb='I8' or
294100140407     c                              comcvb='I9' )
294200140407    3c                              and arbcbo='Z'
294300080408     C                   MOVEL     'A2'          COMTBL
294400080408     C     KBLP          chain     TITAS30C
294500080408    4c                   if        %found(titas30c)
294600080408     c                   setoff                                       2923
294700080408    4c                   endif
294800080408    3c                   endif
294900110214     c* per "I0" e codice bolla "A" cerco la bolla come F1 e poi
295000110214     c*  aggiorno secondo i modi già previsti in AGARBI
295100140407    3c***                if        comcvb='I0' and (arbcbo='A' or arbcbo='Q')
295200140407    3c                   if        (comcvb='I0' or
295300140407     c                              comcvb='I7' or
295400140407     c                              comcvb='I8' or
295500140407     c                              comcvb='I9' )
295600140407     c                             and (arbcbo='A' or arbcbo='Q')
295700110214     C                   MOVEL     'F1'          COMTBL
295800110214     C     KBLP          chain     TITAS30C
295900110214    4c                   if        %found(titas30c)
296000110214     c                   setoff                                       2923
296100110214    4c                   endif
296200110214    3c                   endif
296300930514     C*
296400941121    3C   23COMTBL        IFEQ      §3ATB1
296500991015     C                   MOVEL     ERR(1)        ARBDER
296600920130     C                   ELSE
296700991015     C                   MOVEL     ERR(7)        ARBDER
296800941121    3C                   END
296900941121    2C                   END
297000941130     C**
297100941130     C* 23 ON --> STAMPO ERRORE
297200941130    2C     *IN23         IFEQ      *ON
297300920602     C  N42
297400920602     CAN 40              SETON                                        37
297500920212     C*        PER C.E.D.
297600920603     C  N42
297700920603     CAN 40
297800920603     CORN40              Z-ADD     102           COMFEV
297900920602     C  N42
298000920602     CAN 40
298100920602     CORN40              EXSR      WTRERR
298200941121    2C                   END
298300941121     C*
298400991019    1C                   Endif
298500941121     C*
298600991019    1c     *in29         ifeq      *off
298700941121     C* 29 OFF - S P E D I Z I O N E       E S I S T E N T E
298800121108     c* Salvo contenuto del tracciato record di titas in ds apposita:
298900121108     c* serve per la scrittura dell'immagine di titas prima della prima variazione
299000121108     c* nel file TIVRB00F
299100121108     c                   exsr      memo_dstitas
299200941121     C*
299300941121     C* 02 OFF             --> GIA' CONTABILIZZATA IN BLTAS10 O BLTASP
299400941130     C* 02 ON  MA TASFIC=1 --> GIA' CONTABILIZZATA IN BLTAS00
299500930512     C*
299600941130    2C     *IN02         IFEQ      *OFF
299700941121     C     *IN02         OREQ      *ON
299800941130     C     TASFIC        ANDNE     ' '
299900941130     C**
300000950202     C* VARIAZIONE "Ix" --> indirizzo/ peso/ volume
300100941130     C**
300200941130    3C     FLGCVB        IFEQ      'I'
300300941130     C*
300400950202     C* SE VARIO PESO/VOLUME      E NON E' LA 2 BOLLA --> ERRORE
300500960416    4C     COMCVB        IFNE      'I0'
300600140407    4C     COMCVB        andne     'I7'
300700140407    4C     COMCVB        andne     'I8'
300800140407    4C     COMCVB        andne     'I9'
300900941130     C*
301000991021    5C     §3ATB2        IFNE      tastbl
301100930513     C                   SETON                                        29
301200930804     C*
301300140219    6c***  TASPKB        IFNE      ARBPKF
301400130325     C***                MOVEL     ERR(9)        ARBDER
301500130325     C***                EXSR      WTRERR
301600120313     c                   exsr      ctr_vrb
301700140219   X6C***                ELSE
301800930603     C*
301900140219    7C***  TASFVF        IFNE      'T'
302000140219     C***  TASVLF        ANDNE     ARBVLF
302100140219    8C***  TASFVF        IFEQ      'Z'
302200140219     C***  TASVLF        ANDLT     ARBVLF
302300140219     C***  TASFVF        ORNE      'Z'
302400130325     C***                MOVEL     ERR(9)        ARBDER
302500130325     C***                EXSR      WTRERR
302600140219     c***                eval      wcntab='S'
302700140219     c***                exsr      ctr_vrb
302800140219    8C***                END
302900140219    7C***                END
303000930603     C*
303100140219    6C***                END
303200930603     C*
303300941130    5C                   ENDIF
303400941130     C*
303500930512   X4C                   ELSE
303600930512     C*
303700950308RM*  C*§SE VARIATO FERMO DEPOSITO E NON E' LA 2 BOLLA --> ERRORE
303800950308RM* 5C*§         TASFFD    IFNE ARBFFD
303900950308RM*  C*§         §3ATB2    ANDNECOMTBL
304000950308RM*  C*§                   MOVELERR,9     ARBDER
304100950308RM*  C*§                   EXSR WTRERR
304200950308RM* 5C*§                   END
304300990625     C**
304400990625     C* MEMORIZZO ULTIME 4 CIFRE CODICE FATTURAZIONE 2 BOLLA
304500991021    4C     §3ATB2        IFEQ      tastbl
304600990625     C                   MOVE      TASKSC        KSC42BO           4 0
304700930512    4C                   END
304800990625     C*
304900990625    4C                   END
305000930512    3C                   END
305100950202     C**
305200950202     C* VARIAZIONE "Cx" --> consegne particolari
305300950202     C**
305400950202    3C     FLGCVB        IFEQ      'C'
305500950202     C* no errore se 2 bolla
305600991021    4C     §3ATB2        IFNE      tastbl
305700991019     c
305800991019     C                   MOVEL     'TB'          COD
305900991021     C                   MOVEL(P)  tastbl        KEY
306000991019     C     KTAB          CHAIN     TABEL                              32
306100991019     C  N32              MOVEL     TBLUNI        DSTB
306200991019     C   32              MOVEL     *ALL'1'       DSTB
306300991019   4aC     §TBFCB        IFEQ      '1'
306400950202     C*
306500950202     C* STAMPO SOLO SE UNO DEI TRE VARIATO
306600950202    5C     TASFTC        IFEQ      ARBTC1
306700950202     C     TASTC2        ANDEQ     ARBTC2
306800950202     C     TASFTC        OREQ      ARBTC2
306900950202     C     TASTC2        ANDEQ     ARBTC1
307000950202     C*
307100950202   X5C                   ELSE
307200950202     C* CONTROLLO SE LA TARIFFA TASSA LE CONSEGNE PARTICOLARI
307300140219     C***                EXSR      CONPAR
307400950202     C*
307500140219    6C**   *IN17         IFEQ      *ON
307600130325     C****               MOVEL     ERR(9)        ARBDER
307700130325     C****               EXSR      WTRERR
307800120320     c                   exsr      ctr_vrb
307900140219    6C**                ENDIF
308000950202    5C                   ENDIF
308100950202     C*
308200991019   4ac                   ENDIF
308300950202    4C                   ENDIF
308400950202    3C                   ENDIF
308500941130     C**
308600941130     C* VARIAZIONE "Tx" --> tassazione 1 bolla
308700941130     C**
308800941130    3C     FLGCVB        IFEQ      'T'
308900950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
309000130326     C***  ARBFIV        IFGT      0
309100130326     C***  TASFIV        ANDNE     ARBFIV
309200950324     C                   Z-ADD     46            COMFEV
309300130326     C***                ENDIF
309400950324     C*
309500930513     C                   MOVEL     ERR(9)        ARBDER
309600990505     c                   eval      wdelarbu='S'
309700920220     C                   EXSR      WTRERR
309800920226     C                   SETON                                        29
309900011029     c                   if        tasias > 0  and tasvas = *blanks
310000011029     c                   movel     'ITL'         tasvas
310100011029     c                   endif
310200990419     c* Controllo se variato l'importo da assicurare se contabilizzata     da assicurare
310300990419     c                   if        *in13 and (arbias <> tasias or
310400990419     c                             arbvas <> tasvas)
310500990419     c*
310600990419     c                   exsr      AGGIAS
310700990419     c*
310800990419     c                   endif
310900941130    3C                   END
311000941130     C**
311100941130     C* VARIAZIONE "2x" --> tassazione 2 bolla
311200941130     C**
311300941130    3C     FLGCVB        IFEQ      '2'
311400991021     C     tastbl        ANDEQ     §3ATB2
311500950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
311600130326     C**   ARBFIV        IFGT      0
311700130326     C**   TASFIV        ANDNE     ARBFIV
311800950324     C                   Z-ADD     46            COMFEV
311900130326     C**                 ENDIF
312000950324     C*
312100950203     C                   MOVEL     §3ATB2        TBLERR
312200930513     C                   MOVEL     ERR(9)        ARBDER
312300920226     C                   EXSR      WTRERR
312400920226     C                   SETON                                        29
312500941130    3C                   END
312600950202     C**
312700950202     C* VARIAZIONE "Mx" --> mittente
312800990628     c*                     la variazione del mittente dipende dalla conab
312900990628     c*                     della prima bolla, a prescindere da come è messa
313000990628     c*                     la seconda: se aggiono la  aggiorno anche la 2
313100950202     C**
313200950202    3C     FLGCVB        IFEQ      'M'
313300991021    5C     §3ATB2        ANDNE     tastbl
313400130326     C                   Z-ADD     46            COMFEV
313500950202     C                   MOVEL     ERR(9)        ARBDER
313600950202     C                   EXSR      WTRERR
313700950202     C                   SETON                                        29
313800950202    3C                   END
313900941130    2C                   END
314000930512    1C                   END
314100941130     C*
314200920226     C                   ENDSR
314300920228     C**---------------------------------------------------------
314400920228     C** CHAIN SU PRIMA BOLLA
314500920228     C**---------------------------------------------------------
314600920228     C     CHTB1         BEGSR
314700920228     C                   MOVEL     SAVTB1        COMTBL
314800941130     C                   SETOFF                                       020304
314900990625     C     KBLP          CHAIN     TITAS30C                           39
315000121108     c                   exsr      memo_dstitas
315100920228     C*
315200990414     C                   EXSR      CTR3
315300920228     C                   ENDSR
315400920228     C**---------------------------------------------------------
315500920228     C** CHAIN SU SECONDA BOLLA
315600920228     C**---------------------------------------------------------
315700920228     C     CHTB2         BEGSR
315800920228     C                   MOVEL     SAVTB2        COMTBL
315900941130     C                   SETOFF                                       020304
316000990625     C     KBLP          CHAIN     TITAS30C                           39
316100121108     c                   exsr      memo_dstitas
316200920228     C*
316300990414     C                   EXSR      CTR3
316400920228     C                   ENDSR
316500920226     C**---------------------------------------------------------
316600920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
316700920226     C**---------------------------------------------------------
316800920226     C     CTR2          BEGSR
316900920302     C                   MOVEL     COMTBL        FLGTB1            1
317000950202     C* SPEDIZIONE 1) GIA' IMPONIBILE     SE NO FATTURA E NO ASSEGNATO
317100950202     C*               SE VARIAZOINE DA ARRIVO
317200950202     C*            2) GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
317300950202     C*            --> NON POSSO VARIARE
317400950202    1C     TASDFT        IFGT      0
317500950202    2C     TASFIC        IFNE      ' '
317600920226     C                   SETON                                        29
317700130326     c                   z-add     46            comfev
317800991015     C                   MOVEL     ERR(9)        ARBDER
317900920226     C                   EXSR      WTRERR
318000950202    2C                   END
318100920226     C*
318200950202   X1C                   ELSE
318300920226     C*
318400990624    2C     TASimv        IFNE      *zeros
318500950202     C     COMCVB        ANDNE     'TP'
318600950202    3C     FLGTB1        ANDNE     'A'
318700920302     C                   SETON                                        29
318800920226     C*
318900991015     C
319000991015     C   29              MOVEL     ERR(8)        ARBDER
319100130326     c   29              z-add     46            comfev
319200920226     C   29              EXSR      WTRERR
319300950202    2C                   END
319400920302     C*
319500950202    1C                   END
319600920226     C                   ENDSR
319700920226     C**---------------------------------------------------------
319800920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
319900941130     C** PER LE VAIRAZIONI C/ASSEGNO O TIPI BOLLA
320000920226     C**---------------------------------------------------------
320100920226     C     CTR3          BEGSR
320200920303     C                   SETOFF                                       2938
320300920226     C* SPEDIZIONE GIA' IMPONIBILE     SE NO FATTURA
320400920226     C*            GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
320500920226     C*            NON POSSO VARIARE
320600920226     C*
320700990414     C  N02              SETON                                        2936
320800941202     C   02TASDFT        IFGT      0
320900990414     C     TASFIC        ANDNE     ' '
321000920227     C                   SETON                                        2936
321100920226     C*
321200920226     C                   ELSE
321300920226     C*
321400990624     C     TASimv        IFNE      *zeros
321500920226     C                   SETON                                        38
321600920226     C                   END
321700920226     C*
321800920226     C                   END
321900920226     C                   ENDSR
322000920211     C**---------------------------------------------------------
322100920211     C** SCRIVO FILE ERRORI
322200920211     C**---------------------------------------------------------
322300920211     C     WTRERR        BEGSR
322400110208     c                   clear                   arbdat
322500920211     C*
322600970916     C* CREO IL RECORD DI ERRORE SOLO SE LA DATA VARIAZIONE E' INFERIOR
322700970916     C* DI 3 GIORNI A OGGI PER LE STAMPA DI SEDE
322800000703    0C     COMFEV        IFEQ      102
322900110208     C     ARBDTV        ANDLE     DATAM3
323000000703     c     *in37         andeq     *on
323100100225
323200110208     c* Se si tratta di codice TP con importo da assicurare, stampo subito
323300100225    0C     COMFEV        oreq      102
323400100225     c     *in13         andeq     *on
323500100225     c     arbias        andgt     0
323600970916     C*é*
323700970916     C     COMFEV        ORNE      102
323800000703     C*é*
323900000703     C     *in37         ORNE      *on
324000970916     C**
324100991015     C     KERR          SETLL     FIARBE2C                               01
324200930702     C* SE NON TROVATO ERRORE
324300930706    1C     *IN01         IFEQ      *OFF
324400110208     c
324500110208     c* Se l'errore è bolla inesistente, aspetto 3 gg prima di ricrearlo
324600110208    2c                   if        arbder=err(1)
324700110208     c                   eval      arbdat=dateu
324800110208     C     KERR          SETLL     FIARBE2C                               01
324900110208    3c                   if        not  *in01
325000110208     c                   eval      arbdat=datam1
325100110208     C     KERR          SETLL     FIARBE2C                               01
325200110208    4c                   if        not  *in01
325300110208     c                   eval      arbdat=datam2
325400110208     C     KERR          SETLL     FIARBE2C                               01
325500110208    4c                   endif
325600110208    3c                   endif
325700110208    2c                   endif
325800110208    1c                   endif
325900110208     c
326000110208    1C     *IN01         IFEQ      *OFF
326100110209     c                   clear                   arbdat
326200930702     C* RISCRIVO ERRORE
326300950203     C* SE NON L'HO GIA' IMPOSTATO CERCO FILIALE A CUI INVIARE STAMPA
326400930706    2C     COMFEV        IFEQ      0
326500920212     C*
326600950203    3C     TBLERR        IFEQ      'F'
326700991015     C                   MOVEL     ARBLNP        ARBFEV
326800930706   X3C                   ELSE
326900991015     C                   MOVEL     TASLNA        ARBFEV
327000930706    3C                   END
327100971215     C* PRENDO TERMINAL DI PARTENZA
327200971215     C                   CLEAR                   DSLV55
327300971215     C                   MOVEL     'P'           D55TPT
327400991015     C                   MOVE      ARBFEV        D55LIN
327500971215     C                   Z-ADD     DATEU         D55DRF
327600971215     C                   CALL      'FNLV55R'
327700971215     C                   PARM                    DSLV55
327800971215     C     D55ERR        IFEQ      *BLANKS
327900991015     C                   MOVE      D55TFP        ARBFEV
328000971215     C                   END
328100920212     C*
328200930706   X2C                   ELSE
328300920212     C*
328400991015     C                   Z-ADD     COMFEV        ARBFEV
328500930706    2C                   END
328600991015     C     ARBFEV        IFEQ      102
328700070420     C*********          MOVE      MBR           ARBLNA
328800950202     C                   ELSE
328900950202     C                   MOVEL     TASLNA        ARBLNA
329000950202     C                   ENDIF
329100920211     C*
329200950201     C   11              WRITE     FNARBDEE
329300920212     C*
329400950201     C   12              WRITE     FNARBKEE
329500950201     C* SCRIVO ANCHE FNARBM0E SOLO SE RICHIESTO
329600930706    2C   12*IN36         IFEQ      '1'
329700950202     C     KARBM         CHAIN     FNARBMRR                           32
329800950201     C  N32              WRITE     FNARBMEE
329900920214     C                   SETOFF                                       36
330000930706    2C                   END
330100920212     C*
330200950203     C     *IN13         IFEQ      *ON
330300991015     C                   WRITE     FIARBTEE
330400950203     C*
330500991015     c* se devo cancellare fiarbu, lo faccio
330600991018     c     arbtrc        ifeq      '1'
330700991015     C     KARBU         SETLL     FIARBU1R
330800991015     C     KARBU         READE     FIARBU1R                               30
330900950203     C*
331000950203     C     *IN30         DOWEQ     *OFF
331100991015     C                   WRITE     FIARBUEE                             30
331200950203     C* SE POSSO LI DELETO
331300990505     c  n37              if        wdelarbu='S'
331400990505     C                   ADD       1             NUMRBU
331500991015     C                   DELETE    FIARBURR
331600990505     c                   endif
331700950203     C*
331800991015     C     KARBU         READE     FIARBU1R                               30
331900950203     C                   ENDDO
332000991018     c                   endif
332100950203     C                   ENDIF
332200950203     C*
332300950202     C   15              WRITE     FNARBGEE
332400950202     C   16              WRITE     FNARBMEE
332500991214   X1c                   else
332600991214     c* se devo cancellare fiarbu, lo faccio
332700991214    2c     *in37         ifeq      *off
332800991214     c     wdelarbu      andeq     'S'
332900991214     c     arbtrc        andeq     '1'
333000991214     C     KARBU         SETLL     FIARBU1R
333100991214     C     KARBU         READE     FIARBU1R                               30
333200991214     C*
333300991214    3C     *IN30         DOWEQ     *OFF
333400991214     C                   ADD       1             NUMRBU
333500991214     C                   DELETE    FIARBURR
333600991214     C     KARBU         READE     FIARBU1R                               30
333700991214    3C                   ENDDO
333800991214    2c                   endif
333900930706    1C                   ENDIF
334000970916    0C                   ENDIF
334100920212     C*
334200920212     C                   Z-ADD     0             COMFEV            3 0
334300950203     C                   MOVEL     §3ATB1        TBLERR
334400920211     C                   ENDSR
334500930514     C**---------------------------------------------------------
334600930514     C** CONTROLLO SE LA SPEDIZIONE INESISTENTE E' STORNATA
334700930514     C**---------------------------------------------------------
334800930514     C     STORNO        BEGSR
334900930514     C*
335000930514     C* SE E' SINGOLA BOLLA
335100930514     C     §3ATB2        IFEQ      '  '
335200930514     C*
335300991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
335400991021     c* (cioè il franco) faccio setgt e readpe invece che chain
335500991021     C     KANN          setgt     TITAS30C
335600991021     C     KANN          readpe    TITAS30C                               23
335700931119     C*
335800941130     C     *IN23         IFEQ      *OFF
335900930514     C                   MOVEL     'TB'          COD
336000941130     C                   MOVEL(P)  TASTBL        KEY
336100930514     C     KTAB          CHAIN     TABEL                              32
336200930514     C  N32              MOVEL     TBLUNI        DSTB
336300930514     C   32              MOVEL     *ALL'1'       DSTB
336400930514     C*
336500930514     C* SE NON E' STORNO STAMPO ERRORE
336600930514     C     §TBFCB        IFEQ      '1'
336700930514     C                   SETON                                        23
336800930514     C                   ELSE
336900930514     C                   MOVEL     '1'           ST1               1
337000930514     C                   END
337100930514     C                   END
337200930514     C*
337300930514     C                   END
337400930514     C                   ENDSR
337500930804     C*
337600950202     C**---------------------------------------------------------
337700950202     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE
337800950202     C**---------------------------------------------------------
337900930804     C     CONPAR        BEGSR
338000930804     C                   SETOFF                                       17
338100930804     C*
338200930804     C                   MOVE      TASKSC        KSC4              4 0
338300930804     C*
338400930804     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
338500930804    2C     KSC4          IFEQ      9999
338600930804     C     KSC4          OREQ      8888
338700930804     C                   SETON                                        17
338800930804     C*
338900930804   X2C                   ELSE
339000930804     C*
339100930804     C                   Z-ADD     999           TARPRG
339200930804     C                   Z-ADD     999           PREPRG
339300991021     C                   MOVEL     tastbl        UNOTB1
339400930804     C*
339500930805     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
339600930805     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
339700930804    3C     UNOTB1        IFEQ      'F'
339800930805     C                   MOVEL     'A'           TFP               1
339900930804     C                   ELSE
340000930805     C                   MOVEL     'F'           TFP
340100930804    3C                   END
340200930804     C*
340300930804     C* SE C'E' A ZERO NON DO' L'ERRORE
340400941130     C     KTAM          SETLL     TNTAM000
340500941130     C     KTAM          READE     TNTAM000                               35
340600930804     C*
340700930804    3C     *IN35         DOWEQ     *OFF
340800930804     C*
340900930804     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
341000930804    4C     TAMATB        IFNE      'A'
341100930804     C*
341200941205    5C     TAMBAP        IFNE      'B'
341300941205     C     TAMBAP        ANDNE     TFP
341400930804     C*
341500930804     C                   MOVE      TAMPRG        TARPRG            3 0
341600930804     C*
341700930804     C* VEDO SE E' TARIFFA VALIDA
341800121116     c                   movel     tasaas        tasdsp
341900121116     c                   move      tasmgs        tasdsp
342000120430    6C     TASDSP        IFGE      TAMDDT
342100120430     C     TASDSP        ANDLE     TAMDST
342200120430     C                   SETON                                        35
342300120430    6C                   END
342400930804     C*
342500930804   X5C                   ELSE
342600930804     C                   MOVE      TAMPRG        PREPRG            3 0
342700930804    5C                   END
342800930804    4C                   END
342900930804     C*
343000941130     C  N35KTAM          READE     TNTAM000                               35
343100930804    3C                   END
343200930804     C*
343300930804     C* NON TROVATA TARIFFA: STAMPO ERRORE
343400930804    3C     TARPRG        IFEQ      999
343500930804     C     PREPRG        ANDEQ     999
343600930804     C                   SETON                                        17
343700930804   X3C                   ELSE
343800930804     C*
343900930804    4C     TARPRG        IFNE      999
344000930804     C                   MOVEL     TARPRG        PRG
344100930804   X4C                   ELSE
344200930804     C                   MOVEL     PREPRG        PRG
344300930804    4C                   ENDIF
344400930804     C*
344500930804     C* VEDO SE TARIFFA PARTICOLARE A 0
344600950202     C     ARBTC1        IFNE      ' '
344700990719     C                   MOVEL(P)  ARBTC1        WFTC
344800950202     C                   EXSR      PARTIC
344900950202     C                   ENDIF
345000950202     C  N17ARBTC2        IFNE      ' '
345100990719     C                   MOVEL(P)  ARBTC2        WFTC
345200950202     C                   EXSR      PARTIC
345300950202     C                   ENDIF
345400950202     C*
345500930804    3C                   END
345600930804    2C                   END
345700930804     C*
345800930804     C                   ENDSR
345900950202     C**---------------------------------------------------------
346000950202     C* CONTROLLO IN TPT
346100950202     C**---------------------------------------------------------
346200950202     C     PARTIC        BEGSR
346300950202     C*
346400990719     C     KTAM2         CHAIN     TITPT000                           17
346500950202     C* 17 ON - NON ESISTE TARIFFA
346600950202    4C     *IN17         IFEQ      *OFF
346700950202     C*         NON ESISTE PERCHE' ANNULLATA
346800950202    5C     TPTATB        IFEQ      'A'
346900950202     C                   SETON                                        17
347000950202   X5C                   ELSE
347100950202     C* VEDO SE ESISTE DETTAGLIO TARIFFA PARTICOLARE
347200990719     C     KTAM2         SETLL     TITPD000
347300990719     C     KTAM2         READE     TITPD000                               35
347400950202     C*
347500950202    6C     *IN35         DOWEQ     *OFF
347600950202     C* NO ANNULLATO
347700950202    7C     TPDATB        IFEQ      ' '
347800950202     C* SE > 0 ERRORE
347900950202    8C     TPDITR        IFNE      0
348000950202     C     TPDMIN        ORNE      0
348100950202     C     TPDMAX        ORNE      0
348200950202     C                   SETON                                        1735
348300950202    8C                   END
348400950202    7C                   END
348500950202     C*
348600990719     C  N35KTAM2         READE     TITPD000                               35
348700950202    6C                   END
348800950202    5C                   END
348900950202    4C                   END
349000950202     C                   ENDSR
349100941130     C**---------------------------------------------------------
349200941130     C* CONTROLLO SE ESISTE VARIA "G"
349300941130     C**---------------------------------------------------------
349400941130     C     ESVARG        BEGSR
349500941130     C                   CLEAR                   WSIVAR
349600941130     C*
349700941130     C                   SETON                                        30
349800941130    3C     TASSV1        IFNE      'G'
349900941130     C     TASSV2        ANDNE     'G'
350000941130     C     TASSV3        ANDNE     'G'
350100941130     C                   MOVEL     'G'           WSV1
350200990625     C     KBLPV         SETLL     TITA730C                               30
350300941130    3C                   ENDIF
350400941130     C*
350500941130     C     *IN30         IFEQ      *ON
350600941130     C                   MOVEL     'S'           WSIVAR            1
350700941130     C                   ENDIF
350800941130     C                   ENDSR
350900941202     C**---------------------------------------------------------
351000941202     C* CONTROLLO SE HO MODIFICATO IL DESTINATARIO -------------------*
351100941202     C**---------------------------------------------------------
351200941202     C     CTRDES        BEGSR
351300941202     C*
351400950202     C                   MOVEL     CSBRSD        ALF35            35
351500941202     C* CONTROLLO LA 1 POSIZIONE <> ' '
351600950202     C     ' '           CHECKR    ALF35         C                 3 0    30
351700950202     C                   MOVE      ALF35         W001A             1
351800941202     C*
351900941202     C* CONFRONTO CON IL CAMPO CHE HO RICEVUTO
352000941202     C     *IN30         IFEQ      *ON
352100941215     C     W001A         ANDEQ     ' '
352200950202     C                   MOVEA     ARBRSD        K35
352300941202     C                   ADD       1             C
352400950202     C                   MOVEA     *BLANKS       K35(C)
352500950202     C                   MOVEA     K35           W035A            35
352600941202     C                   ELSE
352700950202     C                   MOVEL     ARBRSD        W035A
352800941202     C                   ENDIF
352900941202     C*
353000950202     C     W035A         IFEQ      ALF35
353100941202     C                   SETOFF                                       31
353200941202     C                   END
353300941202     C*
353400941202     C                   ENDSR
353500990415     C**------------------------------------------------------------------------------
353600990415     C** VARIAZIONE IMPORTO D'ASSICURARE PER SPEDIZIONE GIA' TASSATA / CONTABILIZZATA
353700990415     C**------------------------------------------------------------------------------
353800990415     c*
353900990419     c     AGGIAS        begsr
354000990415     c*
354100990415     c* controllo se la spedizione è già stata fatturata / contabilizzata oppure
354200990415     c* se è stato calcolato l'imponibile
354300990415     c*
354400990415     c                   exsr      ctr3
354500990415     c* se è acceso il  38 vuol dire che non è stata contabilizzata ma è stato calcolato
354600991018     c* fino all'imponibile e quindi devo andare a controllare se è già stata
354700990415     c* calcolata la varia "R"
354800990519     c                   select
354900990519    1c                   when      *in38
355000990421     c*
355100990421     c                   z-add     tasias        arbiap
355200990421     c                   movel     tasvas        arbvap
355300990421     c                   movel     'R'           arbvar
355400991019     c*
355500991019     c* verifico se devo registrare messagio per ufficio assicurazione
355600991019     C* relativo a spedizione avente tipo bolla non assicurabile per la
355700991019     c* quale è stato tolto, modificato o aggiunto l'importo da assicurare
355800991019     c* Escludo le bolle di recupero
355900991019     c     tblrbl        ifne      'R'
356000991019     c                   exsr      msgass
356100991019     c                   endif
356200131129
356300131129     c                   clear                   arbiap
356400131129     c                   clear                   arbvap
356500131129     c                   clear                   arbvar
356600990415     c*
356700990824     c* Non faccio niente se spedizione:
356800990824     c* - contabilizzata
356900990824     c* - tasias> 0
357000990824     c* - tasfcm="F" (tasias immesso dalla sede (dalla fatturazione))
357100990824     c* - arbias= 0
357200990824     c                   when      *in29 and tasias > 0 and tasfcm = 'F'
357300990824     c                                   and arbias = 0
357400990824     c*
357500990519     c* se è acceso il 29  vuol dire che è stata CONTABILIZZATA
357600990519    1c                   when      *in29
357700990505     c                   eval      wdelarbu='S'
357800990415     c
357900990415     c* verifico l'esistenza della varia "R"
358000990415     c                   exsr      esvarr
358100990419     c*
358200990419     c                   z-add     tasias        arbiap
358300990419     c                   movel     tasvas        arbvap
358400990419     c                   movel     'R'           arbvar
358500990415     c*
358600990519    2c                   SELECT
358700990415     c*
358800990415     c* importo in TAS = SI
358900990415     c* importo in ARB = NO
359000990415     c* varia   "R"    = SI
359100990415     c*                            errore no tolta varia "R"
359200991019     c                   when      tasias > 0 and arbias = 0
359300130325     c***                if        Wvariar = 'S'
359400130325     c***                movel     err(34)       arbder
359500130325     c***                exsr      WTRERR
359600130325     c***                endif
359700121203     c                   exsr      ctr_vrb
359800990415     c* messaggio all'Ufficio Assicurazione
359900990519     c* escludo le bolle di recupero
360000990519     c                   if        tblrbl<>'R'
360100990415     c                   z-add     603           comfev
360200990415     c                   movel     err(32)       arbder
360300990419     c                   exsr      wtrerr
360400990519     c                   endif
360500990415     c*
360600990415     c* importo in TAS = NO
360700990415     c* importo in ARB = SI
360800990415     c* varia   "R"    = NO
360900990415     c*                            errore no aggiunta varia "R"
361000991019     c                   when      tasias = 0 and arbias > 0
361100130325     c                   if        wvariar = ' '
361200130325     c***                movel     err(35)       arbder
361300130325     c***                exsr      wtrerr
361400120423     c                   exsr      ctr_vrb
361500991019     c                   endif
361600990415     c* messaggio all'Ufficio Assicurazione
361700990519     c                   if        tblrbl<>'R'
361800990415     c                   z-add     603           comfev
361900990415     c                   movel     err(31)       arbder
362000990419     c                   exsr      wtrerr
362100990519     c                   endif
362200990415     c*
362300990415     c* importo in TAS = SI
362400990415     c* importo in ARB = SI  ma diversa da tas
362500990415     c* varia   "R"    = SI
362600990415     c*                            errore no modificata varia "R"
362700991019     c                   when      tasias <> arbias
362800991019     c                   if        wvariar = 'S'
362900130325     c***                movel     err(36)       arbder
363000130325     c***                exsr      wtrerr
363100120423     c                   exsr      ctr_vrb
363200991019     c                   endif
363300990415     c* messaggio all'Ufficio Assicurazione
363400990519     c                   if        tblrbl<>'R'
363500990415     c                   z-add     603           comfev
363600990415     c                   movel     err(33)       arbder
363700990419     c                   exsr      wtrerr
363800990519     c                   endif
363900990415     c*
364000990519    2c                   endsl
364100990415     c*
364200990624     c* aggiorno in TITAS il valore dell'importo d'assicurare
364300990415     c                   z-add     arbias        tasias
364400990415     c                   movel     arbvas        tasvas
364500990824     c                   clear                   tasfcm
364600990415     c*
364700990624     c   02              update    titas000
364800990624     c   03              update    titas010
364900990624     c   04              update    titasp00
365000990419     c*
365100990419     c                   clear                   arbiap
365200990419     c                   clear                   arbvap
365300990419     c                   clear                   arbvar
365400990419     c**
365500990415     c* riaccendo il 29 e spengo il 36
365600990415     c                   seton                                        29
365700990415     c                   setoff                                       36
365800991019     c* Se e' bolla normale, ma non compare nel tabulato per la
365900991019     c*  Consuldanni, stampo solo per l'ufficio assicurazione
366000990519     c                   other
366100990519     c                   if        tblrbl<>'R'
366200000410     c                   z-add     tasias        arbiap
366300000410     c                   movel     tasvas        arbvap
366400000410     c                   movel     'R'           arbvar
366500991019     c                   exsr      msgass
366600000410     c                   clear                   arbiap
366700000410     c                   clear                   arbvap
366800000410     c                   clear                   arbvar
366900990519     c                   endif
367000990519    1c                   ENDSL
367100990415     c*
367200990415     c                   endsr
367300990415     c*
367400991019     C**-------------------------------------------------------------
367500991019     C** messaggi per ufficio Assicurazione per tipo bolla non assic.
367600991019     C**-------------------------------------------------------------
367700991019     C     msgass        BEGSR
367800991019     C                   MOVEL     'TB'          COD
367900991019     c                   movel(P)  tastbl        key
368000991019     C     KTAB          CHAIN     TABEL                              32
368100991019     C  N32              MOVEL     TBLUNI        DSTB
368200991019     C   32              clear                   DSTB
368300991019     c* se bolla non assicurabile e aggiunto, tolto o modificato importo
368400991019     c* da assicurare devo stampare messaggio per ufficio Assicurazione
368500991019    1c     §tbfas        ifeq      '0'
368600991019    2c                   select
368700991019     c* tolto importo da assicurare
368800991019   w1c                   when      tasias > 0 and arbias = 0
368900991019     c                   z-add     603           comfev
369000991019     c                   movel     err(41)       arbder
369100991019     c                   exsr      wtrerr
369200991019     c* aggiunto importo da assicurare
369300991019   w2c                   when      tasias = 0 and arbias > 0
369400991019     c                   z-add     603           comfev
369500991019     c                   movel     err(40)       arbder
369600991019     c                   exsr      wtrerr
369700991019     c* modificato importo da assicurare
369800991019   w3c                   when      tasias <> 0
369900991019     c                   z-add     603           comfev
370000991019     c                   movel     err(42)       arbder
370100991019     c                   exsr      wtrerr
370200991019    2c                   endsl
370300991019    1c                   endif
370400991019     c                   endsr
370500990415     C**---------------------------------------------------------
370600990415     C* CONTROLLO SE ESISTE VARIA "R"
370700990415     C**---------------------------------------------------------
370800990415     C     ESVARR        BEGSR
370900990415     C                   CLEAR                   WVARIAR           1
371000990415     C*
371100990415     C                   SETON                                        30
371200990415    3C     TASSV1        IFNE      'R'
371300990415     C     TASSV2        ANDNE     'R'
371400990415     C     TASSV3        ANDNE     'R'
371500990415     C                   MOVEL     'R'           WSV1
371600990624     C     KBLPV         SETLL     TITA730C                               30
371700990415    3C                   ENDIF
371800990415     C*
371900990415     C     *IN30         IFEQ      *ON
372000990415     C                   MOVEL     'S'           WVARIAR
372100990415     C                   ENDIF
372200990415     C                   ENDSR
372300941130     C**---------------------------------------------------------
372400941130     C* ROUTINE INIZIALE
372500941130     C**---------------------------------------------------------
372600941130     C     *INZSR        BEGSR
372700021014
372800941130     C     *ENTRY        PLIST
372900941130     C                   PARM                    KPJBA
373000021014
373100941130     C                   Z-ADD     1             CODUT             1 0
373200021014
373300021014     c     *dtaara       define    §azute        azuteds
373400021014     c     *dtaara       define    §datiute      ddatiute
373500021014     C                   in(E)     *dtaara
373600021014     C                   IF        %Error  or  RSUT = *blanks
373700021014     C                   CLEAR                   TIBS34DS
373800021014     C                   call      'TIBS34R'
373900021014     C                   parm                    Tibs34Ds
374000021014     C                   in        *dtaara
374100021014     c                   ENDIF
374200021014
374300021014     C                   Z-ADD     DUTKCI        KCI               4 0
374400021014
374500941130     C*---------------------------------------------------------------*
374600991018     C     KBLP          KLIST
374700950201     C                   KFLD                    ARBAAS
374800941130     C                   KFLD                    ARBLNP
374900941130     C                   KFLD                    ARBNRS
375000941130     C                   KFLD                    ARBNSP
375100941130     C                   KFLD                    COMTBL            2
375200990629     C     KTITA         KLIST
375300990629     C                   KFLD                    TASAAS
375400990629     C                   KFLD                    TASLNP
375500990629     C                   KFLD                    TASNRS
375600990629     C                   KFLD                    TASNSP
375700990629     C                   KFLD                    §TRC
375800941130     C     KBLPV         KLIST
375900950201     C                   KFLD                    ARBAAS
376000941130     C                   KFLD                    ARBLNP
376100941130     C                   KFLD                    ARBNRS
376200941130     C                   KFLD                    ARBNSP
376300941130     C                   KFLD                    COMTBL            2
376400941130     C                   KFLD                    WSV1
376500941130     C     KANN          KLIST
376600950201     C                   KFLD                    ARBAAS
376700941130     C                   KFLD                    ARBLNP
376800941130     C                   KFLD                    ARBNRS
376900941130     C                   KFLD                    ARBNSP
377000950202     C     KARBU         KLIST
377100941130     C                   KFLD                    ARBAAS
377200941130     C                   KFLD                    ARBLNP
377300941130     C                   KFLD                    ARBNRS
377400941130     C                   KFLD                    ARBNSP
377500991015     C                   KFLD                    ARBTRC
377600941130     C                   KFLD                    ARBDTV
377700941130     C                   KFLD                    ARBORV
377800950202     C     KARBM         KLIST
377900950202     C                   KFLD                    ARBAAS
378000950202     C                   KFLD                    ARBLNP
378100950202     C                   KFLD                    ARBNRS
378200950202     C                   KFLD                    ARBNSP
378300950202     C                   KFLD                    ARBCVB
378400950202     C                   KFLD                    ARBDTV
378500950202     C                   KFLD                    ARBORV
378600941130     C     KARBD         KLIST
378700941130     C                   KFLD                    ARBAAS
378800941130     C                   KFLD                    ARBLNP
378900941130     C                   KFLD                    ARBNRS
379000941130     C                   KFLD                    ARBNSP
379100941130     C                   KFLD                    ARBCVB
379200941130     C                   KFLD                    ARBDTV
379300941130     C                   KFLD                    COMORV            6 0
379400941130     C     KTAB          KLIST
379500941130     C                   KFLD                    CODUT
379600941130     C                   KFLD                    COD               2
379700941130     C                   KFLD                    KEY               8
379800941130     C     KACO          KLIST
379900941130     C                   KFLD                    CODUT
380000941130     C                   KFLD                    KCI
380100941130     C                   KFLD                    TASKSC
380200950202     C     KACO2         KLIST
380300950202     C                   KFLD                    CODUT
380400950202     C                   KFLD                    KCI
380500950202     C                   KFLD                    ARBCCM
380600981118     C     Kppa          KLIST
380700981118     C                   KFLD                    wsoc
380800981118     C                   KFLD                    wctb
380900981118     C                   KFLD                    wkcc
381000981118     C                   KFLD                    wksc
381100981118     C                   KFLD                    wdft
381200981118     C                   KFLD                    wnft
381300981118     C                   KFLD                    wfiv
381400981118     C     Kycpa         KLIST
381500981118     C                   KFLD                    ppasys
381600981118     C                   KFLD                    ppanrasint
381700941130     C     KERR          KLIST
381800941130     C                   KFLD                    ARBAAS
381900941130     C                   KFLD                    ARBLNP
382000941130     C                   KFLD                    ARBNRS
382100941130     C                   KFLD                    ARBNSP
382200941130     C                   KFLD                    ARBDER
382300941130     C                   KFLD                    ARBDAT
382400991214     c                   kfld                    ARBDTV
382500991214     c                   kfld                    ARBORV
382600941130     C     KTAM          KLIST
382700941130     C                   KFLD                    TASKSC
382800941130     C                   KFLD                    TASCTR
382900941130     C     KTAM2         KLIST
383000941130     C                   KFLD                    TASKSC
383100941130     C                   KFLD                    TASCTR
383200941130     C                   KFLD                    PRG               3 0
383300950202     C                   KFLD                    WFTC
383400151119     C     Kar5          KLIST
383500151119     C                   KFLD                    ARBAAS
383600151119     C                   KFLD                    ARBLNP
383700151119     C                   KFLD                    ARBNRS
383800151119     C                   KFLD                    ARBNSP
383900151119     C                   KFLD                    kar5trd
384000121108
384100121108
384200121108
384300941130     C**
384400941130     C* DEFINIZIONE CAMPI
384500941130     C**
384600960625     C     *LIKE         DEFINE    TASTBL        SAVTBL
384700990719     C     *LIKE         DEFINE    tptftc        WFTC
384800950202     C     *LIKE         DEFINE    TASVLF        SAVVLF
384900950202     C     *LIKE         DEFINE    TASVLF        SAVVLB
385000950202     C     *LIKE         DEFINE    TASFVF        SAVFVF
385100950202     C     *LIKE         DEFINE    TASFVF        SAVFVB
385200990625     C     *LIKE         DEFINE    TAScts        SAVcts
385300941130     C     *LIKE         DEFINE    TASSV1        WSV1
385400941130     C     *LIKE         DEFINE    TASVA1        WVA1
385500941130     C     *LIKE         DEFINE    ARBDER        SAVDER
385600941130     C     *LIKE         DEFINE    CSBPAR        DESPAR
385700970916     C     *LIKE         DEFINE    CSBPAR        DESPRV
385800010126     C     *LIKE         DEFINE    arblnp        savlnp
385900010126     C     *like         define    tasftc        WTCP
386000010126     C     *like         define    tasftc        WsvTCP
386100941130     C*---------------------------------------------------------------*
386200970916     C                   TIME                    W0140            14 0
386300970916     C                   MOVEL     W0140         UTIME             6 0
386400970916     C                   MOVE      W0140         WDTGIO            8 0
386500970916     C* UDATE IN AAAAMMGG
386600970916     C                   Z-ADD     WDTGIO        G02DAT
386700970916     C                   MOVEL     *BLANK        G02ERR
386800970916     C                   CALL      'XSRDA8'
386900970916     C                   PARM                    WLBDAT
387000970916     C                   MOVEL     G02INV        DATEU             8 0
387100110208     C* SOTTRAGGO  1
387200110208     C     G02TGI        SUB       1             GIOTGI
387300110208     C                   CALL      'XSRGI8'
387400110208     C                   PARM                    WGIDAT
387500110208     C                   Z-ADD     GIOINV        DATAM1            8 0
387600110208     C* SOTTRAGGO  2
387700110208     C     G02TGI        SUB       2             GIOTGI
387800110208     C                   CALL      'XSRGI8'
387900110208     C                   PARM                    WGIDAT
388000110208     C                   Z-ADD     GIOINV        DATAM2            8 0
388100970916     C* SOTTRAGGO  3
388200970916     C     G02TGI        SUB       3             GIOTGI
388300970916     C                   CALL      'XSRGI8'
388400970916     C                   PARM                    WGIDAT
388500110208     C                   Z-ADD     GIOINV        DATAM3            8 0
388600941130     C*
388700941130     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
388800941130     C* ASSUMO IL PRIMO MEMBRO DEL FILE
388900941130     C     MBRFIL        IFEQ      *BLANK
389000020827     C                   MOVEL     'M888888888'  MBRFIL
389100941130     C                   END
389200941130     C* MEMBRO LUNGO 3
389300941130     C                   MOVEL     ALFFIL        MBR               3 0
389400941130     C* PRENDO ANNO IN CORSO
389500941130     C                   MOVE      UDATE         AAS               2 0
389600070119     c* Apro file bolle di filiale
389700070119     c                   clear                   tibs55ds
389800070119     c                   eval      i50apo=001
389900070119     c                   call      'TIBS55R'
390000070119     c                   parm                    tibs55ds
390100070119     c*
390200070119     c                   Clear                   commt
390300070119     c                   Movel(p)  cmdt(1)       commt
390400070119     c                   Eval      %Subst(commt:30:10) = o50ala
390500070119     c                   Eval      commt =
390600070119     c                             %trim(commt) + '/FNBLP01L)'
390700070119     c                   Call      'QCMDEXC'
390800070119     c                   Parm                    commt
390900070119     c                   Parm                    lenght
391000070119     c                   Open      FNBLP01L
391100070119     c
391200070119     c
391300071102      * Aggancio tabella VPO record DECO
391400120510     c**********         Clear                   Dvpodeco
391500120510     c***********        Clear                   Tibs02ds
391600120510     c***********        Eval      T02tla = 'L'
391700120510     c***********        Eval      T02Mod = 'C'
391800120510     c*************      Eval      T02Sif = knsif
391900120510     c*************      Eval      T02Cod = 'VPO'
392000120510     c***********        Movel(p)  'DECO'        T02Ke1
392100120510     c*******            Call      'TIBS02R'
392200120510     c*********          Parm                    Kpjba
392300120510     c*********          Parm                    Tibs02ds
392400120510     c*********          If        T02Err = *Blanks
392500120510     c*********          Movel     T02Uni        Dvpodeco
392600120510     c**********         EndIf
392700071102
392800941130     C                   ENDSR
392900930804     C*
393000981001     C**-------------------------------------------------------------
393100981001     C** AGGIORNO CON CODICI DI VARIAZIONE DM DD BOLLA SEDE MITT/DEST
393200981001     C**-------------------------------------------------------------
393300981001     C     AGTMD         BEGSR
393400981001     C* CONTROLLO CHE TIPO DI VARIAZIONE STO LEGGENDO
393500981001     C* DM CREAZIONE MITTENTE BOLLA SEDE
393600981001     C* DD CREAZIONE DESTINATARIO BOLLA SEDE
393700981001     C* I0 VARIAZIONE DESTINATARIO
393800981001     C* MI VARIAZIONE MITTENTE
393900981001     C*
394000981001    1C     ARBCVB        IFNE      'DM'
394100981001     C     ARBCVB        ANDNE     'DD'
394200981001     C     ARBCVB        ANDNE     'I0'
394300140407     C     ARBCVB        ANDNE     'I7'
394400140407     C     ARBCVB        ANDNE     'I8'
394500140407     C     ARBCVB        ANDNE     'I9'
394600981001     C     ARBCVB        ANDNE     'MI'
394700981001     C*
394800981001   X1C                   ELSE
394900981001     C* CERCO LA SPEDIZIONE NEL FILE TNTMD
395000981001     C     KANN          CHAIN     TNTMD01L                           48
395100981001     C* IMMISSIONE O CAMBIO MITTENTE
395200981001     C* CAUSALI DI VARIAZIONI
395300981001    2C     ARBCVB        IFEQ      'DM'
395400981001     C     ARBCVB        OREQ      'MI'
395500981001     C*
395600981001     C                   MOVEL     ARBRSM        TMDRSM
395700981001     C                   MOVEL     ARBINM        TMDINM
395800981001     C                   MOVEL     ARBCAM        TMDCAM
395900981001     C                   MOVEL     ARBLOM        TMDLOM
396000981001     C                   MOVEL     ARBPRM        TMDPRM
396100981001     C                   MOVEL     ARBNZM        TMDNZM
396200981001     C*
396300981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
396400981001    3C     *IN48         IFEQ      '1'
396500981005     C     ARBCVB        ANDEQ     'DM'
396600981001     C                   Z-ADD     ARBAAS        TMDAAS
396700981001     C                   Z-ADD     ARBLNP        TMDLNP
396800981001     C                   Z-ADD     ARBNRS        TMDNRS
396900981001     C                   Z-ADD     ARBNSP        TMDNSP
397000981001     C                   WRITE     TNTMD000
397100981001     C*
397200981005    3C                   ENDIF
397300981005     C* PER 48 SPENTO AGGIORNO IL RECORD
397400981005     C  N48              UPDATE    TNTMD000
397500981001     C*
397600981001    2C                   ENDIF
397700981001     C*
397800981001     C* IMMISSIONE O CAMBIO DESTINATARIO
397900981001     C* CAUSALI DI VARIAZIONI
398000981001    2C     ARBCVB        IFEQ      'DD'
398100981001     C     ARBCVB        OREQ      'I0'
398200140407     C     ARBCVB        OREQ      'I7'
398300140407     C     ARBCVB        OREQ      'I8'
398400140407     C     ARBCVB        OREQ      'I9'
398500981001     C*
398600981001     C                   MOVEL     ARBRSD        TMDRSD
398700981001     C                   MOVEL     ARBIND        TMDIND
398800981001     C                   MOVEL     ARBCAD        TMDCAD
398900981001     C                   MOVEL     ARBLOD        TMDLOD
399000981001     C                   MOVEL     ARBPRD        TMDPRD
399100981001     C                   MOVEL     ARBNZD        TMDNZD
399200981001     C*
399300981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
399400981001    3C     *IN48         IFEQ      '1'
399500981005     C     ARBCVB        ANDEQ     'DD'
399600981001     C                   Z-ADD     ARBAAS        TMDAAS
399700981001     C                   Z-ADD     ARBLNP        TMDLNP
399800981001     C                   Z-ADD     ARBNRS        TMDNRS
399900981001     C                   Z-ADD     ARBNSP        TMDNSP
400000981001     C                   WRITE     TNTMD000
400100981005    3C                   ENDIF
400200981005     C*
400300981001     C* PER 48 SPENTO AGGIORNO IL RECORD
400400981005     C  N48              UPDATE    TNTMD000
400500981001     C*
400600981001    2C                   ENDIF
400700981001     C*
400800981001     C* SE LA VARIAZIONE ARB E' DD O DM DELETO IL RECORD E NON PROSEGUO CON
400900981001     C* LA LETTURA
401000981001    2C     ARBCVB        IFEQ      'DM'
401100981001     C     ARBCVB        OREQ      'DD'
401200981001     C   11              DELETE    FNARBD00
401300981001     C   16              DELETE    FNARBM00
401400981001     C                   MOVE      'Y'           DMDD
401500981001    2C                   ENDIF
401600981001     C*
401700981001    1C                   ENDIF
401800981001     C*
401900981001     C                   ENDSR
402000120312     C**-------------------------------------------------------------
402100120515     C** RITASSAZIONE DELLA SPEDIZIONE CON I NUOVI DATI RICEVUTI
402200120312     C**-------------------------------------------------------------
402300120313     C     ctr_vrb       BEGSR
402400151008     c* Richiamo pgm memorizzazione variazioni post-fatturazione
402500151008     c                   clear                   fnlvv1ds
402600151008     c                   clear                   parvar
402700151008     c                   clear                   partitas
402800151008     c                   clear                   parcsb
402900151008     c                   eval      iv1cvb=comcvb
403000151008     c                   eval      iv1pgm=nompgm
403100151008     c                   select
403200151008     c                   when      %subst(iv1cvb:1:1)='I'
403300151008     c     nrri          chain     fnarbd0r      dsarbd
403400151008     c                   if        %found(fnarbd0r)
403500151008     c                   eval      parvar =dsarbd
403600151008     c                   endif
403700151008     c                   when      %subst(iv1cvb:1:1)='K'
403800151008     c     nrri          chain     fnarbk0r      dsarbk
403900151008     c                   if        %found(fnarbk0r)
404000151008     c                   eval      parvar =dsarbk
404100151008     c                   endif
404200151008     c                   if        wanncas='S'
404300151008     c                   eval      iv1can='S'
404400151008     c                   endif
404500151008     c                   eval      parcsb  = dscsb
404600151008     c                   when      %subst(iv1cvb:1:1)='T'
404700151008     c     nrri          chain     fiarbt0r      dsarbt
404800151008     c                   if        %found(fiarbt0r)
404900151008     c                   eval      parvar =dsarbt
405000151008     c                   endif
405100151008     c                   when      %subst(iv1cvb:1:1)='C'
405200151008     c     nrri          chain     fnarbg0r      dsarbg
405300151008     c                   if        %found(fnarbg0r)
405400151008     c                   eval      parvar =dsarbg
405500151008     c                   endif
405600151008     c                   endsl
405700151008     c                   eval      kpjbu=fnlvv1ds
405800151009     c                   eval      partitas=dstitas
405900151008     c                   call      'FNLVV1R'
406000151008     c                   parm                    kpjba
406100151008     c                   parm                    parvar
406200151008     c                   parm                    partitas
406300151008     c                   parm                    parcsb
406400120312     c                   endsr
406500121108      ****************************************************
406600121108      ** Memorizzazione tracciato della bolla di sede prima
406700121108      ** della variazione
406800121108      ****************************************************
406900121205     c     memo_dstitas  Begsr
407000121116     c                   eval      dstitas=titasds
407100121108     c                   endsr
407200121109      ****************************************************
407300121109      ** Memorizzazione dati del contrassegno prima
407400121109      ** della variazione
407500121109      ****************************************************
407600121109     c     memo_csb      Begsr
407700121109     c                   clear                   dscsb
407800121109     C     KANN          CHAIN(N)  TNCSB000
407900121109     c                   if        %found(tncsb03l)
408000121115     c                   eval      dscsb=csbds
408100121109     c                   endif
408200121109     c                   endsr
408300981001     C*
408400970814**         CMD1
408500020827CHKOBJ OBJ(FIARBR3C) OBJTYPE(*FILE) MBR(M234567890)
408600920207**         CM2
408700020827ALCOBJ OBJ((FLXXXX0R *FILE *EXCL       M234567890))
408800920212**         CM3
408900020827OVRDBF FILE(FLXXXXXX)               MBR(M234567890)
409000920113**         CM5
409100020827ADDLFM FILE(FIARBR3C) MBR(M234567890)   DTAMBRS((*CURRENT/FNARBK0R
409200020827 (M234567890)) (*CURRENT/FNARBD0R (M234567890)) (*CURRENT/FIARBT0R
409300020827 (M234567890)) (*CURRENT/FNARBG0R (M234567890)) (*CURRENT/FNARBM0R
409400020827 (M234567890)))
409500920113**
4096009201301 BOLLA  INESISTENTE                                                1
409700920130TIPO BOLLA INESISTENTE                                              2
409800920130CAUSALE VARIAZIONE INESISTENTE                                      3
409900960514NON ESISTE RECORD BOLLA IN TNAC200F                                 4
410000920130NON ESISTE ANAGRAFICA CONTRASSEGNI                                  5
410100920130NUOVO CODICE BOLLA INESISTENTE                                      6
4102009201302 BOLLA  INESISTENTE                                                7
410300920220SPEDIZIONE GIA' TASSATA                                             8
410400920212SPEDIZIONE GIA' CONTABILIZZATA                                      9
410500920210C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
410600950201MANCA RECORD FILE FNARBM0R                                          11
410700950201MANCA X CREAZ. ASSEGNATO FNARBD1R                                   12
410800930513X CREAR 2BOLLA MA NO VARIAZ CD.BOLL                                 15
410900920601GIA' CONTABIL.:NO TOLTA VARIA G                                     14
411000930513
411100920601GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 16
411200930513
411300920601GIA' CONTABIL.:NO AGGIUNTA VARIA G                                  18
411400930513
411500920601GIA' CONTABIL.:NO VARIATO L'A3                                      20
411600930513
411700920601GIA' CONTABIL.:NO MODIFIC. VARIA G                                  22
411800990414GIA' TASSATA  :NO TOLTA    VARIA G                                  23
411900920228GIA' TASSATA  :NO AGGIUNTA VARIA G                                  24
412000920228GIA' TASSATA  :NO MODIFIC. VARIA G                                  25
412100950203CAP E NAZIONE MITTENTE INESISTENTE                                  26
412200950203COD.MITTENTE INESISTENTE IN ANAGRAF                                 27
412300920302SPEDIZIONE ANNULLATA                                                28
412400960919COD.MITTENTE BLOCCATO                                               29
412500970814ERRORE IN DETERMINAZIONE COD.TASSAZ                                 30
412600990414AGGIUNTO   IMPORTO DA ASSICURARE                                    31
412700990414TOLTO      IMPORTO DA ASSICURARE                                    32
412800990414MODIFICATO IMPORTO DA ASSICURARE                                    33
412900990414GIA' CONTABIL.:NO TOLTA    VARIA R                                  34
413000990414GIA' CONTABIL.:NO AGGIUNTA VARIA R                                  35
413100990414GIA' CONTABIL.:NO MODIFIC. VARIA R                                  36
413200991022
413300991022
413400991022
413500991022AGGIUNTO IM.ASS. TP.BOLLA NON ASSIC                                 40
413600991022TOLTO    IM.ASS. TP.BOLLA NON ASSIC                                 41
413700991022MODIFIC. IM.ASS. TP.BOLLA NON ASSIC                                 42
413800070119** cmdt
413900070119OVRDBF FILE(FNBLP01L) TOFILE(
