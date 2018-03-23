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
116400170109     C*  ATTENDO L'ARRIVO DEL PDC
116500950203     C                   SETON                                        37
116600950202     C                   GOTO      ENDGBM
116700960919     C                   ELSE
116800960919     C* SE CLIENTE BLOCCATO CARICO LO STESSO MA SEGNALO
116900100303     C***  ACOABL        IFEQ      '8'
117000100303     C***                MOVEL     ERR(29)       ARBDER
117100100303     C***                EXSR      WTRERR
117200100303     C***                END
117300950202     C                   ENDIF
117400950202     C                   ENDIF
117500950203     C*
117600950203     C* VERIFICO ESISTENZA NAZIONE E PROVINCIA MITTENTE
117700970714     C                   CLEAR                   DSSI95
117800970714     C                   MOVEL     ARBCAM        I95CAP
117900121116     C                   MOVEL     ARBNZM        I95NAR
118000121116     c                   movel     tasaas        i95dat
118100121116     c                   move      tasmgs        i95dat
118200121116     C****               MOVEL     TASDSP        I95DAT
118300970714     C                   MOVEL     '3'           I95TCN
118400020311      * Se network FedEx passo flag apposito per dire di prendere i dati
118500020311      * dalla tabella della nazione
118600020311     c     arblnp        chain     azorg01l
118700020311     c                   if        %found(azorg01l) and orgfva = *blanks
118800020311     c                   eval      og143 = orgde3
118900020311     c     §ogntw        ifeq      'FED'
119000020311     c                   eval      i95fi1 = 'S'
119100020311     c                   endif
119200020311     c                   endif
119300020311
119400970714     C                   CALL      'TISI95R'
119500970714     C                   PARM                    DSSI95
119600950203     C*
119700970714     C     O95ERR        IFNE      *BLANKS
119800130326     C                   Z-ADD     102           COMFEV
119900950203     C                   MOVEL     ERR(26)       ARBDER
120000950203     C                   EXSR      WTRERR
120100950203     C                   GOTO      ENDGBM
120200950203     C                   ENDIF
120300950203     C*
120400950203     C* VERIFICO SE ESISTEN L'ANAGRAFICA DEI C/A E SE GIA' INCASSATO
120500950222    1C     §3AFCA        IFNE      0
120600950202     C                   EXSR      CONCSB
120700950222     C* INESISTENTE  --> NON AGGIORNO
120800950222     C   25              GOTO      ENDGBM
120900950203     C***
121000950203     C*** A G G I O R N O   C / A S S E G N O
121100950222     C* AGGIORNO SOLO SE NON E' INCASSATO
121200950222   1AC     *IN31         IFEQ      *OFF
121300950202     C     KANN          CHAIN     TNCSA000                           30
121400950202     C*
121500950202     C* NON E' 8888 :SE CREATA CANCELLO ANAGRAFICA C/A
121600950202IF  2C     KSC4          IFNE      8888
121700970916IF  2C     KSC4          ANDNE     9999
121800950202     C  N30              DELETE    TNCSA000
121900950202X   2C                   ELSE
122000950202     C                   CLEAR                   TNCSA000
122100950202     C*
122200950202     C                   MOVEL     CSBAAS        CSAAAS
122300950202     C                   MOVEL     CSBLNP        CSALNP
122400950202     C                   MOVEL     CSBNRS        CSANRS
122500950202     C                   MOVEL     CSBNSP        CSANSP
122600950202     C                   MOVEL     ARBRSM        CSARSD
122700950202     C                   MOVEL     ARBINM        CSAVID
122800950202     C                   MOVEL     ARBCAM        CSACAD
122900950202     C                   MOVEL     ARBLOM        CSACID
123000950202     C                   MOVEL     ARBPRM        CSAPRM
123100950202     C                   MOVEL     ARBNZM        CSANZM
123200950202     C*
123300950202     C  N30              UPDATE    TNCSA000
123400950202     C   30              WRITE     TNCSA000
123500950202E   2C                   ENDIF
123600950202     C*
123700950202     C                   MOVEL     ARBCCM        CSBCDI
123800950202     C*
123900950202     C* SE C/A SU 1 BOLLA IMPOSTO CODICE BOLLA AMCHE
124000950202    2C     §3AFCA        IFEQ      1
124100950202     C                   MOVEL     ARBCCM        CSBKSC
124200950202    2C                   ENDIF
124300950202     C*
124400950202     C                   UPDATE    TNCSB000
124500950222   1AC                   ENDIF
124600950222    1C                   ENDIF
124700950202     C*
124800950203     C* A G G I O R N O   L A   1  B O L L A
124900970714     C                   MOVEL     O95CTS        TASMCT
125000950202     C                   MOVEL     ARBCAM        TASCAM
125100950202     C                   MOVEL     ARBNZM        TASNZM
125200950202     C                   MOVEL     ARBFAP        TASFAP
125300950202     C                   MOVEL     ARBCCM        TASKSC
125400990628     C                   MOVEL     ARBCCM        TASccm
125500950202     C*
125600950202     C*
125700990628     c                   movel     'M'           §trc
125800990629     c     ktita         chain     titaa000                           30
125900950202IF  3C     KSC4          IFEQ      8888
126000990628     c   30              z-add     tasaas        taaaas
126100990628     c   30              z-add     taslnp        taalnp
126200990628     c   30              z-add     tasnrs        taanrs
126300990628     c   30              z-add     tasnsp        taansp
126400990628     c   30              movel     §trc          taatrc
126500990628     C                   MOVEL     ARBRSM        TAaRSc
126600990628     C                   MOVEL     ARBINM        TAaIND
126700990628     C                   MOVEL     ARBCAM        TAaCAP
126800990628     C                   MOVEL     ARBLOM        TAaLOc
126900990628     C                   MOVEL     ARBPRM        TAaPRV
127000990628     C                   MOVEL     ARBNZM        TAaNAZ
127100990628     C                   MOVEL     ARBCPI        TAacpi
127200990628     c  n30              update    titaa000
127300990628     c   30              write     titaa000
127400990628     c                   else
127500990628     c* se prima della variazione era un 8888 lo vado a cancellare da
127600990628     c* titaa00f
127700990628     c  n30              delete    titaa000
127800990628     c
127900950202E   3C                   ENDIF
128000950202     C*
128100990628     C   02              UPDATE    TiTAS000
128200071029     C***
128300950203     C*** A G G I O R N O   2   B O L L A
128400950202    3C     §3ATB2        IFNE      '  '
128500950202     C                   MOVEL     §3ATB2        COMTBL
128600950202     C                   SETOFF                                       020304
128700990628     C     KBLP          CHAIN     TiTAS30C                           39
128800950202     C*
128900950203     C     *IN39         IFEQ      *OFF
129000950202     C                   MOVEL     ARBCCM        TASCCM
129100950202     C*
129200950202     C                   MOVEL     ARBCAM        TASCAM
129300950202     C                   MOVEL     ARBNZM        TASNZM
129400950202     C                   MOVEL     ARBFAP        TASFAP
129500970714     C                   MOVEL     O95CTS        TASMCT
129600950202     C*
129700990628     C   02              UPDATE    TiTAS000
129800990628     C   03              UPDATE    TiTAS010
129900990628     C   04              UPDATE    TiTASP00
130000950202     C                   ENDIF
130100950202     C                   ENDIF
130200950202     C*
130300950202     C     ENDGBM        ENDSR
130400950202     C**---------------------------------------------------------
130500950202     C** AGGIORNO CODICE VARIAZIONI CX
130600950202     C**---------------------------------------------------------
130700950202     C     AGARBC        BEGSR
130800010126     C     arblnp        ifne      SAVLNP
130900010126     C     arblnp        chain     azorg01l                           32
131000010126     C  n32              movel     orgde3        og143
131100010126     C   32              clear                   og143
131200010126     C                   z-add     arblnp        savlnp
131300010126     C                   endif
131400010126     C* SE BOLLA POSTE ED AGGIUNTO O TOLTO IL FUORI MISURA BISOGNA
131500010126     C* AGGIORNARE tita4 TIPO RECORD "J"
131600010126     C     §ogpt         ifeq      'S'
131700010126     C                   clear                   wtcp
131800010126     C                   clear                   wsvtcp
131900010126    2C     arbtc1        ifeq      'F'
132000010126     C     arbtc2        oreq      'F'
132100010126     C                   movel     'F'           wtcp
132200010126    2C                   endif
132300010126    2C     tasftc        ifeq      'F'
132400010126     C     tastc2        oreq      'F'
132500010126     C                   movel     'F'           wsvtcp
132600010126    2C                   endif
132700010126    2C                   select
132800010126     C* SE NON E' STATA NE' AGGIUNTA NE' TOLTA LA "F" NON FACCIO NIENTE
132900010126     C     arbtc1        whenne    'F'
133000010126     C     arbtc2        andne     'F'
133100010126     C     tasftc        andne     'F'
133200010126     C     tastc2        andne     'F'
133300010126     C     wtcp          oreq      'F'
133400010126     C     wsvtcp        andeq     'F'
133500010126     C                   other
133600010126     C                   movel     'J'           §trc
133700010126     c                   setoff                                       0506
133800010126     C     ktita         chain     tita430c                           32
133900010126    3C     *in32         ifeq      *off
134000010126     C                   movel     ta4not        dsbl4j
134100010126     C* TOLTO LA 'F'
134200010126    4C     wtcp          ifeq      *blanks
134300010126     C     wsvtcp        andeq     'F'
134400010126     C                   clear                   §b4bai
134500010126   X4C                   else
134600010126     C* AGGIUNTA 'F'
134700010126    5C     wtcp          ifeq      'F'
134800010126     C     wsvtcp        andeq     *blanks
134900010126     C                   movel     'S'           §b4bai
135000010126    5C                   endif
135100010126    4C                   endif
135200010126     C                   movel     dsbl4j        ta4not
135300010126     C   05              update    tita4000
135400010126     C   06              update    tita4p00
135500010126   X3C                   else
135600010126     C* AGGIUNTA 'F' --> CREO RECORD "J"
135700010126    4C     wtcp          ifeq      'F'
135800010126     C     wsvtcp        andeq     *blanks
135900010126     C                   clear                   tita4000
136000010126     C                   z-add     tasaas        ta4AAS
136100010126     C                   z-add     taslnp        ta4LNP
136200010126     C                   z-add     tasnrs        ta4NRS
136300010126     C                   z-add     tasnsp        ta4NSP
136400010126     C                   movel     'J'           ta4TRC
136500010126     C                   clear                   dsbl4j
136600010126     C                   clear                   ta4not
136700010126     C                   movel     'S'           §B4BAI
136800010126     C                   movel     dsbl4j        ta4NOT
136900010126     C  N04              write     tita4000
137000010126     C   04              write     tita4p00
137100010126    4C                   endif
137200010126    3C                   endif
137300010126    2C                   endsl
137400010126     C                   endif
137500950202     C* AGGIORNO SEMPRE  LE CONSEGNE PARTICOLARI SOLO SU  1 BOLLA
137600991018     c* idem per gg chiusura e data consegna richiesta
137700950202     C                   MOVEL     ARBTC1        TASFTC
137800950202     C                   MOVEL     ARBTC2        TASTC2
137900991018     c                   movel     arbtcr        tastcr
138000991018     c                   z-add     arbdcr        tasdcr
138100991018     c                   z-add     arbhcr        tashcr
138200991018     C                   MOVEL     arbgC1        tasgc1
138300991018     C                   MOVEL     arbgC2        tasgc2
138400990628     C   02              UPDATE    TiTAS000
138500990628     C   03              UPDATE    TiTAS010
138600990628     C   04              UPDATE    TiTASP00
138700950202     C                   ENDSR
138800950202     C*
138900920113     C**---------------------------------------------------------
139000920113     C** AGGIORNO CODICE VARIAZIONI TX
139100920113     C**---------------------------------------------------------
139200920113     C     AGARBT        BEGSR
139300920113     C*
139400920226     C                   EXSR      CTR2
139500920226     C*
139600941130    1C  N29              DO
139700950202     C*
139800991102     c
139900990419     c* controllo l'importo da assicurare se la spedizione e' tassata
140000990419     c*  o con tot.imponibile
140100011029     c                   if        tasias > 0  and tasvas = *blanks
140200011029     c                   movel     'ITL'         tasvas
140300011029     c                   endif
140400990419     c                   if        arbias <> tasias or
140500990419     c                             arbvas <> tasvas
140600990419     c*
140700990419     c                   exsr      AGGIAS
140800990419     c                   endif
140900950202     C*
141000950202     C                   Z-ADD     ARBIAS        TASIAS
141100950202     C                   MOVEL     ARBVAS        TASVAS
141200990824     c                   clear                   TASFCM
141300920113     C*
141400941130    2C     §3ATB2        IFEQ      '  '
141500970514     C     COMCVB        OREQ      'TP'
141600950222     C* AGGIORNO KSC SOLO SE VARIAZIONE DA ARRIVO E NON DA PARTENZA
141700950222     C*  PERCHE' SI TRATTA DI VARIAZIONE DI MITTENTE E NON DI TASSAZION
141800950222     C     ARBCVB        IFNE      'TP'
141900920113     C                   Z-ADD     ARBKSC        TASKSC
142000950222     C                   ENDIF
142100051219     c* Non vado sopra al codice tariffa se bolla in assegnato tassata con
142200051219     c* tariffa mittente
142300051219     c     tasftm        ifeq      *blanks
142400051219     C     comcvb        oreq      'TP'
142500950222     C                   MOVEL     ARBCTR        TASCTR
142600051219     c                   endif
142700950202     C                   Z-ADD     ARBQFT        TASQFT
142800991015     c                   MOVEL     ARBDIV        TASDIV
142900920113     C                   Z-ADD     ARBPOR        TASPOR
143000920113     C                   MOVEL     ARBSV1        TASSV1
143100920113     C                   Z-ADD     ARBVA1        TASVA1
143200920113     C                   MOVEL     ARBSV2        TASSV2
143300941130     C                   Z-ADD     ARBVA2        TASVA2
143400950202     C                   MOVEL     ARBSV3        TASSV3
143500950202     C                   Z-ADD     ARBVA3        TASVA3
143600941130     C*
143700990628     C* SE ESITONO GIA' DELLE VARIE IN TITA700F LE CANCELLO
143800990628     C     KBLP          SETLL     TITA7000
143900990628     C     KBLP          READE     TITA7000                               30
144000941130     C*
144100941130    3C     *IN30         DOWEQ     *OFF
144200991015     C                   DELETE    TITA7000
144300991015     C     KBLP          READE     TITA7000                               30
144400941130    3C                   ENDDO
144500991015     C     KARBU         SETLL     FIARBU1R
144600991015     C     KARBU         READE     FIARBU1R                               30
144700950202     C*
144800950202     C     *IN30         DOWEQ     *OFF
144900990628     C                   CLEAR                   TiTA7000
145000950202     C                   Z-ADD     ARBAAS        TA7AAS
145100950202     C                   Z-ADD     ARBLNP        TA7LNP
145200950202     C                   Z-ADD     ARBNRS        TA7NRS
145300950202     C                   Z-ADD     ARBNSP        TA7NSP
145400950202     C                   MOVEL     TASTBL        TA7TBL
145500950202     C                   MOVEL     ARBSVN        TA7SVN
145600950202     C                   Z-ADD     ARBVAN        TA7VAN
145700990629     C                   WRITE     TITA7000
145800950202     C*
145900950202     C                   ADD       1             NUMRBU            7 0
146000991015     C                   DELETE    FIARBURR
146100950202     C*
146200991015     C     KARBU         READE     FIARBU1R                               30
146300950202     C                   ENDDO
146400990628     c
146500941130     C*
146600920113     C                   Z-ADD     ARBDFT        TASDFT
146700920113     C                   Z-ADD     ARBNFT        TASNFT
146800920113     C                   Z-ADD     ARBFIV        TASFIV
146900941130     C                   MOVEL     ARBCEI        TASFEI
147000920113     C*
147100990624     C                   z-add     arbimv        TASimv
147200151120     c* Totale imponibile = 0 --> Cancello record FAT se presente e imposto fpf='R'
147300151120    3c                   if        tasimv=0
147400151120     c                   Eval      kar5trd='FAT'
147500151120     c     kar5          Chain     Fiar531c
147600151120     c                   if        %found(fiar531c)
147700151120     c   02              delete    fiar5000
147800151120     c                   endif
147900151118     c                   eval      tasfpf='R'
148000151120   x3c                   else
148100151120     c* Totale imponibile > 0 --> Scrivo record FAT se manca imposto in base a record FAT
148200151120     c*                           Imposto TASFPF in base al record FAT
148300151118     c                   clear                   dar5fat
148400151118     c                   Eval      kar5trd='FAT'
148500151118     c     kar5          Chain     Fiar531c
148600151120    4c                   If        %Found(Fiar531c)
148700151118     c                   Movel     ar5uni        dar5fat
148800151120   x4c                   else
148900151120     c                   Eval      Ar5Aas = ArbAas
149000151120     c                   Eval      Ar5Lnp = ArbLnp
149100151120     c                   Eval      Ar5Nrs = ArbNrs
149200151120     c                   Eval      Ar5Nsp = ArbNsp
149300151120     c                   Eval      Ar5Trd = 'FAT'
149400151120     c                   movel     tasvlf        §ar5vltas
149500151120     c                   movel     tasfvf        §ar5fvtas
149600151120     c                   movel     taspkf        §ar5pktas
149700151120     c                   movel     'R'           §ar5fptas
149800151120     c                   eval      §AR5DATAS=%dec(%date())
149900151120     c                   eval      §AR5HMTAS=%dec(%time())
150000151120     c                   eval      §AR5PRTAS=knmus
150100151120     c                   eval      §AR5NJTAS=knmtd
150200151120     c                   Movel(p)  dar5fat       Ar5uni
150300151120     c                   eval      ar5dac=%dec(%date())
150400151120     c                   eval      ar5orc=%dec(%time())
150500151120     c                   Eval      AR5FT1='T'
150600151120     c                   Eval      AR5DT1=99999999
150700151120     c                   Eval      AR5FT2='T'
150800151120     c                   Eval      AR5DT2=99999999
150900151120     c                   Eval      AR5FT3='T'
151000151120     c                   Eval      AR5DT3=99999999
151100151120     c   02              write     fiar5000
151200151120    4c                   endif
151300151120     c* Imposto TASFPF
151400151118     c                   select
151500151118     c                   when      §AR5FPTAS='R'
151600151118     c                   eval      tasfpf='R'
151700151118     c                   when      §AR5FPTAS='D'
151800151118     c                   eval      tasfpf='D'
151900151118     c                   other
152000151118     c                   eval      tasfpf='V'
152100151118     c                   endsl
152200151120    3c                   endif
152300941130    3C     ARBIFT        IFGT      0
152400941130     C                   MOVEL     '1'           TASFTS
152500920113     C                   ELSE
152600941130     C                   MOVEL     ' '           TASFTS
152700941130    3C                   END
152800941130    2C                   END
152900920113     C* AGGIORNO
153000990628     C   02              UPDATE    TiTAS000
153100941130    1C                   ENDDO
153200920113     C*
153300920113     C                   ENDSR
153400920113     C**---------------------------------------------------------
153500920113     C** AGGIORNO CODICE VARIAZIONI 2T
153600920113     C**---------------------------------------------------------
153700920113     C     AGAR2T        BEGSR
153800920113     C*
153900920226     C* SECONDA BOLLA
154000920113     C     §3ATB2        IFNE      '  '
154100920226     C                   EXSR      CTR2
154200920114     C*
154300920226     C  N29              DO
154400991015     C                   Z-ADD     ARBKSC        TASKSC
154500991015     C                   MOVEL     ARBCTR        TASCTR
154600991015     C                   movel     arbdiv        tasdiv
154700991015     C                   MOVEL     ARBSV1        TASSV1
154800991015     C                   Z-ADD     ARBVA1        TASVA1
154900991015     C                   MOVEL     ARBSV2        TASSV2
155000991015     C                   Z-ADD     ARBVA2        TASVA2
155100040114     c                   Movel     ArbSv3        TasSv3
155200040114     c                   Z-Add     ArbVa3        TasVa3
155300040114      * Se esistono già della varie su Tita7 le devo eliminare
155400040114     c     Kblp          Setll     Tita7000
155500040114     c                   Do        *Hival
155600040114     c     Kblp          Reade     Tita7000
155700040114     c                   If        %Eof
155800040114     c                   Leave
155900040114     c                   EndIf
156000040114     c                   Delete    Tita7000
156100040114     c                   EndDo
156200040114      * Scrivo il nuovo Tita7 e cancello il file di ricezione
156300040114     c     Karbu         Setll     Fiarbu1r
156400040114     c                   Do        *Hival
156500040114     c     Karbu         Reade     Fiarbu1r
156600040114     c                   If        %Eof(Fiarbu1r)
156700040114     c                   Leave
156800040114     c                   EndIf
156900040114     c                   Clear                   Tita7000
157000040114     c                   Z-add     ArbAas        Ta7Aas
157100040114     c                   Z-add     ArbLnp        Ta7Lnp
157200040114     c                   Z-add     ArbNrs        Ta7Nrs
157300040114     c                   Z-add     ArbNsp        Ta7Nsp
157400040114     c                   Movel     TasTbl        Ta7Tbl
157500040114     c                   Movel     ArbSvn        Ta7Svn
157600040114     c                   Z-add     ArbVan        Ta7Van
157700040114     c                   Write     Tita7000
157800040114     c                   Add       1             NumRbu
157900040114     c                   Delete    Fiarburr
158000040114     c                   EndDo
158100040114
158200991015     C                   Z-ADD     ARBDFT        TASDFT
158300991015     C                   Z-ADD     ARBNFT        TASNFT
158400991015     C                   Z-ADD     ARBFIV        TASFIV
158500991015     C                   MOVEL     ARBCEI        TASFEI
158600040114     C**!!!              z-add     arbva1        TASimv
158700040114     c                   Z-add     ArbImv        TasImv
158800991015     C     ARBIFT        IFGT      0
158900941130     C                   MOVEL     '1'           TASFTS
159000920114     C                   ELSE
159100941130     C                   MOVEL     ' '           TASFTS
159200920114     C                   END
159300920113     C*
159400040114      * Aggiorno Titas
159500990628     C   02              UPDATE    TiTAS000
159600920113     C                   END
159700920130     C                   END
159800920113     C*
159900920113     C                   ENDSR
160000920127     C**---------------------------------------------------------
160100920127     C** AGGIORNO CODICE VARIAZIONI KX
160200920127     C**---------------------------------------------------------
160300920127     C     AGARBK        BEGSR
160400121109     c                   if        *in11='0' and *in16='0'
160500121109     c                   exsr      memo_csb
160600121109     c                   endif
160700121205     c                   clear                   wanncas
160800920212     C                   SETOFF                                       31
160900941130     C                   CLEAR                   TNCSB000
161000950202     C* NON AGGIORNO SE HO LETTO
161100950202    1C     *IN11         IFEQ      *OFF
161200950202     C     *IN16         ANDEQ     *OFF
161300920127     C* VARIA CONTRASSEGNO
161400941130    2C     ARBCBP        IFEQ      ARBCBN
161500920127     C* VEDO CHE TIPO BOLLA AGGANCIARE DAL FLAG C/ASSEGNO
161600920131     C* RIAGGANCIO SOLO LA SECONDA PERCHE' LA PRIMA L'HO GIA' AGGANCIAT
161700941130    3C     §3AFCA        IFEQ      2
161800920127     C                   MOVEL     §3ATB2        COMTBL
161900941130     C                   SETOFF                                       020304
162000990625     C     KBLP          CHAIN     TITAS30C                           39
162100121108     c                   exsr      memo_dstitas
162200941130    3C                   END
162300920228     C*
162400920228     C* CONTROLLO SPEDIZIONE SE OK
162500990414     C                   EXSR      CTR3
162600920210     C*
162700920212     C* CONTROLLO ARCHIVIO C/ASSEGNO SE ERA PREVISTO
162800941130    3C     §3AFCA        IFNE      0
162900920212     C                   EXSR      CONCSB
163000941130    3C                   END
163100920210     C* SE NO ERRORE
163200950324    3C  N31CSBDDC        IFEQ      0
163300950324     C     CSBBNA        ANDEQ     0
163400941130     C                   MOVEL     *BLANKS       ARBDER
163500941130     C*
163600920228     C*** AGGIORNO CONTRASSEGNO
163700920228     C*** PRIMA     ESISTEVA ORA     ESISTE: V A R I A Z I O N E
163800941130    4C     CSBCAS        IFNE      0
163900950202     C     ARBCAS        ANDNE     0
164000950202     C* SCRIVO SOLO SE IMPORTO DIVERSO
164100950202    5C     ARBCAS        IFNE      CSBCAS
164200991103    5C     ARBVCA        ORNE      WCSBVCA
164300000703     c*
164400000703     c     csbsta        oreq      9
164500920212     C* SCRIVO STORICO
164600920212     C                   MOVEL     'VACA'        CSVCAV
164700920212     C                   EXSR      CSVWTR
164800950202     C*
164900950202     C                   EXSR      AGGCSB
165000950202     C*
165100941130     C                   UPDATE    TNCSB000
165200920228     C*
165300120420     C* E R R O R E : NON MODIFICABILE IMPORTO VARIA G
165400130325     C***29              MOVEL     ERR(22)       ARBDER
165500120322     C   29              exsr      ctr_vrb
165600941130    5C                   END
165700941130    4C                   END
165800920228     C***
165900920228     C*** PRIMA     ESISTEVA ORA NON ESISTE: A N N U L L O
166000941130    4C     CSBCAS        IFNE      0
166100950202     C     ARBCAS        ANDEQ     0
166200920212     C                   EXSR      ANNCSB
166300920226     C*
166400920228     C* E R R O R E : NON ELIMINABILE IMPORTO VARIA G
166500920316     C                   MOVEL     *BLANKS       ARBDER
166600990505    5C   29              DO
166700941130     C* CONTROLLO SE ESISTE VARIA "G"
166800130326     C****               EXSR      ESVARG
166900130325    6C***  WSIVAR        IFEQ      'S'
167000130325     C***                MOVEL     ERR(14)       ARBDER
167100130325    6C***                END
167200121205     c                   eval      wanncas='S'
167300121203     c                   exsr      ctr_vrb
167400941130    5C                   END
167500941130    4C                   END
167600920228     C***
167700920228     C*** PRIMA NON ESISTEVA ORA     ESISTE: C R E O  RECORD C/ASSEGNO
167800941130    4C     CSBCAS        IFEQ      0
167900950202     C     ARBCAS        ANDNE     0
168000920212     C                   MOVEL     §3ATB1        TBLCA
168100941130     C                   MOVEL     §3ATB1        FLGTB1
168200941130     C*
168300941130     C                   MOVEL     TASLOD        DESPAR
168400990625     C                   MOVEL     TASPRD        DESPRV
168500950913     C*
168600920212     C                   EXSR      CRTCSB
168700920228     C* E R R O R E : NON POSSO AGGIUNGERE IMPORTO VARIA G
168800130325     C***29              MOVEL     ERR(18)       ARBDER
168900130325     c***29              setoff                                       36
169000120322     C   29              exsr      ctr_vrb
169100941130    4C                   END
169200920228     C***
169300920228     C*** AGGIORNO SPEDIZIONE
169400941130     C                   MOVEL     'S'           TASFCA
169500920228     C* SE GIA' TASSATA METTO A POSTO PER VARIA G
169600110203     c                   if        *in38
169700110203     c                   Z-ADD     arbcas        wcas
169800110203     C                   EXSR      VARIAG
169900110203     c
170000110203      * riorganizzo le varie se è necessario
170100110203     c                   if        flgriorg = '1'
170200110203     c                   exsr      sr_riorg
170300110203     c                   endif
170400110203     c                   endif
170500110203
170600950202     C* SCRIVO ERRORE SE GIA' CONTABILIZZATA/STORICIZZATA
170700941130    4C     ARBDER        IFNE      *BLANKS
170800920309     C                   EXSR      WTRERR
170900941130    4C                   END
171000920228     C*
171100990625     C   02              UPDATE    TITAS000
171200990625     C   03              UPDATE    TITAS010
171300990625     C   04              UPDATE    TITASP00
171400941130    3C                   END
171500920127     C*
171600941130   X2C                   ELSE
171700920228     C******
171800920228     C****** M O D I F I C A T O   T I P O   B O L L A
171900920228     C******
172000920212     C                   SETOFF                                       31
172100920127     C                   MOVEL     §3ATB1        SAVTB1            2
172200920127     C                   MOVEL     §3ATB2        SAVTB2            2
172300920227     C                   MOVEL     §3AFCA        SAVFCA            1 0
172400920227     C                   MOVEL     §3AFTD        SAVFTD            1 0
172500941130    3C     §3ABCM        IFNE      ' '
172600920324     C                   MOVEL     0             SAVFCA
172700941130    3C                   END
172800920127     C* NUOVO TIPO BOLLA
172900920127     C                   MOVEL     '3A'          COD
173000941130     C                   MOVEL(P)  ARBCBN        KEY
173100920212     C     KTAB          CHAIN     TABEL                              34
173200941130     C*
173300941130     C* COD BOLLA NUOVO NON TROVATO
173400941130    3C     *IN34         IFEQ      *ON
173500941130     C                   MOVEL     ERR(6)        ARBDER
173600920212     C*
173700941130     C                   SETON                                        37
173800941130     C                   Z-ADD     102           COMFEV
173900941130     C                   EXSR      WTRERR
174000941130     C**
174100941130   X3C                   ELSE
174200941130     C**
174300941130     C* COD BOLLA NUOVO TROVATO
174400920131     C                   MOVEL     TBLUNI        DS3A
174500941130    4C     §3ABCM        IFNE      ' '
174600920324     C                   Z-ADD     0             §3AFCA
174700941130    4C                   END
174800920131     C*
174900920212     C* ANNULLAMENTO CONTRASSEGNO
175000941130    4C     SAVFCA        IFNE      0
175100920212     C     §3AFCA        ANDEQ     0
175200920212     C                   EXSR      CONCSB
175300920212     C*
175400941209    5C     *IN31         IFEQ      *OFF
175500941209     C* IMPOSTO IL NUOVO TIPO BOLLA
175600941209    6C     SAVFCA        IFEQ      2
175700941209     C     §3ATB2        ANDNE     *BLANKS
175800941209     C                   MOVEL     §3ATB2        CSBTBL
175900941209     C                   ELSE
176000941209     C                   MOVEL     §3ATB1        CSBTBL
176100941209    6C                   ENDIF
176200941209     C*
176300941209     C                   EXSR      ANNCSB
176400941209    5C                   END
176500941209    4C                   END
176600950202     C***************
176700920212     C* SE C/A PASSA A  "F"  A  "A"  O VICEVERSA POSSO FARLO ANCHE SE
176800920212     C*  INCASSATO - BASTA CHE NON SIA MODIFICATO L'IMPORTO
176900941130    4C     SAVFCA        IFEQ      1
177000920212     C     §3AFCA        ANDEQ     2
177100920212     C                   EXSR      CONCSB
177200950630     C* Se da bolla singola a bolla doppia e C/A sulla 2 bolla non
177300950630     C* aggiorno adesso TNCSB ma solo dopo scrittura della 2 bolla su
177400950630     C* TNTAS
177500950630    5C     SAVTB2        IFEQ      '  '
177600950630     C     §3ATB2        ANDNE     '  '
177700950630     C                   UNLOCK    TNCSB03L                             99
177800950630     C                   ELSE
177900941130     C* ANAGRAFICA C/A TROVATA
178000950630    6C     *IN31         IFEQ      *OFF
178100941130     C* CREO STORICO SE MODIFICATO IMPORTO
178200950630    7C     CSBCAS        IFNE      ARBCAS
178300991103    7C     WCSBVCA       ORNE      ARBVCA
178400920212     C                   MOVEL     'VACA'        CSVCAV
178500920212     C                   EXSR      CSVWTR
178600950630    7C                   END
178700920212     C*
178800941130     C                   Z-ADD     9999          CSBKSC
178900941130     C                   MOVEL     TASLNA        CSBKSC
179000950202     C                   MOVEL     §3ATB2        CSBTBL
179100950202     C*
179200950202     C                   EXSR      AGGCSB
179300941130     C                   UPDATE    TNCSB000
179400950630    6C                   END
179500950630    5C                   END
179600941130    4C                   END
179700950202     C***************
179800941130    4C     SAVFCA        IFEQ      2
179900920212     C     §3AFCA        ANDEQ     1
180000920228     C                   EXSR      CHTB2
180100920212     C*
180200920228     C                   EXSR      CONCSB
180300941130     C*
180400941130    5C     *IN31         IFEQ      *OFF
180500920212     C* CREO STORICO SE MODIFICATO IMPORTO
180600950202    6C     ARBCAS        IFNE      CSBCAS
180700991102    6C     ARBVCA        ORNE      WCSBVCA
180800920212     C                   MOVEL     'VACA'        CSVCAV
180900920212     C                   EXSR      CSVWTR
181000941130    6C                   END
181100920212     C*
181200941130     C                   MOVEL     CSBCDI        CSBKSC
181300941130     C                   MOVEL     §3ATB1        CSBTBL
181400950202     C                   EXSR      AGGCSB
181500941130     C                   UPDATE    TNCSB000
181600941130    5C                   END
181700941130    4C                   END
181800950202     C***************
181900950202     C* MODIFICATO IMPORTO C/ASSEGNO
182000941130    4C     SAVFCA        IFEQ      1
182100920227     C     §3AFCA        ANDEQ     1
182200941130     C* CONTROLLO IL C/A
182300920227     C                   EXSR      CONCSB
182400941130     C*
182500950202    5C  N31CSBCAS        IFNE      ARBCAS
182600991103    5C     WCSBVCA       ORNE      ARBVCA
182700920227     C                   MOVEL     'VACA'        CSVCAV
182800920227     C                   EXSR      CSVWTR
182900941209    5C                   END
183000950202     C*
183100950202     C                   EXSR      AGGCSB
183200950202     C*
183300941209     C* IMPOSTO IL NUOVO TIPO BOLLA
183400941209     C                   MOVEL     §3ATB1        CSBTBL
183500941209     C  N31              UPDATE    TNCSB000
183600941130    4C                   END
183700950202     C***************
183800941130    4C     SAVFCA        IFEQ      2
183900920227     C     §3AFCA        ANDEQ     2
184000920228     C                   EXSR      CHTB2
184100920227     C*
184200920227     C                   EXSR      CONCSB
184300941130     C*
184400950202    5C  N31CSBCAS        IFNE      ARBCAS
184500991103    5C     WCSBVCA       ORNE      ARBVCA
184600920227     C                   MOVEL     'VACA'        CSVCAV
184700920227     C                   EXSR      CSVWTR
184800941209    5C                   END
184900950202     C*
185000950202     C                   EXSR      AGGCSB
185100950202     C*
185200941209     C* IMPOSTO IL NUOVO TIPO BOLLA
185300941209     C                   MOVEL     §3ATB2        CSBTBL
185400941209     C  N31              UPDATE    TNCSB000
185500941130    4C                   END
185600920212     C***
185700920227     C*** A  G  G  I  O  R  N  O       B  O  L  L  A
185800920212     C***
185900080408     c  N31              if        VariataSede=' '
186000941130     C                   CLEAR                   DESPAR
186100970916     C                   CLEAR                   DESPRV
186200920228     C***
186300920228     C*** DA BOLLA SINGOLA A BOLLA SINGOLA
186400941130    5C     SAVTB2        IFEQ      '  '
186500920227     C     §3ATB2        ANDEQ     '  '
186600110315     c
186700110315     c* Non lo faccio se devo aggiungere solo il C/Ass perchè la bolla
186800110315     c*  non la trovo col cod bolla vecchio ma col cod bolla nuovo
186900110315    6c                   if        SoloCAss=' '
187000110315     c
187100941130     C                   MOVEL     'N'           VARFCA            1
187200920228     C                   EXSR      AGGTB1
187300941130     C*
187400941130     C* IMPOSTO IL PAESE DI ARRIVO
187500941130     C                   MOVEL     TASLOD        DESPAR
187600990625     C                   MOVEL     TASPRD        DESPRV
187700110315    6C                   END
187800110315    5C                   END
187900920228     C***
188000920228     C*** DA BOLLA DOPPIA A BOLLA DOPPIA
188100941130    5C     SAVTB2        IFNE      '  '
188200920227     C     §3ATB2        ANDNE     '  '
188300920228     C* AGGIORNO 1 BOLLA
188400941130     C                   MOVEL     'N'           VARFCA            1
188500920228     C                   EXSR      AGGTB1
188600941130     C* AGGANCIO 2 BOLLA
188700941130     C                   EXSR      CHTB2
188800941130     C* IMPOSTO IL PAESE DI ARRIVO
188900941130     C                   MOVEL     TASLOD        DESPAR
189000990625     C                   MOVEL     TASPRD        DESPRV
189100920227     C*
189200941130     C   02
189300941130    6CANN29              DO
189400920227     C*
189500941130    7C   38SAVFCA        IFEQ      2
189600920228     C     §3AFCA        OREQ      2
189700990505     c* Se il C/A si trova sulla 2 bolla, li imposto, altrimenti lo azzero
189800990505     c*  per il controllo della varia G
189900990505     c                   if        §3afca=2
190000990505     c                   eval      wcas=arbcas
190100990505     c                   else
190200990505     c                   clear                   wcas
190300990505     c                   endif
190400990505     c
190500920228     C                   EXSR      VARIAG
190600941130    7C                   END
190700920227     C*
190800920227     C                   MOVEL     §3ATB2        TASTBL
190900990629     C                   MOVEL     arbcbn        TASCBO
191000011218
191100011218      * riorganizzo le varie se è necessario
191200011218     c                   if        flgriorg = '1'
191300011218     c                   exsr      sr_riorg
191400011218     c                   endif
191500011218
191600920227     C*
191700941201     C* ALTRIMENTI CAMBIO IL TIPO BOLAA ANCHE NEL TA7
191800990625     C     KBLP          SETLL     TiTA7000
191900990625     C     KBLP          READE     TiTA7000                               30
192000941201    8C     *IN30         DOWEQ     *OFF
192100941201     C                   MOVEL     §3ATB2        TA7TBL
192200990625     C                   UPDATE    TITA7000
192300990625     C     KBLP          READE     TITA7000                               30
192400941201    8C                   ENDDO
192500941130    6C                   END
192600920227     C*
192700990625     C   02              UPDATE    TITAS000
192800990625     C   03              UPDATE    TITAS010
192900990625     C   04              UPDATE    TITASP00
193000920227     C*
193100920227     C* ERRORI
193200941130     C  N02
193300941130     COR 02
193400941130    6CAN 29              DO
193500941130    7C     SAVTB2        IFNE      §3ATB2
193600130410     c* solo se tipo bolla nuovo non è uno storno
193700130410     c                   movel     'TB'          cod
193800130410     c                   movel(p)  §3atb2        key
193900130410     c                   clear                   dstb
194000130410     C     KTAB          CHAIN     TABEL
194100130410    8c                   if        %found(tabel00f)
194200130410     c                   movel     tbluni        dstb
194300130410    8c                   endif
194400130410    8c                   if        §tbfcb='1'
194500130326     c                   z-add     46            comfev
194600930513     C                   MOVEL     ERR(16)       ARBDER                          MODIF BOLLA
194700130410    8c                   endif
194800941130   X7C                   ELSE
194900920228     C*
195000130326     c                   z-add     46            comfev
195100930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF A3
195200941130    7C                   END
195300920228     C*
195400920227     C                   EXSR      WTRERR
195500941130    6C                   END
195600941130    5C                   END
195700920228     C***
195800920228     C*** DA BOLLA DOPPIA A BOLLA SINGOLA
195900941130    5C     SAVTB2        IFNE      '  '
196000920227     C     §3ATB2        ANDEQ     '  '
196100950630     C*
196200920228     C* AGGANCIO 2 BOLLA
196300920228     C                   EXSR      CHTB2
196400941130     C* IMPOSTO IL PAESE DI ARRIVO
196500941130     C                   MOVEL     TASLOD        DESPAR
196600990625     C                   MOVEL     TASPRD        DESPRV
196700920227     C*
196800941130     C   02
196900941130    6CANN29              DO
197000920227     C                   MOVEL     'AP'          TASTBL
197100991021     c                   movel     arbcbn        tascbo
197200941130     C*
197300941130     C* TOLGO FLAG C/A SOLO SE DALLA 2 BOLLA E' PASSATO ALLA 1
197400941130     C     SAVFCA        IFEQ      2
197500941130     C     §3AFCA        ANDEQ     1
197600941130     C                   MOVEL     ' '           TASFCA
197700941130     C                   ENDIF
197800941130     C*
197900990625     C                   UPDATE    TITAS000
198000941130    6C                   END
198100920227     C*
198200920227     C* ERRORI
198300990505    6C   29              DO
198400130326     c                   z-add     46            comfev
198500930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF BOLLA
198600920227     C                   EXSR      WTRERR
198700941130    6C                   END
198800920227     C**
198900920228     C** AGGIORNO 1 BOLLA
199000941130     C                   MOVEL     'S'           VARFCA
199100920228     C                   EXSR      AGGTB1
199200941130    5C                   END
199300920228     C***
199400920228     C*** DA BOLLA SINGOLA A BOLLA DOPPIA
199500941130    5C     SAVTB2        IFEQ      '  '
199600920227     C     §3ATB2        ANDNE     '  '
199700950630     C*
199800920228     C* AGGIORNO PRIMA BOLLA
199900941130     C                   MOVEL     'S'           VARFCA
200000920228     C                   EXSR      AGGTB1
200100920227     C*
200200000816     c* Prima di creare la seconda bolla cancello eventualmente bolla tipo
200300000816     c* "AP" che potrei avere in seguito ad un precedente cambio da bolla
200400000816     c* doppia a bolla singola
200500000816     c                   movel     'AP'          comtbl
200600000816     c                   setoff                                       020304
200700000816     c     kblp          chain     titas30c                           39
200800000816     c     *in39         ifeq      *off
200900000816     c     *in02         andeq     *on
201000000816     c     tasdft        ifeq      *zeros
201100000816     c     tasfic        oreq      ' '
201200000816     c                   delete    titas000
201300000816     c                   endif
201400000816     c                   endif
201500920227     C* CREO SECONDA BOLLA
201600920227     C*
201700950201     C* AGGANCIO FILE FNARBD1R
201800170109     c*  per sicurezza chaino bolla in FNBLP e aggiorno i dati
201900170109     c*  del destinatario da blp.
202000170109     C     ARBORV        ADD       1             COMORV
202100170109     C     KARBD         CHAIN     FNARBDRR                           32
202200170109     c
202300170109     C**N32              ADD       1             NUMRBD
202400920227     C* 32 ON - INESISTENTE: NON SCRIVO DESTINATARIO E SEGNALO ERRORE
202500170109    6C** 32              DO
202600170109     C**                 MOVEL     *BLANKS       ARBRSD
202700170109     C**                 MOVEL     *BLANKS       ARBRD2
202800170109     C**                 MOVEL     *BLANKS       ARBIND
202900170109     C**                 MOVEL     *BLANKS       ARBLOD
203000170109     C**                 MOVEL     *BLANKS       ARBCAD
203100170109     C**                 MOVEL     *BLANKS       ARBPRD
203200170109     C**                 MOVEL     *BLANKS       ARBNZD
203300920227     C*
203400170109     C**                 MOVEL     ERR(12)       ARBDER
203500170109     C**                 Z-ADD     102           COMFEV
203600170109     C**                 EXSR      WTRERR
203700170109    6C***                END
203800920227     C*
203900920227     C*
204000941130     C                   MOVEL     §3ATB2        TASTBL
204100990629     C                   MOVEL     ARBCBN        TASCBO
204200941130     C                   MOVE      TASKSC        KSC4
204300941130     C*
204400941130     C                   CLEAR                   TASRSD
204500941130     C                   CLEAR                   TASIND
204600990625     C                   CLEAR                   TASCAd
204700941130     C                   CLEAR                   TASLOD
204800990625     C                   CLEAR                   TASPRd
204900990625     C                   CLEAR                   TAScpd
205000170109     c
205100170109     c     kann          chain     fnblp01l
205200941130     C*
205300941130     C* MUOVO L'INIDIRIZZO COMPLETO DEL DESTINATARIO
205400170109     c                   if        %found(fnblp01l)
205500170109     C                   MOVEL     blpRSD        TASRSD
205600170109     C                   MOVEL     blpIND        TASIND
205700170109     C                   MOVEL     blpCAD        TASCAD
205800170109     C                   MOVEL     blpLOD        TASLOD
205900170109     C                   MOVEL     blpLOD        DESPAR
206000170109     C                   MOVEL     blpPRD        DESPRV
206100170109     C                   MOVEL     blpPRD        TASPRD
206200170109     C                   MOVEL     blpNZD        TASNZD
206300990701     c* Muovo comunque la p.iva in tascpd anche se in questo caso
206400990701     c* sarà sempre vuota perchè visto che prima della variazione
206500990701     c* la bolla era franco non poteva esserci in filiale la p.iva
206600990701     c* del destinatario memoziazzata su fnarb
206700170109     C*********          MOVEL     ARBCPI        TASCPD
206800170109     c                   endif
206900941130     C*
207000941130     C                   CLEAR                   TASPVL
207100941130     C                   CLEAR                   TASFTC
207200950202     C                   CLEAR                   TASTC2
207300941130     C                   CLEAR                   TASIAS
207400990824     C                   CLEAR                   TASFCM
207500950202     C                   CLEAR                   TASVAS
207600941130     C                   MOVEL     *ZEROS        TASCTR
207700930604     C                   MOVEL     *BLANKS       TASFTM
207800991018     C                   MOVEL     *BLANKS       TASDIV
207900920227     C                   MOVE      *ZERO         TASPOR
208000920227     C                   MOVE      *BLANK        TASSV1
208100920227     C                   MOVE      *ZERO         TASVA1
208200920227     C                   MOVE      *BLANK        TASSV2
208300920227     C                   MOVE      *ZERO         TASVA2
208400930212     C                   MOVE      *BLANK        TASSV3
208500930212     C                   MOVE      *ZERO         TASVA3
208600920227     C                   Z-ADD     0             TASDFT
208700920227     C                   Z-ADD     0             TASNFT
208800920227     C                   Z-ADD     0             TASFIV
208900930212     C                   MOVE      ' '           TASFA2
209000920227     C                   Z-ADD     9999          TASKSC
209100920227     C                   MOVEL     TASLNA        TASKSC
209200920227     C* CONTRASSEGNO A SECONDA DEL FLAG
209300941130    6C     §3AFCA        IFEQ      2
209400941130     C                   MOVEL     'S'           TASFCA
209500920227     C                   MOVEL     §3ATB2        TBLCA             2
209600920227     C                   ELSE
209700941130     C                   MOVEL     ' '           TASFCA
209800941130    6C                   END
209900920227     C*
210000990624     C                   MOVE      *zeros        TASimv
210100941130     C                   MOVEL     ' '           TASFEI
210200941130     C                   MOVEL     ' '           TASFTS
210300941130     C                   MOVEL     ' '           TASFIC
210400920227     C*
210500990625     C                   WRITE     TITAS000                             99
210600950630     C*
210700950630     C     SAVFCA        IFEQ      1
210800950630     C     §3AFCA        ANDEQ     2
210900950630     C     KANN          CHAIN     TNCSB000                           31
211000991103     c                   move      csbvca        wcsbvca
211100991103     c     csbcas        ifgt      0
211200991103     c     wcsbvca       andeq     *blanks
211300991103     c                   movel     'ITL'         wcsbvca
211400991103     c                   endif
211500950630     C     CSBCAS        IFNE      ARBCAS
211600991103     C     wcsbvca       ORNE      ARBVCA
211700950630     C                   MOVEL     'VACA'        CSVCAV
211800950630     C                   EXSR      CSVWTR
211900950630     C                   END
212000950630     C*
212100950630     C                   Z-ADD     9999          CSBKSC
212200950630     C                   MOVEL     TASLNA        CSBKSC
212300950630     C                   MOVEL     §3ATB2        CSBTBL
212400950630     C*
212500950630     C                   EXSR      AGGCSB
212600950630     C                   UPDATE    TNCSB000
212700950630     C                   END
212800920227     C*
212900941130     C* REIMPOSTO ORA DI VARIAZIONE DI ARBK: ORA DI ARBD - 1
213000920227     C  N32              SUB       1             ARBORV
213100950201     C  N32              DELETE    FNARBDRR
213200941130    5C                   END
213300920227     C*
213400920227     C* CREO SE NECESSARIO RECORD IN FILE CONTRASSEGNI
213500941130    5C     SAVFCA        IFEQ      0
213600941130    6C     §3AFCA        IFEQ      1
213700920227     C                   MOVEL     §3ATB1        TBLCA
213800941130     C* RICHAINO LA BOLLA
213900941130     C                   MOVEL     §3ATB1        COMTBL
214000941130     C                   SETOFF                                       020304
214100990625     C     KBLP          CHAIN     TITAS30C                           30
214200110315     c* Se solo C/Ass--> aggiungo il flag
214300110315     c                   if        SoloCAss='S'
214400110315     c                   eval      TASFCA='S'
214500110315     C                   MOVEL     TASLOD        DESPAR
214600110315     C                   MOVEL     TASPRD        DESPRV
214700110315     C   02              UPDATE    TITAS000
214800110315     C   03              UPDATE    TITAS010
214900110315     C   04              UPDATE    TITASP00
215000110315     c                   endif
215100950913     C*
215200920227     C                   EXSR      CRTCSB
215300941130     C*
215400920227     C                   ELSE
215500941130     C*
215600941130    7C     §3AFCA        IFEQ      2
215700920227     C                   MOVEL     §3ATB2        TBLCA
215800941130     C* RICHAINO LA BOLLA
215900941130     C                   MOVEL     §3ATB2        COMTBL
216000941130     C                   SETOFF                                       020304
216100990625     C     KBLP          CHAIN     TITAS30C                           30
216200920227     C                   EXSR      CRTCSB
216300941130    7C                   END
216400941130    6C                   END
216500941130    5C                   END
216600920227     C*
216700941130    4C                   END
216800941130    3C                   END
216900941130    2C                   END
217000930513     C*
217100930513     C* 11 ON - RECORD DESTINATARIO PER CREARE 2 BOLLA
217200941130   X1C                   ELSE
217300950202     C* NON CANCELLO IL RECORD : IN ARBM LO LASCIO LI'
217400950202     C                   SETON                                        37
217500950202     C*
217600950202     C     *IN11         IFEQ      *ON
217700930513     C                   MOVEL     102           COMFEV
217800930513     C                   MOVEL     ERR(15)       ARBDER
217900930513     C                   EXSR      WTRERR
218000950202     C                   ENDIF
218100950202    1C                   ENDIF
218200920227     C*
218300920227     C                   ENDSR
218400060317     C**---------------------------------------------------------
218500060317     C** AGGIORNO CODICE VARIAZIONE RM
218600060317     C**---------------------------------------------------------
218700060317     C     AGARBR        BEGSR
218800060317     c* rma in tita4 record "A"
218900060317     c                   setoff                                       0506
219000060317     C                   movel     'A'           §trc
219100060317     C     ktita         chain     tita430c                           32
219200060317    3C     *in32         ifeq      *off
219300060317     C                   movel     ta4not        dta4a
219400060317     c                   movel     arbrsd        §TA4ARMA
219500160121     C                   MOVEL     ARBrd2        §TA4anas
219600060317     C                   movel     dta4a         ta4not
219700060317     C   05              update    tita4000
219800060317     C   06              update    tita4p00
219900060317     c                   endif
220000060317     c*
220100160412     C****               MOVEL     ARBrd2        TASnas
220200160412     C** 02              UPDATE    TiTAS000
220300160412     C** 03              UPDATE    TiTAS010
220400160412     C** 04              UPDATE    TiTASP00
220500160412     c**
220600160412     c                   unlock    titas30c
220700060317     c*
220800060317     C                   ENDSR
220900920228     C**---------------------------------------------------------
221000920228     C** AGGIORNO PRIMA BOLLA
221100920228     C**---------------------------------------------------------
221200920228     C     AGGTB1        BEGSR
221300920228     C* AGGANCIO 1 BOLLA
221400920228     C                   EXSR      CHTB1
221500920228     C*
221600941201     C* SE E' DA AGGIUNGERE LO AGGIUNGO
221700920228     C     §3AFCA        IFEQ      1
221800941130     C                   MOVEL     'S'           TASFCA
221900941201     C*
222000920228     C                   ELSE
222100941201     C* LO TOLGO SOLO SE FALG IMPOSTATO = "S"
222200941201     C*
222300941201     C     VARFCA        IFEQ      'S'
222400941130     C                   MOVEL     ' '           TASFCA
222500920228     C                   END
222600941130     C                   END
222700920228     C*
222800941130     C   02
222900941130     CANN29              DO
223000920228     C   38SAVFCA        IFEQ      1
223100920228     C     §3AFCA        OREQ      1
223200990505     c* Devo impostare l'importo C/A nella routine varia G :
223300990505     c*  se il C/A e' stato tolto dalla 1 bolla, richiamo con importo=0
223400990505     c                   if        §3afca=1
223500990505     c                   eval      wcas=arbcas
223600990505     c                   else
223700990505     c                   clear                   wcas
223800990505     c                   endif
223900990505     c
224000920228     C                   EXSR      VARIAG
224100920228     C                   END
224200920228     C*
224300920228     C                   MOVEL     §3ATB1        TASTBL
224400990629     C                   MOVEL     ARBCBN        TASCBO
224500011219
224600011219      * riorganizzo le varie se è necessario
224700011219     c                   if        flgriorg = '1'
224800011219     c                   exsr      sr_riorg
224900011219     c                   endif
225000011219
225100000209     C* AGGIORNO TITA7000 SE ESISTE
225200000209     C     KBLP          SETLL     TITA7000
225300000209     C     KBLP          READE     TITA7000                               30
225400000209     C*
225500000209     C     *IN30         DOWEQ     *OFF
225600000209     C                   MOVEL     §3ATB1        TA7TBL
225700000209     C                   UPDATE    TITA7000
225800000209     C     KBLP          READE     TITA7000                               30
225900000209     C                   ENDDO
226000920228     C                   END
226100941130     C*
226200920228     C*
226300990625     C   02              UPDATE    TITAS000
226400990625     C   03              UPDATE    TITAS010
226500990625     C   04              UPDATE    TITASP00
226600920228     C*
226700990505     C* spedizione gia' contabilizzata
226800990505     C   29              DO
226900920228     C*
227000920312     C                   MOVEL     *BLANKS       ARBDER
227100920228     C     §3ATB1        IFNE      SAVTB1
227200130410     c* solo se tipo bolla nuovo non è uno storno
227300130410     c                   movel     'TB'          cod
227400130410     c                   movel(p)  §3atb1        key
227500130410     c                   clear                   dstb
227600130410     C     KTAB          CHAIN     TABEL
227700130410    3c                   if        %found(tabel00f)
227800130410     c                   movel     tbluni        dstb
227900130410    3c                   endif
228000130410    3c                   if        §tbfcb='1'
228100130326     c                   z-add     46            comfev
228200930513     C                   MOVEL     ERR(16)       ARBDER                         MODIF BOLLA
228300130410    3c                   endif
228400920228     C                   ELSE
228500920228     C*
228600920228     C     SAVFCA        IFNE      1
228700920228     C     §3AFCA        ANDEQ     1
228800130325     C***                MOVEL     ERR(18)       ARBDER                         AGGIUN VARIA
228900130325     c***                setoff                                       36
229000120423     c                   exsr      ctr_vrb
229100920228     C                   END
229200920228     C*
229300920228     C     SAVFCA        IFEQ      1
229400920228     C     §3AFCA        IFNE      1
229500941130     C*
229600941130     C* CONTROLLO SE ESISTE VARIA "G"
229700130326     C***                EXSR      ESVARG
229800130325     C***  WSIVAR        IFEQ      'S'
229900130325     C***                MOVEL     ERR(14)       ARBDER                         TOLTA VARIA
230000130325     c***                setoff                                       36
230100130325     C***                END
230200121205     c                   eval      wanncas='S'
230300121203     c                   exsr      ctr_vrb
230400941130     C*
230500920228     C                   ELSE
230600130325     C****               MOVEL     ERR(22)       ARBDER                         MODIF VARIA
230700130325     c****               setoff                                       36
230800120423     c                   exsr      ctr_vrb
230900920228     C                   END
231000920228     C                   END
231100920228     C                   END
231200920228     C*
231300920312     C     ARBDER        IFNE      *BLANKS
231400920228     C                   EXSR      WTRERR
231500920228     C                   END
231600920312     C                   END
231700920228     C                   ENDSR
231800920227     C**---------------------------------------------------------
231900920227     C** CALCOLO O ANNULLO VARIA  G
232000120322     C**---------------------------------------------------------
232100920227     C     VARIAG        BEGSR
232200920228     C                   MOVE      TASKSC        KSC4
232300990505     c                   clear                   ARBDER
232400011218
232500011218     c                   clear                   flgriorg
232600941130     C*
232700990414     c* Se data fattura a zero e cliente codificato vado a togliere la varia G
232800990505     C     TASDFT        IFEQ      *ZEROS
232900990414    1C     KSC4          ANDNE     9999
233000920303     C     KSC4          ANDNE     8888
233100920228     C*
233200990505     c* C/A tolto
233300990505     c                   if        wcas=0
233400941130     C                   SELECT
233500941130     C     TASSV1        WHENEQ    'G'
233600011218     c                   eval      flgriorg = '1'
233700991019     c     tasimv        ifeq      tasva1
233800991019     c                   clear                   tasdiv
233900991019     c                   endif
234000920227     C                   MOVEL     ' '           TASSV1
234100920227     C                   Z-ADD     0             TASVA1
234200990624     C                   MOVE      *zeros        TASimv
234300941130     C     TASSV2        WHENEQ    'G'
234400011218     c                   eval      flgriorg = '1'
234500991019     c     tasimv        ifeq      tasva2
234600991019     c                   clear                   tasdiv
234700991019     c                   endif
234800920227     C                   MOVEL     ' '           TASSV2
234900920227     C                   Z-ADD     0             TASVA2
235000990624     C                   MOVE      *zeros        TASimv
235100941130     C     TASSV3        WHENEQ    'G'
235200011218     c                   eval      flgriorg = '1'
235300991019     c     tasimv        ifeq      tasva3
235400991019     c                   clear                   tasdiv
235500991019     c                   endif
235600930212     C                   MOVEL     ' '           TASSV3
235700930212     C                   Z-ADD     0             TASVA3
235800990624     C                   MOVE      *zeros        TASimv
235900941130     C*
236000941130     C                   OTHER
236100941130     C*
236200941130     C                   MOVEL     'G'           WSV1
236300990625     C     KBLPV         DELETE    TITA7000                           30
236400990624     C  n30              MOVE      *zeros        TASimv
236500941130     C                   ENDSL
236600990419     c                   else
236700990419     c* C/A aggiunto o midificato
236800990419     c                   EXSR      ESVARG
236900990419     c* Se la varia non c'e' tolto totale imponibile cosi' la calcola
237000990419     c                   if        wsivar=' '
237100990624     c                   clear                   TASimv
237200990419     c                   else
237300990419     c* Se gia' esiste stampo errore --> no modificata varia G
237400130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
237500130326     C***                EXSR      WTRERR
237600130326     c***                eval      wcntab=' '
237700130326     c***                exsr      ctr_vrb
237800990419     c                   endif
237900990419     c                   endif
238000920228     C*
238100941130   X1C                   ELSE
238200990414     C* CLIENTE NON CODIFICATO O CON DATA FATTURA VALORIZZATA
238300991019     C* COMUNQUE SE ANNULLATO C/A ELIMINO VARIA G  E ADEGUO TOT.IMPONIBILE
238400990414     C* Se c'è importo del contrassegno e la data fattura è uguale a zero posso
238500990414     c* togliere la varia "G"
238600990505    2C     WCAS          IFEQ      0
238700990505     C     TASDFT        ANDEQ     0
238800990414     c*
238900941130    3C                   SELECT
239000941130     C     TASSV1        WHENEQ    'G'
239100011218     c                   eval      flgriorg = '1'
239200991019     c                   sub       tasva1        tasimv
239300991019     c     tasimv        ifeq      0
239400991019     c                   clear                   tasdiv
239500991019     c                   endif
239600920228     C                   MOVEL     ' '           TASSV1
239700920228     C                   Z-ADD     0             TASVA1
239800941130     C     TASSV2        WHENEQ    'G'
239900011218     c                   eval      flgriorg = '1'
240000991019     c                   sub       tasva2        tasimv
240100991019     c     tasimv        ifeq      0
240200991019     c                   clear                   tasdiv
240300991019     c                   endif
240400920228     C                   MOVEL     ' '           TASSV2
240500920228     C                   Z-ADD     0             TASVA2
240600941130     C     TASSV3        WHENEQ    'G'
240700011218     c                   eval      flgriorg = '1'
240800991019     c                   sub       tasva3        tasimv
240900991019     c     tasimv        ifeq      0
241000991019     c                   clear                   tasdiv
241100991019     c                   endif
241200930212     C                   MOVEL     ' '           TASSV3
241300930212     C                   Z-ADD     0             TASVA3
241400941130     C                   OTHER
241500941130     C                   MOVEL     'G'           WSV1
241600991019     C     KBLPV         chain     TITA7000                           30
241700991019     c  n30              sub       ta7van        tasimv
241800991019     c  n30              delete    tita7000
241900941130    3C                   ENDSL
242000920228     C*
242100941130   X2C                   ELSE
242200990414     c*
242300990414     c* Se è già stato fatturato oppure arbcas > 0
242400920228     C*
242500941130     C* VEDO SE LA VARIA "G" C'ERA
242600130326     C*****              EXSR      ESVARG
242700941130     C*
242800991019     c* se c'è  l'importo del contrassegno per non codificati
242900991019     c*                                      o data fattura maggiore di zero
243000130326    3c***  wcas          ifgt      *zeros
243100990414     c*
243200130326    4C***  WSIVAR        IFEQ      ' '
243300130326     C***                MOVEL     ERR(24)       ARBDER                         AGGIUN VARIA
243400130326     C***                EXSR      WTRERR
243500130326     c***                eval      wcntab=' '
243600130326     c***                exsr      ctr_vrb
243700130326     C***                ELSE
243800130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
243900130326     C***                EXSR      WTRERR
244000130326     c***                eval      wcntab=' '
244100130326     c***                exsr      ctr_vrb
244200130326    4C***                END
244300130326    3c****               endif
244400990505     c*
244500990414     c* se l'importo è uguale a zero e prima c'era la varia "G"
244600130326    3c***  wcas          ifeq      *zeros
244700130326     c***  wsivar        andne     ' '
244800130326     C***                MOVEL     ERR(23)       ARBDER                          no tolta varia
244900130326     C***                EXSR      WTRERR
245000130326    3C***                END
245100941130    2C                   END
245200941130    1C                   END
245300920227     C*
245400920227     C                   ENDSR
245500011218      *----------------------------------------------------------
245600011218      * Riorganizzo le varie sul titas
245700011218      *----------------------------------------------------------
245800011218     c     sr_riorg      begsr
245900011218
246000011218     c                   clear                   sksv
246100011218     c                   clear                   skva
246200011218     c                   eval      xx = 1
246300011218
246400011219      * carico la sk delle varie dal titas
246500011218     c                   if        tassv1 <> *blanks
246600011218     c                   eval      sksv(xx) = tassv1
246700011218     c                   eval      skva(xx) = tasva1
246800020107     c                   clear                   tassv1
246900020107     c                   clear                   tasva1
247000011218     c                   add       1             xx
247100011218     c                   endif
247200011218     c                   if        tassv2 <> *blanks
247300011218     c                   eval      sksv(xx) = tassv2
247400011218     c                   eval      skva(xx) = tasva2
247500020107     c                   clear                   tassv2
247600020107     c                   clear                   tasva2
247700011218     c                   add       1             xx
247800011218     c                   endif
247900011218     c                   if        tassv3 <> *blanks
248000011218     c                   eval      sksv(xx) = tassv3
248100011218     c                   eval      skva(xx) = tasva3
248200020107     c                   clear                   tassv3
248300020107     c                   clear                   tasva3
248400011218     c                   add       1             xx
248500011218     c                   endif
248600011218
248700011219      * carico la sk delle varie dal tita7
248800011219     c     kblp          setll     tita7000
248900011218     c                   do        *hival
249000011219     c     kblp          reade     tita7000
249100011219     c                   if        %eof
249200011218     c                   leave
249300011219     c                   endif
249400011218     c                   eval      skva(xx) = ta7van
249500011218     c                   eval      sksv(xx) = ta7svn
249600011218     c                   add       1             xx
249700011219     c                   delete    tita7000
249800011218     c                   enddo
249900011219
250000011219      * scrivo le varie su titas
250100011219     c                   if        sksv(1) <> *blanks
250200011219     c                   eval      tassv1 = sksv(1)
250300011219     c                   eval      tasva1 = skva(1)
250400011219     c                   endif
250500011219     c                   if        sksv(2) <> *blanks
250600011219     c                   eval      tassv2 = sksv(2)
250700011219     c                   eval      tasva2 = skva(2)
250800011219     c                   endif
250900011219     c                   if        sksv(3) <> *blanks
251000011219     c                   eval      tassv3 = sksv(3)
251100011219     c                   eval      tasva3 = skva(3)
251200011219     c                   endif
251300011219
251400011219      * scrivo le varie su tita7
251500011219     c     4             do        20            xx
251600011219     c                   if        sksv(xx) <> *blanks
251700011219     c                   eval      ta7aas = arbaas
251800011219     c                   eval      ta7lnp = arblnp
251900011219     c                   eval      ta7nrs = arbnrs
252000011219     c                   eval      ta7nsp = arbnsp
252100011219     c                   eval      ta7tbl = comtbl
252200011219     c                   eval      ta7svn = sksv(xx)
252300011219     c                   eval      ta7van = skva(xx)
252400011219     c                   write     tita7000
252500011219     c                   endif
252600011219     c                   enddo
252700011218
252800011218     c                   endsr
252900011218
253000920210     C**---------------------------------------------------------
253100920210     C** CREO RECORD CONTRASSEGNO
253200920210     C**---------------------------------------------------------
253300920210     C     CRTCSB        BEGSR
253400941130     C                   MOVEL     ARBDER        SAVDER
253500920303     C*
253600920303     C* SE ESISTE ANNULLATO LO DELETO E RISCRIVO
253700941130     C     KANN          CHAIN     TNCSB000                           41
253800941130    1C     *IN41         IFEQ      *ON
253900941130     C                   CLEAR                   TNCSB000
254000941130     C                   ELSE
254100941130     C     KANN          CHAIN     TNCSA000                           39
254200941130     C  N39              DELETE    TNCSA000
254300941130    1C                   ENDIF
254400920303     C*
254500950201     C* AGGANCIO FILE FNARBM0R
254600170109     C***  KARBM         CHAIN     FNARBMRR                           35
254700170109     C**N35              ADD       1             NUMRBM            7 0
254800170109     c** non utilizzo più il mittente da trasmissione in FNARBM
254900170109     c*  ma aggancio direttamente l'anagrafica mittente in TITAA30C
255000170109     c
255100920212     C* INESISTENTE: STAMPO MA CREO UGUALMENTE ARCHIVIO (DA COMPLETARE)
255200170109    1C** 35              DO
255300170109     C**                 MOVEL     ERR(11)       ARBDER
255400170109     C**                 Z-ADD     102           COMFEV
255500170109     C**                 EXSR      WTRERR
255600920212     C* AZZERO CAMPI MITTENTE
255700170109     C**                 MOVEL     *BLANKS       ARBRSM
255800170109     C**                 MOVEL     *BLANKS       ARBINM
255900170109     C**                 MOVEL     *BLANKS       ARBCAM
256000170109     C**                 MOVEL     *BLANKS       ARBLOM
256100170109     C**                 MOVEL     *BLANKS       ARBPRD
256200170109     C**                 MOVEL     *ZEROS        ARBCCM
256300170109    1C**                 END
256400920212     C*
256500920210     C                   MOVEL     TASLNP        CSALNP
256600920210     C                   MOVEL     TASNRS        CSANRS
256700920210     C                   MOVEL     TASAAS        CSAAAS
256800920210     C                   MOVEL     TASNSP        CSANSP
256900920210     C*
257000920210     C                   MOVEL     TBLCA         CSBTBL
257100920210     C                   Z-ADD     TASLNP        CSBLNP
257200920210     C                   Z-ADD     TASAAS        CSBAAS
257300920210     C                   Z-ADD     TASNRS        CSBNRS
257400920210     C                   Z-ADD     TASNSP        CSBNSP
257500941130     C                   Z-ADD     TASMGS        CSBMGS
257600990629     C                   Z-ADD     TASTFP        CSBFLE
257700941130     C                   MOVEL     DESPAR        CSBPAR                         PAESE ARRIVO
257800970916     C                   MOVEL     DESPRV        CSBPRO                         PROV.DEST.
257900030310     C                   MOVE      *ZERO         CSBSTA
258000950202     C                   EXSR      AGGCSB
258100920304     C*
258200941130     C                   MOVEL     TBLCA         FLGTB1
258300920304     C*
258400920304     C* SE BLANK E CODIFICATO DESTINATARIO CHIAN
258500941130    2C     TASRSD        IFEQ      *BLANKS
258600990625     c* N.B.la call al tibs69 sarà da togliere quando tutte le bolla avranno
258700990625     c* il destinatario pieno
258800990625     c     flgtb1        ifeq      'A'
258900981001     C                   CLEAR                   DSBS69
259000981001     C                   MOVEL     KCI           I69KCC
259100981001     C                   MOVEL     KNSIF         I69SIF
259200981001     C                   MOVEL     TASKSC        I69KAC
259300981001     C                   CALL      'TIBS69R'
259400981001     C                   PARM                    DSBS69
259500981001     C                   PARM                    DSACO
259600981001     C                   PARM                    DSIND
259700981001     C                   PARM                    DSCLP
259800981001     C                   PARM                    DSCLS
259900981001     C**
260000981001     C     O69ERR        IFEQ      ' '
260100981001     C                   MOVEL     ACORAG        CSBRSD
260200981001     C                   ELSE
260300981001     C                   MOVEL     *BLANKS       CSBRSD
260400981001     C                   ENDIF
260500990625     C                   ENDIF
260600981001     C                   ELSE
260700981001     C                   MOVEL     TASRSD        CSBRSD
260800941130    2C                   END
260900920304     C*
261000920304     C*
261100920210     C                   Z-ADD     TASRMN        CSBRMN
261200990625     c* prendo il riferimento mittente alfabetico da estensione (tita4)
261300990625     c                   movel     'A'           §trc
261400010126     c     ktita         chain(N)  tita430c                           39
261500990625     c  n39              movel     ta4not        csbrma
261600990625     c   39              clear                   csbrma
261700941207     C                   Z-ADD     TASLNA        CSBLNA
261800040721     C**                   MOVEL     TASFIN        CSBFIN
261900920210     C                   Z-ADD     TASKSC        CSBKSC
262000920210     C                   MOVE      CSBKSC        CSBCDI
262100931108     C   41              MOVEL     'R'           CSBFTR
262200931108     C   41              MOVE      DATEU         CSBDTR
262300920303     C*
262400941130    1C     FLGTB1        IFEQ      'A'
262500941130     C                   Z-ADD     99999         CSBKSC
262600941130     C                   MOVEL     TASLNA        CSBKSC
262700941206     C                   MOVE      CSBKSC        CSBCDI                         WB/LB
262800941206    1C                   END                                                    WB/LB
262900941130     C*
263000170109     C* VEDO SE tasccm   e'GENERICo
263100170109     C                   MOVE      tasccm        W0040             4 0
263200170109     c                   if        tasccm<>8888 and tasccm<>9999
263300941130     C                   MOVEL     TASCCM        CSBCDI
263400170109     c                   endif
263500170109     c
263600941206    1C*************        END                             WB/LB
263700941130     C*
263800920210     C** RAGGRUPPAMENTO C/ASSEGNI
263900920210     C     TASLNA        CHAIN     AZORG                              51
264000920210     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
264100920210     C   51              Z-ADD     TASLNA        CSBRGF
264200941130     C*
264300941130     C* VEDO SE CSBCDI E' INTESTAZIONE GENERICA
264400941130     C                   MOVE      CSBCDI        W0040             4 0
264500920210     C*
264600920210     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
264700941130     C     W0040         IFEQ      8888
264800941130     C     W0040         OREQ      9999
264900170109     C**                 MOVEL     ARBRSM        CSARSD                         MITTENTE
265000170109     C**                 MOVEL     ARBINM        CSAVID                            '
265100170109     C**                 MOVEL     ARBCAM        CSACAD                            '
265200170109     C**                 MOVEL     ARBLOM        CSACID                            '
265300170109     C**                 MOVEL     ARBPRM        CSAPRM
265400170109     C**                 MOVEL     ARBNZM        CSANZM
265500170109
265600170109     c                   movel     'M'           §trc
265700170109     c     ktita         chain(N)  titaa30c
265800170109     c                   if        %found(titaa30c)
265900170109     C                   MOVEL     taarsc        CSARSD                         MITTENTE
266000170109     C                   MOVEL     taaind        CSAVID                            '
266100170109     C                   MOVEL     taacap        CSACAD                            '
266200170109     C                   MOVEL     taaloc        CSACID                            '
266300170109     C                   MOVEL     taaprv        CSAPRM
266400170109     C                   MOVEL     taanaz        CSANZM
266500170109     c                   else
266600170109     c                   clear                   csarsd
266700170109     c                   clear                   csavid
266800170109     c                   clear                   csacid
266900170109     c                   clear                   csaprm
267000170109     c                   clear                   csanzm
267100170109     c                   endif
267200920212     C*
267300941201     C                   WRITE     TNCSA000                             99
267400920212     C                   END
267500920212     C**
267600941201     C   41              WRITE     TNCSB000                             99
267700941130     C  N41              UPDATE    TNCSB000
267800950914     C*
267900950914     C* CREO RECORD DI INSERIMENTO C/ASSEGNO NELLE VARIAZIONI
268000950914     C                   MOVEL     'IMCA'        CSVCAV
268100950914     C                   EXSR      CSVWTR
268200941130     C*
268300941130     C** SE ERA IMPOSTATO LO RIMETTO COME ERA PRIMA
268400941130     C                   MOVEL     SAVDER        ARBDER
268500920210     C                   ENDSR
268600920210     C**---------------------------------------------------------
268700920210     C** CONTROLLO ESISTENZA ARCHIVIO CONTRASSEGNI
268800920210     C**---------------------------------------------------------
268900920210     C     CONCSB        BEGSR
269000930513     C                   SETOFF                                           42    25
269100920212     C* VARIO  CONTRASSEGNO SOLO SE NON PAGATO-INCASSATO
269200941130     C     KANN          CHAIN     TNCSB000                           31
269300930303     C*
269400941130     C   31KANN          SETLL     FNLBL01L                               42
269500920212     C*
269600930513     C* PER CASUALE VARIAZIONE "K" SEMPRE ERRORE
269700930303     C   31FLGCVB        IFEQ      'K'
269800930513     C                   SETON                                        25
269900930303     C                   MOVEL     ERR(5)        ARBDER
270000930303     C                   Z-ADD     102           COMFEV
270100930303     C                   EXSR      WTRERR
270200930513     C*
270300930303     C                   ELSE
270400930513     C*
270500930513     C* ALTRIMENTI SOLO SE NON C'E' IL LEGAME BOLLA
270600930513     C     *IN42         IFEQ      '0'
270700930513     C                   SETON                                        25
270800930513     C                   MOVEL     ERR(5)        ARBDER
270900930513     C                   Z-ADD     102           COMFEV
271000930513     C                   EXSR      WTRERR
271100930303     C                   END
271200930513     C                   END
271300991103     c*
271400991103     c     *in31         ifeq      *off
271500991103     c                   movel     csbvca        wcsbvca
271600991103     c     csbcas        ifgt      0
271700991103     c     csbvca        andeq     *blanks
271800991103     c                   movel     'ITL'         wcsbvca
271900991103     c                   endif
272000991103     c                   endif
272100930513     C*
272200920302     C* GIA' INCASSATO O PAGATO
272300950202    1C  N31CSBDDC        IFNE      0
272400920210     C     CSBBNA        ORNE      0
272500920212     C*
272600950324     C* SE NON MODIFICATO L'IMPORTO DEL C/A OK, SE VARIAZIONE "K"
272700950324     C     FLGCVB        IFEQ      'K'
272800950202    3C     ARBCAS        IFNE      CSBCAS
272900991102     C     ARBVCA        ORNE      WCSBVCA
273000920212     C                   SETON                                        31
273100110214     C                   Z-ADD     102           COMFEV
273200950202    2C                   END
273300110214     c
273400950324     C* ALTRIMENTI SEMPRE ERRORE
273500950324     C                   ELSE
273600950324     C                   SETON                                        31
273700110214     C                   Z-ADD     102           COMFEV
273800950324    2C                   END
273900920601     C*
274000950202    2C   31FLGCVB        IFNE      'I'
274100950222    2C     FLGCVB        ANDNE     'M'
274200920601     C                   SETON                                        36
274300920601     C                   MOVEL     ERR(10)       ARBDER
274400920601     C                   EXSR      WTRERR
274500950324     C                   ELSE
274600950324     C                   CLEAR                   COMFEV
274700950202    2C                   END
274800950202    1C                   END
274900920210     C                   ENDSR
275000950202     C*
275100950202     C**---------------------------------------------------------
275200950202     C** AGGIORNO RECORD CONTRASSEGNO
275300950202     C**---------------------------------------------------------
275400950202     C     AGGCSB        BEGSR
275500950202     C                   Z-ADD     ARBCAS        CSBCAS
275600950202     C                   MOVEL     ARBVCA        CSBVCA
275700950202     C                   MOVEL     ARBGCA        CSBGCA
275800950203     C                   Z-ADD     ARBCMB        CSBCMB
275900030310     c                   if        csbsta <> 3
276000000703     c                   movel     0             csbsta
276100030310     c                   end
276200950202     C* TIPO ASSEGNO: MITTENTE / BARTOLINI
276300950202     C                   MOVEL     '1A'          COD
276400950202     C                   MOVEL(P)  ARBTIC        KEY
276500950202     C     KTAB          CHAIN     TABEL                              30
276600950202     C  N30              MOVEL     TBLUNI        DS1A
276700950202     C  N30              MOVEL     §1AFMB        CSBFUS
276800950202     C   30              CLEAR                   CSBFUS
276900950202     C                   ENDSR
277000920212     C**---------------------------------------------------------
277100920212     C** ANNULLO RECORD CONTRASSEGNO
277200920212     C**---------------------------------------------------------
277300920212     C     ANNCSB        BEGSR
277400941209     C* CREO STORICO VARIAZIONE
277500920602     C     CSBSTA        IFNE      9
277600920212     C                   MOVEL     'ANCA'        CSVCAV
277700920212     C                   EXSR      CSVWTR
277800920212     C* ANNULLO
277900920212     C                   MOVEL     9             CSBSTA
278000941130     C                   UPDATE    TNCSB000
278100920212     C                   END
278200920212     C                   ENDSR
278300920212     C**---------------------------------------------------------
278400920212     C** ESEGUE WRITE BLCSV00F                                  -
278500920212     C**---------------------------------------------------------
278600920212     C     CSVWTR        BEGSR
278700950202     C* COME DATA E ORA VARIAZIONE METTO QUELLA DELLA FILIALE
278800941130     C                   Z-ADD     ARBDTV        CSVDTV
278900941130     C                   Z-ADD     ARBORV        CSVORV
279000020604     c*
279100021112     c                   clear                   tibs36ds
279200021112     c                   movel     arbpru        i36ute
279300140407     c                   eval      %subst(i36ute:10:1)=' '
279400021112     C                   CALL      'TIBS36R'
279500021112     C                   PARM                    tibs36ds
279600020604     c* non tovato profilo su azute
279700070420   1 c     o36err        ifne      *blanks
279800020604     c* richiamo fnlv55r per sapere se lna è gestita da altro p.o. e da chi
279900020604     c                   clear                   dslv55
280000020604     c                   movel     '6'           d55tpt
280100020604     c                   z-add     taslna        d55lin
280200020604     c                   z-add     csvdtv        d55drf
280300020604     c                   call      'FNLV55R'
280400020604     c                   parm                    dslv55
280500020604     C                   MOVEL     d55tfa        CSVFEV
280600070420   x1c                   else
280700020604     c* trovato profilo su azute
280800021112     c                   movel     o36pou        csvfev
280900070420    1c                   endif
281000020604     c*
281100920212     C                   MOVEL     CSBDS         CSVDS
281200941130     C* COME DATA TRSMISSIONE QUANDO HO CARICATO
281300941130     C                   Z-ADD     DATEU         CSVDTR
281400941130     C                   WRITE     TNCSV000
281500920212     C                   ENDSR
281600920130     C**---------------------------------------------------------
281700920130     C** CONTROLLO ESISTENZA BOLLA
281800920130     C**---------------------------------------------------------
281900990628     C     CTRSPE        BEGSR
282000920302     C                   SETOFF                                       332940
282100941130     C                   SETOFF                                       4217
282200941121     C                   SETOFF                                       020304
282300930514     C                   SETON                                        23
282400080408     c                   clear                   VariataSede       1
282500110315     c                   clear                   SoloCASS          1
282600941121     C*
282700991015     C     KBLP          CHAIN     TITAS30C                           29
282800920220     C*
282900941121     C* 29 ON  - S P E D I Z I O N E   I N E S I S T E N T E
283000941121    1C     *IN29         IFEQ      *ON
283100941121     C*
283200941121     C     KBLP          CHAIN     TNBLA000                           40
283300941121     C   40KANN          CHAIN     TNBLA000                           40
283400920602     C*
283500930804     C* 40 OFF - S P E D I Z I O N E   A N N U L L A T A
283600991015     C  N40              MOVEL     ERR(28)       ARBDER
283700070119     c
283800070119     c* Verifico se lnp estera, se record bolla chiuso in partenza
283900070119     c*  come merce mai affidata
284000070119    2c                   if        *in40
284100070119     c
284200070119     c     ArbLnp        Chain     Azorg01l
284300070119    3c                   If        %Found(Azorg01l)
284400070119     c                   Eval      Og143 = OrgDe3
284500070119    4c                   If        §OgNtw = 'DPD' or §ogntw='EEX' or
284600070119     c                             §ogntw='FED'
284700070119     c* controllo bolla se presente in fnblp
284800070119     c     kann          chain     fnblp01l
284900070119    5c                   if        %found(fnblp01l) and blpft1='N'
285000070119     c                             and blpcca='7'
285100070119     c                   setoff                                       23
285200070119    5c                   endif
285300070119    4c                   endif
285400070119    3c                   endif
285500070119    2c                   endif
285600920302     C*
285700920602     C* 40 ON - INESISTENTE: VEDO SE GIA' TRASFORMATO COD BOLLA
285800070119     c   23
285900070119    2Can 40FLGCVB        IFEQ      'K'
286000941121    3C     COMTBL        IFEQ      §3ATB1
286100930108     C                   MOVEL     ERR(1)        ARBDER
286200930108     C                   ELSE
286300930108     C                   MOVEL     ERR(7)        ARBDER
286400941121    3C                   END
286500930108     C*
286600920602     C                   MOVEL     §3ATB1        SAVTB1            2
286700920602     C                   MOVEL     §3ATB2        SAVTB2            2
286800080408     c                   movel     ds3a          savds3a
286900920602     C*
287000920602     C                   MOVEL     '3A'          COD
287100070119     c                   MOVEL(P)  ARBCBN        KEY
287200920602     C     KTAB          CHAIN     TABEL                              32
287300920602     C  N32              MOVEL     TBLUNI        DS3A
287400080408     c   32              clear                   ds3a
287500080408     c                   movel     §3afca        newfca            1 0
287600920602     C*
287700941121     C* 1BOLLA MODIFICATA
287800941121    3C  N32SAVTB1        IFNE      §3ATB1
287900920602     C                   MOVEL     §3ATB1        COMTBL
288000990624     C     KBLP          SETLL     TITAS30C                               42
288100920602     C  N42              MOVEL     ERR(1)        ARBDER
288200941121    3C                   END
288300920602     C*
288400941121     C* 2 BOLLA MODIFICATA
288500920602     C* SOLO SE ESISTEVA CONTROLLO
288600941121    3C  N32SAVTB2        IFNE      §3ATB2
288700920602     C     SAVTB2        ANDNE     '  '
288800920602     C*
288900941121    4C     §3ATB2        IFEQ      '  '
289000920602     C                   MOVEL     'AP'          COMTBL
289100941121     C                   ELSE
289200920602     C                   MOVEL     §3ATB2        COMTBL
289300941121    4C                   END
289400990624     C     KBLP          SETLL     TITAS30C                               42
289500070119     C  N42              MOVEL     ERR(7)        ARBDER
289600941121    3C                   END
289700080408     C***                MOVEL     SAVTB1        §3ATB1
289800080408     C***                MOVEL     SAVTB2        §3ATB2
289900080408     c                   movel     savds3a       ds3a
290000070119     c
290100070119     c* Se codice bolla vecchio = codice bolla nuovo, anche se
290200070119     c*  inesistente è inutile ristamparlo: già fatto precedentemente
290300080408    3c  n42              if        arbcbp=arbcbn and §3afca=0
290400070119     c                   setoff                                       23
290500080408    3c                   endif
290600080408     c* Se si tratta di tipo bolla con C/ASS prima e senza dopo
290700080408     c*  o una KB che toglie solo in C/ASS,
290800080408     c*  se bolla modificata in C/S --> vado avanti per il C/ASS
290900090504    3c***                if        not *in42 and *in23  and §3atb2=*blanks
291000090504    3c                   if        *in23  and §3atb2=*blanks
291100080408    4c                   if        (newfca=0 and §3afca=1) or
291200080408     c                             (arbcbp=arbcbn and §3afca=1 and arbcas=0)
291300080408     C                   MOVEL     'F7'          COMTBL
291400080408     C     KBLP          chain     TITAS30C
291500080408    5c                   if        %found(titas30c)
291600080408     c                   setoff                                       2923
291700080408     c                   eval      VariataSede='S'
291800160714   x5c                   else
291900160714
292000160714     C                   MOVEL     'A2'          COMTBL
292100160714     C     KBLP          chain     TITAS30C
292200160714    6c                   if        %found(titas30c)
292300160714    7c                   if        tascca='2' or tascca='6'
292400160714     c                   setoff                                       2923
292500160714     c                   eval      VariataSede='S'
292600160714    7c                   endif
292700160714   x6c                   else
292800160714
292900160714     C                   MOVEL     'F1'          COMTBL
293000160714     C     KBLP          chain     TITAS30C
293100160714    7c                   if        %found(titas30c)
293200160714    8c                   if        tascca='2' or tascca='6'
293300160714     c                   setoff                                       2923
293400160714     c                   eval      VariataSede='S'
293500160714    8c                   endif
293600160714    7c                   endif
293700160714    6c                   endif
293800080408    5c                   endif
293900080408    4c                   endif
294000080408    3c                   endif
294100110315     c
294200110315     c* se ho trovato la bolla con cod bolla nuovo ma era stato annullato il C/Ass  o
294300110315     c*   non c'era e ora c'e' --> lo aggiungo o riabilito
294400110315    3c                   if        *in23  and *in42 and §3atb2=*blanks  and
294500110315    4c                              newfca=1 and §3afca=0
294600110315     c                   setoff                                       2923
294700110315     c                   eval       soloCAss='S'
294800110315     c                   endif
294900920602     C*
295000941121   X2C                   ELSE
295100920602     C*
295200941130     C* CONTROLLO SE LA SPEDIZIONE E' STORNATA
295300930514     C                   EXSR      STORNO
295400991019     c* se causale variazione 'Cx' e la spedizione e' stornata
295500991019     c* spengo in29 per caricare ugualmente la variazione
295600991021    3c     flgcvb        ifeq      'C'
295700060317    3c     flgcvb        oreq      'R'
295800991021    4c     *in23         ifeq      *off
295900991021     c                   setoff                                       29
296000991021   x4c                   else
296100991021     c* altrimenti cerco la spedizione
296200991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
296300991021     c* (cioè il franco) faccio setgt e readpe invece che chain
296400991021     c     kann          setgt     titas30c
296500991021     c     kann          readpe    titas30c                               23
296600991019     c  n23              setoff                                       29
296700991021    4c                   endif
296800991021    3c                   endif
296900080408     c
297000080408     c* per "I0" e codice bolla "Z" cerco la bolla come A2 e poi
297100080408     c*  aggiorno secondo i modi già previsti in AGARBI
297200140407    3c                   if        (comcvb='I0' or
297300140407     c                              comcvb='I7' or
297400140407     c                              comcvb='I8' or
297500140407     c                              comcvb='I9' )
297600140407    3c                              and arbcbo='Z'
297700080408     C                   MOVEL     'A2'          COMTBL
297800080408     C     KBLP          chain     TITAS30C
297900080408    4c                   if        %found(titas30c)
298000080408     c                   setoff                                       2923
298100080408    4c                   endif
298200080408    3c                   endif
298300110214     c* per "I0" e codice bolla "A" cerco la bolla come F1 e poi
298400110214     c*  aggiorno secondo i modi già previsti in AGARBI
298500140407    3c***                if        comcvb='I0' and (arbcbo='A' or arbcbo='Q')
298600140407    3c                   if        (comcvb='I0' or
298700140407     c                              comcvb='I7' or
298800140407     c                              comcvb='I8' or
298900140407     c                              comcvb='I9' )
299000140407     c                             and (arbcbo='A' or arbcbo='Q')
299100110214     C                   MOVEL     'F1'          COMTBL
299200110214     C     KBLP          chain     TITAS30C
299300110214    4c                   if        %found(titas30c)
299400110214     c                   setoff                                       2923
299500110214    4c                   endif
299600110214    3c                   endif
299700160714     c
299800160714     c* se si tratta di causale variazione TB non stampo: la bolla è in arrivo
299900160714     c*  solo per tipo bolla AR
300000160714     c                   if        comcvb='TB' and §3atb1='AR'
300100160714     c                   setoff                                       23
300200160714     c                   seton                                        37
300300160714     c                   endif
300400930514     C*
300500941121    3C   23COMTBL        IFEQ      §3ATB1
300600991015     C                   MOVEL     ERR(1)        ARBDER
300700920130     C                   ELSE
300800991015     C                   MOVEL     ERR(7)        ARBDER
300900941121    3C                   END
301000941121    2C                   END
301100941130     C**
301200941130     C* 23 ON --> STAMPO ERRORE
301300941130    2C     *IN23         IFEQ      *ON
301400920602     C  N42
301500920602     CAN 40              SETON                                        37
301600920212     C*        PER C.E.D.
301700920603     C  N42
301800920603     CAN 40
301900920603     CORN40              Z-ADD     102           COMFEV
302000920602     C  N42
302100920602     CAN 40
302200920602     CORN40              EXSR      WTRERR
302300941121    2C                   END
302400941121     C*
302500991019    1C                   Endif
302600941121     C*
302700991019    1c     *in29         ifeq      *off
302800941121     C* 29 OFF - S P E D I Z I O N E       E S I S T E N T E
302900121108     c* Salvo contenuto del tracciato record di titas in ds apposita:
303000121108     c* serve per la scrittura dell'immagine di titas prima della prima variazione
303100121108     c* nel file TIVRB00F
303200121108     c                   exsr      memo_dstitas
303300941121     C*
303400941121     C* 02 OFF             --> GIA' CONTABILIZZATA IN BLTAS10 O BLTASP
303500941130     C* 02 ON  MA TASFIC=1 --> GIA' CONTABILIZZATA IN BLTAS00
303600930512     C*
303700941130    2C     *IN02         IFEQ      *OFF
303800941121     C     *IN02         OREQ      *ON
303900941130     C     TASFIC        ANDNE     ' '
304000941130     C**
304100950202     C* VARIAZIONE "Ix" --> indirizzo/ peso/ volume
304200941130     C**
304300941130    3C     FLGCVB        IFEQ      'I'
304400941130     C*
304500950202     C* SE VARIO PESO/VOLUME      E NON E' LA 2 BOLLA --> ERRORE
304600960416    4C     COMCVB        IFNE      'I0'
304700140407    4C     COMCVB        andne     'I7'
304800140407    4C     COMCVB        andne     'I8'
304900140407    4C     COMCVB        andne     'I9'
305000941130     C*
305100991021    5C     §3ATB2        IFNE      tastbl
305200930513     C                   SETON                                        29
305300930804     C*
305400140219    6c***  TASPKB        IFNE      ARBPKF
305500130325     C***                MOVEL     ERR(9)        ARBDER
305600130325     C***                EXSR      WTRERR
305700120313     c                   exsr      ctr_vrb
305800140219   X6C***                ELSE
305900930603     C*
306000140219    7C***  TASFVF        IFNE      'T'
306100140219     C***  TASVLF        ANDNE     ARBVLF
306200140219    8C***  TASFVF        IFEQ      'Z'
306300140219     C***  TASVLF        ANDLT     ARBVLF
306400140219     C***  TASFVF        ORNE      'Z'
306500130325     C***                MOVEL     ERR(9)        ARBDER
306600130325     C***                EXSR      WTRERR
306700140219     c***                eval      wcntab='S'
306800140219     c***                exsr      ctr_vrb
306900140219    8C***                END
307000140219    7C***                END
307100930603     C*
307200140219    6C***                END
307300930603     C*
307400941130    5C                   ENDIF
307500941130     C*
307600930512   X4C                   ELSE
307700930512     C*
307800950308RM*  C*§SE VARIATO FERMO DEPOSITO E NON E' LA 2 BOLLA --> ERRORE
307900950308RM* 5C*§         TASFFD    IFNE ARBFFD
308000950308RM*  C*§         §3ATB2    ANDNECOMTBL
308100950308RM*  C*§                   MOVELERR,9     ARBDER
308200950308RM*  C*§                   EXSR WTRERR
308300950308RM* 5C*§                   END
308400990625     C**
308500990625     C* MEMORIZZO ULTIME 4 CIFRE CODICE FATTURAZIONE 2 BOLLA
308600991021    4C     §3ATB2        IFEQ      tastbl
308700990625     C                   MOVE      TASKSC        KSC42BO           4 0
308800930512    4C                   END
308900990625     C*
309000990625    4C                   END
309100930512    3C                   END
309200950202     C**
309300950202     C* VARIAZIONE "Cx" --> consegne particolari
309400950202     C**
309500950202    3C     FLGCVB        IFEQ      'C'
309600950202     C* no errore se 2 bolla
309700991021    4C     §3ATB2        IFNE      tastbl
309800991019     c
309900991019     C                   MOVEL     'TB'          COD
310000991021     C                   MOVEL(P)  tastbl        KEY
310100991019     C     KTAB          CHAIN     TABEL                              32
310200991019     C  N32              MOVEL     TBLUNI        DSTB
310300991019     C   32              MOVEL     *ALL'1'       DSTB
310400991019   4aC     §TBFCB        IFEQ      '1'
310500950202     C*
310600950202     C* STAMPO SOLO SE UNO DEI TRE VARIATO
310700950202    5C     TASFTC        IFEQ      ARBTC1
310800950202     C     TASTC2        ANDEQ     ARBTC2
310900950202     C     TASFTC        OREQ      ARBTC2
311000950202     C     TASTC2        ANDEQ     ARBTC1
311100950202     C*
311200950202   X5C                   ELSE
311300950202     C* CONTROLLO SE LA TARIFFA TASSA LE CONSEGNE PARTICOLARI
311400140219     C***                EXSR      CONPAR
311500950202     C*
311600140219    6C**   *IN17         IFEQ      *ON
311700130325     C****               MOVEL     ERR(9)        ARBDER
311800130325     C****               EXSR      WTRERR
311900120320     c                   exsr      ctr_vrb
312000140219    6C**                ENDIF
312100950202    5C                   ENDIF
312200950202     C*
312300991019   4ac                   ENDIF
312400950202    4C                   ENDIF
312500950202    3C                   ENDIF
312600941130     C**
312700941130     C* VARIAZIONE "Tx" --> tassazione 1 bolla
312800941130     C**
312900941130    3C     FLGCVB        IFEQ      'T'
313000950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
313100130326     C***  ARBFIV        IFGT      0
313200130326     C***  TASFIV        ANDNE     ARBFIV
313300950324     C                   Z-ADD     46            COMFEV
313400130326     C***                ENDIF
313500950324     C*
313600930513     C                   MOVEL     ERR(9)        ARBDER
313700990505     c                   eval      wdelarbu='S'
313800920220     C                   EXSR      WTRERR
313900920226     C                   SETON                                        29
314000011029     c                   if        tasias > 0  and tasvas = *blanks
314100011029     c                   movel     'ITL'         tasvas
314200011029     c                   endif
314300990419     c* Controllo se variato l'importo da assicurare se contabilizzata     da assicurare
314400990419     c                   if        *in13 and (arbias <> tasias or
314500990419     c                             arbvas <> tasvas)
314600990419     c*
314700990419     c                   exsr      AGGIAS
314800990419     c*
314900990419     c                   endif
315000941130    3C                   END
315100941130     C**
315200941130     C* VARIAZIONE "2x" --> tassazione 2 bolla
315300941130     C**
315400941130    3C     FLGCVB        IFEQ      '2'
315500991021     C     tastbl        ANDEQ     §3ATB2
315600950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
315700130326     C**   ARBFIV        IFGT      0
315800130326     C**   TASFIV        ANDNE     ARBFIV
315900950324     C                   Z-ADD     46            COMFEV
316000130326     C**                 ENDIF
316100950324     C*
316200950203     C                   MOVEL     §3ATB2        TBLERR
316300930513     C                   MOVEL     ERR(9)        ARBDER
316400920226     C                   EXSR      WTRERR
316500920226     C                   SETON                                        29
316600941130    3C                   END
316700950202     C**
316800950202     C* VARIAZIONE "Mx" --> mittente
316900990628     c*                     la variazione del mittente dipende dalla conab
317000990628     c*                     della prima bolla, a prescindere da come è messa
317100990628     c*                     la seconda: se aggiono la  aggiorno anche la 2
317200950202     C**
317300950202    3C     FLGCVB        IFEQ      'M'
317400991021    5C     §3ATB2        ANDNE     tastbl
317500130326     C                   Z-ADD     46            COMFEV
317600950202     C                   MOVEL     ERR(9)        ARBDER
317700950202     C                   EXSR      WTRERR
317800950202     C                   SETON                                        29
317900950202    3C                   END
318000941130    2C                   END
318100930512    1C                   END
318200941130     C*
318300920226     C                   ENDSR
318400920228     C**---------------------------------------------------------
318500920228     C** CHAIN SU PRIMA BOLLA
318600920228     C**---------------------------------------------------------
318700920228     C     CHTB1         BEGSR
318800920228     C                   MOVEL     SAVTB1        COMTBL
318900941130     C                   SETOFF                                       020304
319000990625     C     KBLP          CHAIN     TITAS30C                           39
319100121108     c                   exsr      memo_dstitas
319200920228     C*
319300990414     C                   EXSR      CTR3
319400920228     C                   ENDSR
319500920228     C**---------------------------------------------------------
319600920228     C** CHAIN SU SECONDA BOLLA
319700920228     C**---------------------------------------------------------
319800920228     C     CHTB2         BEGSR
319900920228     C                   MOVEL     SAVTB2        COMTBL
320000941130     C                   SETOFF                                       020304
320100990625     C     KBLP          CHAIN     TITAS30C                           39
320200121108     c                   exsr      memo_dstitas
320300920228     C*
320400990414     C                   EXSR      CTR3
320500920228     C                   ENDSR
320600920226     C**---------------------------------------------------------
320700920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
320800920226     C**---------------------------------------------------------
320900920226     C     CTR2          BEGSR
321000920302     C                   MOVEL     COMTBL        FLGTB1            1
321100950202     C* SPEDIZIONE 1) GIA' IMPONIBILE     SE NO FATTURA E NO ASSEGNATO
321200950202     C*               SE VARIAZOINE DA ARRIVO
321300950202     C*            2) GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
321400950202     C*            --> NON POSSO VARIARE
321500950202    1C     TASDFT        IFGT      0
321600950202    2C     TASFIC        IFNE      ' '
321700920226     C                   SETON                                        29
321800130326     c                   z-add     46            comfev
321900991015     C                   MOVEL     ERR(9)        ARBDER
322000920226     C                   EXSR      WTRERR
322100950202    2C                   END
322200920226     C*
322300950202   X1C                   ELSE
322400920226     C*
322500990624    2C     TASimv        IFNE      *zeros
322600950202     C     COMCVB        ANDNE     'TP'
322700950202    3C     FLGTB1        ANDNE     'A'
322800920302     C                   SETON                                        29
322900920226     C*
323000991015     C
323100991015     C   29              MOVEL     ERR(8)        ARBDER
323200130326     c   29              z-add     46            comfev
323300920226     C   29              EXSR      WTRERR
323400950202    2C                   END
323500920302     C*
323600950202    1C                   END
323700920226     C                   ENDSR
323800920226     C**---------------------------------------------------------
323900920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
324000941130     C** PER LE VAIRAZIONI C/ASSEGNO O TIPI BOLLA
324100920226     C**---------------------------------------------------------
324200920226     C     CTR3          BEGSR
324300920303     C                   SETOFF                                       2938
324400920226     C* SPEDIZIONE GIA' IMPONIBILE     SE NO FATTURA
324500920226     C*            GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
324600920226     C*            NON POSSO VARIARE
324700920226     C*
324800990414     C  N02              SETON                                        2936
324900941202     C   02TASDFT        IFGT      0
325000990414     C     TASFIC        ANDNE     ' '
325100920227     C                   SETON                                        2936
325200920226     C*
325300920226     C                   ELSE
325400920226     C*
325500990624     C     TASimv        IFNE      *zeros
325600920226     C                   SETON                                        38
325700920226     C                   END
325800920226     C*
325900920226     C                   END
326000920226     C                   ENDSR
326100920211     C**---------------------------------------------------------
326200920211     C** SCRIVO FILE ERRORI
326300920211     C**---------------------------------------------------------
326400920211     C     WTRERR        BEGSR
326500110208     c                   clear                   arbdat
326600920211     C*
326700970916     C* CREO IL RECORD DI ERRORE SOLO SE LA DATA VARIAZIONE E' INFERIOR
326800970916     C* DI 3 GIORNI A OGGI PER LE STAMPA DI SEDE
326900000703    0C     COMFEV        IFEQ      102
327000110208     C     ARBDTV        ANDLE     DATAM3
327100000703     c     *in37         andeq     *on
327200100225
327300110208     c* Se si tratta di codice TP con importo da assicurare, stampo subito
327400100225    0C     COMFEV        oreq      102
327500100225     c     *in13         andeq     *on
327600100225     c     arbias        andgt     0
327700970916     C*é*
327800970916     C     COMFEV        ORNE      102
327900000703     C*é*
328000000703     C     *in37         ORNE      *on
328100970916     C**
328200991015     C     KERR          SETLL     FIARBE2C                               01
328300930702     C* SE NON TROVATO ERRORE
328400930706    1C     *IN01         IFEQ      *OFF
328500110208     c
328600110208     c* Se l'errore è bolla inesistente, aspetto 3 gg prima di ricrearlo
328700110208    2c                   if        arbder=err(1)
328800110208     c                   eval      arbdat=dateu
328900110208     C     KERR          SETLL     FIARBE2C                               01
329000110208    3c                   if        not  *in01
329100110208     c                   eval      arbdat=datam1
329200110208     C     KERR          SETLL     FIARBE2C                               01
329300110208    4c                   if        not  *in01
329400110208     c                   eval      arbdat=datam2
329500110208     C     KERR          SETLL     FIARBE2C                               01
329600110208    4c                   endif
329700110208    3c                   endif
329800110208    2c                   endif
329900110208    1c                   endif
330000110208     c
330100110208    1C     *IN01         IFEQ      *OFF
330200110209     c                   clear                   arbdat
330300930702     C* RISCRIVO ERRORE
330400950203     C* SE NON L'HO GIA' IMPOSTATO CERCO FILIALE A CUI INVIARE STAMPA
330500930706    2C     COMFEV        IFEQ      0
330600920212     C*
330700950203    3C     TBLERR        IFEQ      'F'
330800991015     C                   MOVEL     ARBLNP        ARBFEV
330900930706   X3C                   ELSE
331000991015     C                   MOVEL     TASLNA        ARBFEV
331100930706    3C                   END
331200971215     C* PRENDO TERMINAL DI PARTENZA
331300971215     C                   CLEAR                   DSLV55
331400971215     C                   MOVEL     'P'           D55TPT
331500991015     C                   MOVE      ARBFEV        D55LIN
331600971215     C                   Z-ADD     DATEU         D55DRF
331700971215     C                   CALL      'FNLV55R'
331800971215     C                   PARM                    DSLV55
331900971215     C     D55ERR        IFEQ      *BLANKS
332000991015     C                   MOVE      D55TFP        ARBFEV
332100971215     C                   END
332200920212     C*
332300930706   X2C                   ELSE
332400920212     C*
332500991015     C                   Z-ADD     COMFEV        ARBFEV
332600930706    2C                   END
332700991015     C     ARBFEV        IFEQ      102
332800070420     C*********          MOVE      MBR           ARBLNA
332900950202     C                   ELSE
333000950202     C                   MOVEL     TASLNA        ARBLNA
333100950202     C                   ENDIF
333200920211     C*
333300950201     C   11              WRITE     FNARBDEE
333400920212     C*
333500950201     C   12              WRITE     FNARBKEE
333600950201     C* SCRIVO ANCHE FNARBM0E SOLO SE RICHIESTO
333700930706    2C   12*IN36         IFEQ      '1'
333800950202     C     KARBM         CHAIN     FNARBMRR                           32
333900950201     C  N32              WRITE     FNARBMEE
334000920214     C                   SETOFF                                       36
334100930706    2C                   END
334200920212     C*
334300950203     C     *IN13         IFEQ      *ON
334400991015     C                   WRITE     FIARBTEE
334500950203     C*
334600991015     c* se devo cancellare fiarbu, lo faccio
334700991018     c     arbtrc        ifeq      '1'
334800991015     C     KARBU         SETLL     FIARBU1R
334900991015     C     KARBU         READE     FIARBU1R                               30
335000950203     C*
335100950203     C     *IN30         DOWEQ     *OFF
335200991015     C                   WRITE     FIARBUEE                             30
335300950203     C* SE POSSO LI DELETO
335400990505     c  n37              if        wdelarbu='S'
335500990505     C                   ADD       1             NUMRBU
335600991015     C                   DELETE    FIARBURR
335700990505     c                   endif
335800950203     C*
335900991015     C     KARBU         READE     FIARBU1R                               30
336000950203     C                   ENDDO
336100991018     c                   endif
336200950203     C                   ENDIF
336300950203     C*
336400950202     C   15              WRITE     FNARBGEE
336500950202     C   16              WRITE     FNARBMEE
336600991214   X1c                   else
336700991214     c* se devo cancellare fiarbu, lo faccio
336800991214    2c     *in37         ifeq      *off
336900991214     c     wdelarbu      andeq     'S'
337000991214     c     arbtrc        andeq     '1'
337100991214     C     KARBU         SETLL     FIARBU1R
337200991214     C     KARBU         READE     FIARBU1R                               30
337300991214     C*
337400991214    3C     *IN30         DOWEQ     *OFF
337500991214     C                   ADD       1             NUMRBU
337600991214     C                   DELETE    FIARBURR
337700991214     C     KARBU         READE     FIARBU1R                               30
337800991214    3C                   ENDDO
337900991214    2c                   endif
338000930706    1C                   ENDIF
338100970916    0C                   ENDIF
338200920212     C*
338300920212     C                   Z-ADD     0             COMFEV            3 0
338400950203     C                   MOVEL     §3ATB1        TBLERR
338500920211     C                   ENDSR
338600930514     C**---------------------------------------------------------
338700930514     C** CONTROLLO SE LA SPEDIZIONE INESISTENTE E' STORNATA
338800930514     C**---------------------------------------------------------
338900930514     C     STORNO        BEGSR
339000930514     C*
339100930514     C* SE E' SINGOLA BOLLA
339200930514     C     §3ATB2        IFEQ      '  '
339300930514     C*
339400991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
339500991021     c* (cioè il franco) faccio setgt e readpe invece che chain
339600991021     C     KANN          setgt     TITAS30C
339700991021     C     KANN          readpe    TITAS30C                               23
339800931119     C*
339900941130     C     *IN23         IFEQ      *OFF
340000930514     C                   MOVEL     'TB'          COD
340100941130     C                   MOVEL(P)  TASTBL        KEY
340200930514     C     KTAB          CHAIN     TABEL                              32
340300930514     C  N32              MOVEL     TBLUNI        DSTB
340400930514     C   32              MOVEL     *ALL'1'       DSTB
340500930514     C*
340600930514     C* SE NON E' STORNO STAMPO ERRORE
340700930514     C     §TBFCB        IFEQ      '1'
340800930514     C                   SETON                                        23
340900930514     C                   ELSE
341000930514     C                   MOVEL     '1'           ST1               1
341100930514     C                   END
341200930514     C                   END
341300930514     C*
341400930514     C                   END
341500930514     C                   ENDSR
341600930804     C*
341700950202     C**---------------------------------------------------------
341800950202     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE
341900950202     C**---------------------------------------------------------
342000930804     C     CONPAR        BEGSR
342100930804     C                   SETOFF                                       17
342200930804     C*
342300930804     C                   MOVE      TASKSC        KSC4              4 0
342400930804     C*
342500930804     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
342600930804    2C     KSC4          IFEQ      9999
342700930804     C     KSC4          OREQ      8888
342800930804     C                   SETON                                        17
342900930804     C*
343000930804   X2C                   ELSE
343100930804     C*
343200930804     C                   Z-ADD     999           TARPRG
343300930804     C                   Z-ADD     999           PREPRG
343400991021     C                   MOVEL     tastbl        UNOTB1
343500930804     C*
343600930805     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
343700930805     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
343800930804    3C     UNOTB1        IFEQ      'F'
343900930805     C                   MOVEL     'A'           TFP               1
344000930804     C                   ELSE
344100930805     C                   MOVEL     'F'           TFP
344200930804    3C                   END
344300930804     C*
344400930804     C* SE C'E' A ZERO NON DO' L'ERRORE
344500941130     C     KTAM          SETLL     TNTAM000
344600941130     C     KTAM          READE     TNTAM000                               35
344700930804     C*
344800930804    3C     *IN35         DOWEQ     *OFF
344900930804     C*
345000930804     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
345100930804    4C     TAMATB        IFNE      'A'
345200930804     C*
345300941205    5C     TAMBAP        IFNE      'B'
345400941205     C     TAMBAP        ANDNE     TFP
345500930804     C*
345600930804     C                   MOVE      TAMPRG        TARPRG            3 0
345700930804     C*
345800930804     C* VEDO SE E' TARIFFA VALIDA
345900121116     c                   movel     tasaas        tasdsp
346000121116     c                   move      tasmgs        tasdsp
346100120430    6C     TASDSP        IFGE      TAMDDT
346200120430     C     TASDSP        ANDLE     TAMDST
346300120430     C                   SETON                                        35
346400120430    6C                   END
346500930804     C*
346600930804   X5C                   ELSE
346700930804     C                   MOVE      TAMPRG        PREPRG            3 0
346800930804    5C                   END
346900930804    4C                   END
347000930804     C*
347100941130     C  N35KTAM          READE     TNTAM000                               35
347200930804    3C                   END
347300930804     C*
347400930804     C* NON TROVATA TARIFFA: STAMPO ERRORE
347500930804    3C     TARPRG        IFEQ      999
347600930804     C     PREPRG        ANDEQ     999
347700930804     C                   SETON                                        17
347800930804   X3C                   ELSE
347900930804     C*
348000930804    4C     TARPRG        IFNE      999
348100930804     C                   MOVEL     TARPRG        PRG
348200930804   X4C                   ELSE
348300930804     C                   MOVEL     PREPRG        PRG
348400930804    4C                   ENDIF
348500930804     C*
348600930804     C* VEDO SE TARIFFA PARTICOLARE A 0
348700950202     C     ARBTC1        IFNE      ' '
348800990719     C                   MOVEL(P)  ARBTC1        WFTC
348900950202     C                   EXSR      PARTIC
349000950202     C                   ENDIF
349100950202     C  N17ARBTC2        IFNE      ' '
349200990719     C                   MOVEL(P)  ARBTC2        WFTC
349300950202     C                   EXSR      PARTIC
349400950202     C                   ENDIF
349500950202     C*
349600930804    3C                   END
349700930804    2C                   END
349800930804     C*
349900930804     C                   ENDSR
350000950202     C**---------------------------------------------------------
350100950202     C* CONTROLLO IN TPT
350200950202     C**---------------------------------------------------------
350300950202     C     PARTIC        BEGSR
350400950202     C*
350500990719     C     KTAM2         CHAIN     TITPT000                           17
350600950202     C* 17 ON - NON ESISTE TARIFFA
350700950202    4C     *IN17         IFEQ      *OFF
350800950202     C*         NON ESISTE PERCHE' ANNULLATA
350900950202    5C     TPTATB        IFEQ      'A'
351000950202     C                   SETON                                        17
351100950202   X5C                   ELSE
351200950202     C* VEDO SE ESISTE DETTAGLIO TARIFFA PARTICOLARE
351300990719     C     KTAM2         SETLL     TITPD000
351400990719     C     KTAM2         READE     TITPD000                               35
351500950202     C*
351600950202    6C     *IN35         DOWEQ     *OFF
351700950202     C* NO ANNULLATO
351800950202    7C     TPDATB        IFEQ      ' '
351900950202     C* SE > 0 ERRORE
352000950202    8C     TPDITR        IFNE      0
352100950202     C     TPDMIN        ORNE      0
352200950202     C     TPDMAX        ORNE      0
352300950202     C                   SETON                                        1735
352400950202    8C                   END
352500950202    7C                   END
352600950202     C*
352700990719     C  N35KTAM2         READE     TITPD000                               35
352800950202    6C                   END
352900950202    5C                   END
353000950202    4C                   END
353100950202     C                   ENDSR
353200941130     C**---------------------------------------------------------
353300941130     C* CONTROLLO SE ESISTE VARIA "G"
353400941130     C**---------------------------------------------------------
353500941130     C     ESVARG        BEGSR
353600941130     C                   CLEAR                   WSIVAR
353700941130     C*
353800941130     C                   SETON                                        30
353900941130    3C     TASSV1        IFNE      'G'
354000941130     C     TASSV2        ANDNE     'G'
354100941130     C     TASSV3        ANDNE     'G'
354200941130     C                   MOVEL     'G'           WSV1
354300990625     C     KBLPV         SETLL     TITA730C                               30
354400941130    3C                   ENDIF
354500941130     C*
354600941130     C     *IN30         IFEQ      *ON
354700941130     C                   MOVEL     'S'           WSIVAR            1
354800941130     C                   ENDIF
354900941130     C                   ENDSR
355000941202     C**---------------------------------------------------------
355100941202     C* CONTROLLO SE HO MODIFICATO IL DESTINATARIO -------------------*
355200941202     C**---------------------------------------------------------
355300941202     C     CTRDES        BEGSR
355400941202     C*
355500950202     C                   MOVEL     CSBRSD        ALF35            35
355600941202     C* CONTROLLO LA 1 POSIZIONE <> ' '
355700950202     C     ' '           CHECKR    ALF35         C                 3 0    30
355800950202     C                   MOVE      ALF35         W001A             1
355900941202     C*
356000941202     C* CONFRONTO CON IL CAMPO CHE HO RICEVUTO
356100941202     C     *IN30         IFEQ      *ON
356200941215     C     W001A         ANDEQ     ' '
356300950202     C                   MOVEA     ARBRSD        K35
356400941202     C                   ADD       1             C
356500950202     C                   MOVEA     *BLANKS       K35(C)
356600950202     C                   MOVEA     K35           W035A            35
356700941202     C                   ELSE
356800950202     C                   MOVEL     ARBRSD        W035A
356900941202     C                   ENDIF
357000941202     C*
357100950202     C     W035A         IFEQ      ALF35
357200941202     C                   SETOFF                                       31
357300941202     C                   END
357400941202     C*
357500941202     C                   ENDSR
357600990415     C**------------------------------------------------------------------------------
357700990415     C** VARIAZIONE IMPORTO D'ASSICURARE PER SPEDIZIONE GIA' TASSATA / CONTABILIZZATA
357800990415     C**------------------------------------------------------------------------------
357900990415     c*
358000990419     c     AGGIAS        begsr
358100990415     c*
358200990415     c* controllo se la spedizione è già stata fatturata / contabilizzata oppure
358300990415     c* se è stato calcolato l'imponibile
358400990415     c*
358500990415     c                   exsr      ctr3
358600990415     c* se è acceso il  38 vuol dire che non è stata contabilizzata ma è stato calcolato
358700991018     c* fino all'imponibile e quindi devo andare a controllare se è già stata
358800990415     c* calcolata la varia "R"
358900990519     c                   select
359000990519    1c                   when      *in38
359100990421     c*
359200990421     c                   z-add     tasias        arbiap
359300990421     c                   movel     tasvas        arbvap
359400990421     c                   movel     'R'           arbvar
359500991019     c*
359600991019     c* verifico se devo registrare messagio per ufficio assicurazione
359700991019     C* relativo a spedizione avente tipo bolla non assicurabile per la
359800991019     c* quale è stato tolto, modificato o aggiunto l'importo da assicurare
359900991019     c* Escludo le bolle di recupero
360000991019     c     tblrbl        ifne      'R'
360100991019     c                   exsr      msgass
360200991019     c                   endif
360300131129
360400131129     c                   clear                   arbiap
360500131129     c                   clear                   arbvap
360600131129     c                   clear                   arbvar
360700990415     c*
360800990824     c* Non faccio niente se spedizione:
360900990824     c* - contabilizzata
361000990824     c* - tasias> 0
361100990824     c* - tasfcm="F" (tasias immesso dalla sede (dalla fatturazione))
361200990824     c* - arbias= 0
361300990824     c                   when      *in29 and tasias > 0 and tasfcm = 'F'
361400990824     c                                   and arbias = 0
361500990824     c*
361600990519     c* se è acceso il 29  vuol dire che è stata CONTABILIZZATA
361700990519    1c                   when      *in29
361800990505     c                   eval      wdelarbu='S'
361900990415     c
362000990415     c* verifico l'esistenza della varia "R"
362100990415     c                   exsr      esvarr
362200990419     c*
362300990419     c                   z-add     tasias        arbiap
362400990419     c                   movel     tasvas        arbvap
362500990419     c                   movel     'R'           arbvar
362600990415     c*
362700990519    2c                   SELECT
362800990415     c*
362900990415     c* importo in TAS = SI
363000990415     c* importo in ARB = NO
363100990415     c* varia   "R"    = SI
363200990415     c*                            errore no tolta varia "R"
363300991019     c                   when      tasias > 0 and arbias = 0
363400130325     c***                if        Wvariar = 'S'
363500130325     c***                movel     err(34)       arbder
363600130325     c***                exsr      WTRERR
363700130325     c***                endif
363800121203     c                   exsr      ctr_vrb
363900990415     c* messaggio all'Ufficio Assicurazione
364000990519     c* escludo le bolle di recupero
364100990519     c                   if        tblrbl<>'R'
364200990415     c                   z-add     603           comfev
364300990415     c                   movel     err(32)       arbder
364400990419     c                   exsr      wtrerr
364500990519     c                   endif
364600990415     c*
364700990415     c* importo in TAS = NO
364800990415     c* importo in ARB = SI
364900990415     c* varia   "R"    = NO
365000990415     c*                            errore no aggiunta varia "R"
365100991019     c                   when      tasias = 0 and arbias > 0
365200130325     c                   if        wvariar = ' '
365300130325     c***                movel     err(35)       arbder
365400130325     c***                exsr      wtrerr
365500120423     c                   exsr      ctr_vrb
365600991019     c                   endif
365700990415     c* messaggio all'Ufficio Assicurazione
365800990519     c                   if        tblrbl<>'R'
365900990415     c                   z-add     603           comfev
366000990415     c                   movel     err(31)       arbder
366100990419     c                   exsr      wtrerr
366200990519     c                   endif
366300990415     c*
366400990415     c* importo in TAS = SI
366500990415     c* importo in ARB = SI  ma diversa da tas
366600990415     c* varia   "R"    = SI
366700990415     c*                            errore no modificata varia "R"
366800991019     c                   when      tasias <> arbias
366900991019     c                   if        wvariar = 'S'
367000130325     c***                movel     err(36)       arbder
367100130325     c***                exsr      wtrerr
367200120423     c                   exsr      ctr_vrb
367300991019     c                   endif
367400990415     c* messaggio all'Ufficio Assicurazione
367500990519     c                   if        tblrbl<>'R'
367600990415     c                   z-add     603           comfev
367700990415     c                   movel     err(33)       arbder
367800990419     c                   exsr      wtrerr
367900990519     c                   endif
368000990415     c*
368100990519    2c                   endsl
368200990415     c*
368300990624     c* aggiorno in TITAS il valore dell'importo d'assicurare
368400990415     c                   z-add     arbias        tasias
368500990415     c                   movel     arbvas        tasvas
368600990824     c                   clear                   tasfcm
368700990415     c*
368800990624     c   02              update    titas000
368900990624     c   03              update    titas010
369000990624     c   04              update    titasp00
369100990419     c*
369200990419     c                   clear                   arbiap
369300990419     c                   clear                   arbvap
369400990419     c                   clear                   arbvar
369500990419     c**
369600990415     c* riaccendo il 29 e spengo il 36
369700990415     c                   seton                                        29
369800990415     c                   setoff                                       36
369900991019     c* Se e' bolla normale, ma non compare nel tabulato per la
370000991019     c*  Consuldanni, stampo solo per l'ufficio assicurazione
370100990519     c                   other
370200990519     c                   if        tblrbl<>'R'
370300000410     c                   z-add     tasias        arbiap
370400000410     c                   movel     tasvas        arbvap
370500000410     c                   movel     'R'           arbvar
370600991019     c                   exsr      msgass
370700000410     c                   clear                   arbiap
370800000410     c                   clear                   arbvap
370900000410     c                   clear                   arbvar
371000990519     c                   endif
371100990519    1c                   ENDSL
371200990415     c*
371300990415     c                   endsr
371400990415     c*
371500991019     C**-------------------------------------------------------------
371600991019     C** messaggi per ufficio Assicurazione per tipo bolla non assic.
371700991019     C**-------------------------------------------------------------
371800991019     C     msgass        BEGSR
371900991019     C                   MOVEL     'TB'          COD
372000991019     c                   movel(P)  tastbl        key
372100991019     C     KTAB          CHAIN     TABEL                              32
372200991019     C  N32              MOVEL     TBLUNI        DSTB
372300991019     C   32              clear                   DSTB
372400991019     c* se bolla non assicurabile e aggiunto, tolto o modificato importo
372500991019     c* da assicurare devo stampare messaggio per ufficio Assicurazione
372600991019    1c     §tbfas        ifeq      '0'
372700991019    2c                   select
372800991019     c* tolto importo da assicurare
372900991019   w1c                   when      tasias > 0 and arbias = 0
373000991019     c                   z-add     603           comfev
373100991019     c                   movel     err(41)       arbder
373200991019     c                   exsr      wtrerr
373300991019     c* aggiunto importo da assicurare
373400991019   w2c                   when      tasias = 0 and arbias > 0
373500991019     c                   z-add     603           comfev
373600991019     c                   movel     err(40)       arbder
373700991019     c                   exsr      wtrerr
373800991019     c* modificato importo da assicurare
373900991019   w3c                   when      tasias <> 0
374000991019     c                   z-add     603           comfev
374100991019     c                   movel     err(42)       arbder
374200991019     c                   exsr      wtrerr
374300991019    2c                   endsl
374400991019    1c                   endif
374500991019     c                   endsr
374600990415     C**---------------------------------------------------------
374700990415     C* CONTROLLO SE ESISTE VARIA "R"
374800990415     C**---------------------------------------------------------
374900990415     C     ESVARR        BEGSR
375000990415     C                   CLEAR                   WVARIAR           1
375100990415     C*
375200990415     C                   SETON                                        30
375300990415    3C     TASSV1        IFNE      'R'
375400990415     C     TASSV2        ANDNE     'R'
375500990415     C     TASSV3        ANDNE     'R'
375600990415     C                   MOVEL     'R'           WSV1
375700990624     C     KBLPV         SETLL     TITA730C                               30
375800990415    3C                   ENDIF
375900990415     C*
376000990415     C     *IN30         IFEQ      *ON
376100990415     C                   MOVEL     'S'           WVARIAR
376200990415     C                   ENDIF
376300990415     C                   ENDSR
376400941130     C**---------------------------------------------------------
376500941130     C* ROUTINE INIZIALE
376600941130     C**---------------------------------------------------------
376700941130     C     *INZSR        BEGSR
376800021014
376900941130     C     *ENTRY        PLIST
377000941130     C                   PARM                    KPJBA
377100021014
377200941130     C                   Z-ADD     1             CODUT             1 0
377300021014
377400021014     c     *dtaara       define    §azute        azuteds
377500021014     c     *dtaara       define    §datiute      ddatiute
377600021014     C                   in(E)     *dtaara
377700021014     C                   IF        %Error  or  RSUT = *blanks
377800021014     C                   CLEAR                   TIBS34DS
377900021014     C                   call      'TIBS34R'
378000021014     C                   parm                    Tibs34Ds
378100021014     C                   in        *dtaara
378200021014     c                   ENDIF
378300021014
378400021014     C                   Z-ADD     DUTKCI        KCI               4 0
378500021014
378600941130     C*---------------------------------------------------------------*
378700991018     C     KBLP          KLIST
378800950201     C                   KFLD                    ARBAAS
378900941130     C                   KFLD                    ARBLNP
379000941130     C                   KFLD                    ARBNRS
379100941130     C                   KFLD                    ARBNSP
379200941130     C                   KFLD                    COMTBL            2
379300990629     C     KTITA         KLIST
379400990629     C                   KFLD                    TASAAS
379500990629     C                   KFLD                    TASLNP
379600990629     C                   KFLD                    TASNRS
379700990629     C                   KFLD                    TASNSP
379800990629     C                   KFLD                    §TRC
379900941130     C     KBLPV         KLIST
380000950201     C                   KFLD                    ARBAAS
380100941130     C                   KFLD                    ARBLNP
380200941130     C                   KFLD                    ARBNRS
380300941130     C                   KFLD                    ARBNSP
380400941130     C                   KFLD                    COMTBL            2
380500941130     C                   KFLD                    WSV1
380600941130     C     KANN          KLIST
380700950201     C                   KFLD                    ARBAAS
380800941130     C                   KFLD                    ARBLNP
380900941130     C                   KFLD                    ARBNRS
381000941130     C                   KFLD                    ARBNSP
381100950202     C     KARBU         KLIST
381200941130     C                   KFLD                    ARBAAS
381300941130     C                   KFLD                    ARBLNP
381400941130     C                   KFLD                    ARBNRS
381500941130     C                   KFLD                    ARBNSP
381600991015     C                   KFLD                    ARBTRC
381700941130     C                   KFLD                    ARBDTV
381800941130     C                   KFLD                    ARBORV
381900950202     C     KARBM         KLIST
382000950202     C                   KFLD                    ARBAAS
382100950202     C                   KFLD                    ARBLNP
382200950202     C                   KFLD                    ARBNRS
382300950202     C                   KFLD                    ARBNSP
382400950202     C                   KFLD                    ARBCVB
382500950202     C                   KFLD                    ARBDTV
382600950202     C                   KFLD                    ARBORV
382700941130     C     KARBD         KLIST
382800941130     C                   KFLD                    ARBAAS
382900941130     C                   KFLD                    ARBLNP
383000941130     C                   KFLD                    ARBNRS
383100941130     C                   KFLD                    ARBNSP
383200941130     C                   KFLD                    ARBCVB
383300941130     C                   KFLD                    ARBDTV
383400941130     C                   KFLD                    COMORV            6 0
383500941130     C     KTAB          KLIST
383600941130     C                   KFLD                    CODUT
383700941130     C                   KFLD                    COD               2
383800941130     C                   KFLD                    KEY               8
383900941130     C     KACO          KLIST
384000941130     C                   KFLD                    CODUT
384100941130     C                   KFLD                    KCI
384200941130     C                   KFLD                    TASKSC
384300950202     C     KACO2         KLIST
384400950202     C                   KFLD                    CODUT
384500950202     C                   KFLD                    KCI
384600950202     C                   KFLD                    ARBCCM
384700981118     C     Kppa          KLIST
384800981118     C                   KFLD                    wsoc
384900981118     C                   KFLD                    wctb
385000981118     C                   KFLD                    wkcc
385100981118     C                   KFLD                    wksc
385200981118     C                   KFLD                    wdft
385300981118     C                   KFLD                    wnft
385400981118     C                   KFLD                    wfiv
385500981118     C     Kycpa         KLIST
385600981118     C                   KFLD                    ppasys
385700981118     C                   KFLD                    ppanrasint
385800941130     C     KERR          KLIST
385900941130     C                   KFLD                    ARBAAS
386000941130     C                   KFLD                    ARBLNP
386100941130     C                   KFLD                    ARBNRS
386200941130     C                   KFLD                    ARBNSP
386300941130     C                   KFLD                    ARBDER
386400941130     C                   KFLD                    ARBDAT
386500991214     c                   kfld                    ARBDTV
386600991214     c                   kfld                    ARBORV
386700941130     C     KTAM          KLIST
386800941130     C                   KFLD                    TASKSC
386900941130     C                   KFLD                    TASCTR
387000941130     C     KTAM2         KLIST
387100941130     C                   KFLD                    TASKSC
387200941130     C                   KFLD                    TASCTR
387300941130     C                   KFLD                    PRG               3 0
387400950202     C                   KFLD                    WFTC
387500151119     C     Kar5          KLIST
387600151119     C                   KFLD                    ARBAAS
387700151119     C                   KFLD                    ARBLNP
387800151119     C                   KFLD                    ARBNRS
387900151119     C                   KFLD                    ARBNSP
388000151119     C                   KFLD                    kar5trd
388100121108
388200121108
388300121108
388400941130     C**
388500941130     C* DEFINIZIONE CAMPI
388600941130     C**
388700960625     C     *LIKE         DEFINE    TASTBL        SAVTBL
388800990719     C     *LIKE         DEFINE    tptftc        WFTC
388900950202     C     *LIKE         DEFINE    TASVLF        SAVVLF
389000950202     C     *LIKE         DEFINE    TASVLF        SAVVLB
389100950202     C     *LIKE         DEFINE    TASFVF        SAVFVF
389200950202     C     *LIKE         DEFINE    TASFVF        SAVFVB
389300990625     C     *LIKE         DEFINE    TAScts        SAVcts
389400941130     C     *LIKE         DEFINE    TASSV1        WSV1
389500941130     C     *LIKE         DEFINE    TASVA1        WVA1
389600941130     C     *LIKE         DEFINE    ARBDER        SAVDER
389700941130     C     *LIKE         DEFINE    CSBPAR        DESPAR
389800970916     C     *LIKE         DEFINE    CSBPAR        DESPRV
389900010126     C     *LIKE         DEFINE    arblnp        savlnp
390000010126     C     *like         define    tasftc        WTCP
390100010126     C     *like         define    tasftc        WsvTCP
390200941130     C*---------------------------------------------------------------*
390300970916     C                   TIME                    W0140            14 0
390400970916     C                   MOVEL     W0140         UTIME             6 0
390500970916     C                   MOVE      W0140         WDTGIO            8 0
390600970916     C* UDATE IN AAAAMMGG
390700970916     C                   Z-ADD     WDTGIO        G02DAT
390800970916     C                   MOVEL     *BLANK        G02ERR
390900970916     C                   CALL      'XSRDA8'
391000970916     C                   PARM                    WLBDAT
391100970916     C                   MOVEL     G02INV        DATEU             8 0
391200110208     C* SOTTRAGGO  1
391300110208     C     G02TGI        SUB       1             GIOTGI
391400110208     C                   CALL      'XSRGI8'
391500110208     C                   PARM                    WGIDAT
391600110208     C                   Z-ADD     GIOINV        DATAM1            8 0
391700110208     C* SOTTRAGGO  2
391800110208     C     G02TGI        SUB       2             GIOTGI
391900110208     C                   CALL      'XSRGI8'
392000110208     C                   PARM                    WGIDAT
392100110208     C                   Z-ADD     GIOINV        DATAM2            8 0
392200970916     C* SOTTRAGGO  3
392300970916     C     G02TGI        SUB       3             GIOTGI
392400970916     C                   CALL      'XSRGI8'
392500970916     C                   PARM                    WGIDAT
392600110208     C                   Z-ADD     GIOINV        DATAM3            8 0
392700941130     C*
392800941130     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
392900941130     C* ASSUMO IL PRIMO MEMBRO DEL FILE
393000941130     C     MBRFIL        IFEQ      *BLANK
393100020827     C                   MOVEL     'M888888888'  MBRFIL
393200941130     C                   END
393300941130     C* MEMBRO LUNGO 3
393400941130     C                   MOVEL     ALFFIL        MBR               3 0
393500941130     C* PRENDO ANNO IN CORSO
393600941130     C                   MOVE      UDATE         AAS               2 0
393700070119     c* Apro file bolle di filiale
393800070119     c                   clear                   tibs55ds
393900070119     c                   eval      i50apo=001
394000070119     c                   call      'TIBS55R'
394100070119     c                   parm                    tibs55ds
394200070119     c*
394300070119     c                   Clear                   commt
394400070119     c                   Movel(p)  cmdt(1)       commt
394500070119     c                   Eval      %Subst(commt:30:10) = o50ala
394600070119     c                   Eval      commt =
394700070119     c                             %trim(commt) + '/FNBLP01L)'
394800070119     c                   Call      'QCMDEXC'
394900070119     c                   Parm                    commt
395000070119     c                   Parm                    lenght
395100070119     c                   Open      FNBLP01L
395200070119     c
395300070119     c
395400071102      * Aggancio tabella VPO record DECO
395500120510     c**********         Clear                   Dvpodeco
395600120510     c***********        Clear                   Tibs02ds
395700120510     c***********        Eval      T02tla = 'L'
395800120510     c***********        Eval      T02Mod = 'C'
395900120510     c*************      Eval      T02Sif = knsif
396000120510     c*************      Eval      T02Cod = 'VPO'
396100120510     c***********        Movel(p)  'DECO'        T02Ke1
396200120510     c*******            Call      'TIBS02R'
396300120510     c*********          Parm                    Kpjba
396400120510     c*********          Parm                    Tibs02ds
396500120510     c*********          If        T02Err = *Blanks
396600120510     c*********          Movel     T02Uni        Dvpodeco
396700120510     c**********         EndIf
396800071102
396900941130     C                   ENDSR
397000930804     C*
397100981001     C**-------------------------------------------------------------
397200981001     C** AGGIORNO CON CODICI DI VARIAZIONE DM DD BOLLA SEDE MITT/DEST
397300981001     C**-------------------------------------------------------------
397400981001     C     AGTMD         BEGSR
397500981001     C* CONTROLLO CHE TIPO DI VARIAZIONE STO LEGGENDO
397600981001     C* DM CREAZIONE MITTENTE BOLLA SEDE
397700981001     C* DD CREAZIONE DESTINATARIO BOLLA SEDE
397800981001     C* I0 VARIAZIONE DESTINATARIO
397900981001     C* MI VARIAZIONE MITTENTE
398000981001     C*
398100981001    1C     ARBCVB        IFNE      'DM'
398200981001     C     ARBCVB        ANDNE     'DD'
398300981001     C     ARBCVB        ANDNE     'I0'
398400140407     C     ARBCVB        ANDNE     'I7'
398500140407     C     ARBCVB        ANDNE     'I8'
398600140407     C     ARBCVB        ANDNE     'I9'
398700981001     C     ARBCVB        ANDNE     'MI'
398800981001     C*
398900981001   X1C                   ELSE
399000981001     C* CERCO LA SPEDIZIONE NEL FILE TNTMD
399100981001     C     KANN          CHAIN     TNTMD01L                           48
399200981001     C* IMMISSIONE O CAMBIO MITTENTE
399300981001     C* CAUSALI DI VARIAZIONI
399400981001    2C     ARBCVB        IFEQ      'DM'
399500981001     C     ARBCVB        OREQ      'MI'
399600981001     C*
399700981001     C                   MOVEL     ARBRSM        TMDRSM
399800981001     C                   MOVEL     ARBINM        TMDINM
399900981001     C                   MOVEL     ARBCAM        TMDCAM
400000981001     C                   MOVEL     ARBLOM        TMDLOM
400100981001     C                   MOVEL     ARBPRM        TMDPRM
400200981001     C                   MOVEL     ARBNZM        TMDNZM
400300981001     C*
400400981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
400500981001    3C     *IN48         IFEQ      '1'
400600981005     C     ARBCVB        ANDEQ     'DM'
400700981001     C                   Z-ADD     ARBAAS        TMDAAS
400800981001     C                   Z-ADD     ARBLNP        TMDLNP
400900981001     C                   Z-ADD     ARBNRS        TMDNRS
401000981001     C                   Z-ADD     ARBNSP        TMDNSP
401100981001     C                   WRITE     TNTMD000
401200981001     C*
401300981005    3C                   ENDIF
401400981005     C* PER 48 SPENTO AGGIORNO IL RECORD
401500981005     C  N48              UPDATE    TNTMD000
401600981001     C*
401700981001    2C                   ENDIF
401800981001     C*
401900981001     C* IMMISSIONE O CAMBIO DESTINATARIO
402000981001     C* CAUSALI DI VARIAZIONI
402100981001    2C     ARBCVB        IFEQ      'DD'
402200981001     C     ARBCVB        OREQ      'I0'
402300140407     C     ARBCVB        OREQ      'I7'
402400140407     C     ARBCVB        OREQ      'I8'
402500140407     C     ARBCVB        OREQ      'I9'
402600981001     C*
402700981001     C                   MOVEL     ARBRSD        TMDRSD
402800981001     C                   MOVEL     ARBIND        TMDIND
402900981001     C                   MOVEL     ARBCAD        TMDCAD
403000981001     C                   MOVEL     ARBLOD        TMDLOD
403100981001     C                   MOVEL     ARBPRD        TMDPRD
403200981001     C                   MOVEL     ARBNZD        TMDNZD
403300981001     C*
403400981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
403500981001    3C     *IN48         IFEQ      '1'
403600981005     C     ARBCVB        ANDEQ     'DD'
403700981001     C                   Z-ADD     ARBAAS        TMDAAS
403800981001     C                   Z-ADD     ARBLNP        TMDLNP
403900981001     C                   Z-ADD     ARBNRS        TMDNRS
404000981001     C                   Z-ADD     ARBNSP        TMDNSP
404100981001     C                   WRITE     TNTMD000
404200981005    3C                   ENDIF
404300981005     C*
404400981001     C* PER 48 SPENTO AGGIORNO IL RECORD
404500981005     C  N48              UPDATE    TNTMD000
404600981001     C*
404700981001    2C                   ENDIF
404800981001     C*
404900981001     C* SE LA VARIAZIONE ARB E' DD O DM DELETO IL RECORD E NON PROSEGUO CON
405000981001     C* LA LETTURA
405100981001    2C     ARBCVB        IFEQ      'DM'
405200981001     C     ARBCVB        OREQ      'DD'
405300981001     C   11              DELETE    FNARBD00
405400981001     C   16              DELETE    FNARBM00
405500981001     C                   MOVE      'Y'           DMDD
405600981001    2C                   ENDIF
405700981001     C*
405800981001    1C                   ENDIF
405900981001     C*
406000981001     C                   ENDSR
406100120312     C**-------------------------------------------------------------
406200120515     C** RITASSAZIONE DELLA SPEDIZIONE CON I NUOVI DATI RICEVUTI
406300120312     C**-------------------------------------------------------------
406400120313     C     ctr_vrb       BEGSR
406500151008     c* Richiamo pgm memorizzazione variazioni post-fatturazione
406600151008     c                   clear                   fnlvv1ds
406700151008     c                   clear                   parvar
406800151008     c                   clear                   partitas
406900151008     c                   clear                   parcsb
407000151008     c                   eval      iv1cvb=comcvb
407100151008     c                   eval      iv1pgm=nompgm
407200151008     c                   select
407300151008     c                   when      %subst(iv1cvb:1:1)='I'
407400151008     c     nrri          chain     fnarbd0r      dsarbd
407500151008     c                   if        %found(fnarbd0r)
407600151008     c                   eval      parvar =dsarbd
407700151008     c                   endif
407800151008     c                   when      %subst(iv1cvb:1:1)='K'
407900151008     c     nrri          chain     fnarbk0r      dsarbk
408000151008     c                   if        %found(fnarbk0r)
408100151008     c                   eval      parvar =dsarbk
408200151008     c                   endif
408300151008     c                   if        wanncas='S'
408400151008     c                   eval      iv1can='S'
408500151008     c                   endif
408600151008     c                   eval      parcsb  = dscsb
408700151008     c                   when      %subst(iv1cvb:1:1)='T'
408800151008     c     nrri          chain     fiarbt0r      dsarbt
408900151008     c                   if        %found(fiarbt0r)
409000151008     c                   eval      parvar =dsarbt
409100151008     c                   endif
409200151008     c                   when      %subst(iv1cvb:1:1)='C'
409300151008     c     nrri          chain     fnarbg0r      dsarbg
409400151008     c                   if        %found(fnarbg0r)
409500151008     c                   eval      parvar =dsarbg
409600151008     c                   endif
409700151008     c                   endsl
409800151008     c                   eval      kpjbu=fnlvv1ds
409900151009     c                   eval      partitas=dstitas
410000151008     c                   call      'FNLVV1R'
410100151008     c                   parm                    kpjba
410200151008     c                   parm                    parvar
410300151008     c                   parm                    partitas
410400151008     c                   parm                    parcsb
410500120312     c                   endsr
410600121108      ****************************************************
410700121108      ** Memorizzazione tracciato della bolla di sede prima
410800121108      ** della variazione
410900121108      ****************************************************
411000121205     c     memo_dstitas  Begsr
411100121116     c                   eval      dstitas=titasds
411200121108     c                   endsr
411300121109      ****************************************************
411400121109      ** Memorizzazione dati del contrassegno prima
411500121109      ** della variazione
411600121109      ****************************************************
411700121109     c     memo_csb      Begsr
411800121109     c                   clear                   dscsb
411900121109     C     KANN          CHAIN(N)  TNCSB000
412000121109     c                   if        %found(tncsb03l)
412100121115     c                   eval      dscsb=csbds
412200121109     c                   endif
412300121109     c                   endsr
412400981001     C*
412500970814**         CMD1
412600020827CHKOBJ OBJ(FIARBR3C) OBJTYPE(*FILE) MBR(M234567890)
412700920207**         CM2
412800020827ALCOBJ OBJ((FLXXXX0R *FILE *EXCL       M234567890))
412900920212**         CM3
413000020827OVRDBF FILE(FLXXXXXX)               MBR(M234567890)
413100920113**         CM5
413200020827ADDLFM FILE(FIARBR3C) MBR(M234567890)   DTAMBRS((*CURRENT/FNARBK0R
413300020827 (M234567890)) (*CURRENT/FNARBD0R (M234567890)) (*CURRENT/FIARBT0R
413400020827 (M234567890)) (*CURRENT/FNARBG0R (M234567890)) (*CURRENT/FNARBM0R
413500020827 (M234567890)))
413600920113**
4137009201301 BOLLA  INESISTENTE                                                1
413800920130TIPO BOLLA INESISTENTE                                              2
413900920130CAUSALE VARIAZIONE INESISTENTE                                      3
414000960514NON ESISTE RECORD BOLLA IN TNAC200F                                 4
414100920130NON ESISTE ANAGRAFICA CONTRASSEGNI                                  5
414200920130NUOVO CODICE BOLLA INESISTENTE                                      6
4143009201302 BOLLA  INESISTENTE                                                7
414400920220SPEDIZIONE GIA' TASSATA                                             8
414500920212SPEDIZIONE GIA' CONTABILIZZATA                                      9
414600920210C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
414700950201MANCA RECORD FILE FNARBM0R                                          11
414800950201MANCA X CREAZ. ASSEGNATO FNARBD1R                                   12
414900930513X CREAR 2BOLLA MA NO VARIAZ CD.BOLL                                 15
415000920601GIA' CONTABIL.:NO TOLTA VARIA G                                     14
415100930513
415200920601GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 16
415300930513
415400920601GIA' CONTABIL.:NO AGGIUNTA VARIA G                                  18
415500930513
415600920601GIA' CONTABIL.:NO VARIATO L'A3                                      20
415700930513
415800920601GIA' CONTABIL.:NO MODIFIC. VARIA G                                  22
415900990414GIA' TASSATA  :NO TOLTA    VARIA G                                  23
416000920228GIA' TASSATA  :NO AGGIUNTA VARIA G                                  24
416100920228GIA' TASSATA  :NO MODIFIC. VARIA G                                  25
416200950203CAP E NAZIONE MITTENTE INESISTENTE                                  26
416300950203COD.MITTENTE INESISTENTE IN ANAGRAF                                 27
416400920302SPEDIZIONE ANNULLATA                                                28
416500960919COD.MITTENTE BLOCCATO                                               29
416600970814ERRORE IN DETERMINAZIONE COD.TASSAZ                                 30
416700990414AGGIUNTO   IMPORTO DA ASSICURARE                                    31
416800990414TOLTO      IMPORTO DA ASSICURARE                                    32
416900990414MODIFICATO IMPORTO DA ASSICURARE                                    33
417000990414GIA' CONTABIL.:NO TOLTA    VARIA R                                  34
417100990414GIA' CONTABIL.:NO AGGIUNTA VARIA R                                  35
417200990414GIA' CONTABIL.:NO MODIFIC. VARIA R                                  36
417300991022
417400991022
417500991022
417600991022AGGIUNTO IM.ASS. TP.BOLLA NON ASSIC                                 40
417700991022TOLTO    IM.ASS. TP.BOLLA NON ASSIC                                 41
417800991022MODIFIC. IM.ASS. TP.BOLLA NON ASSIC                                 42
417900070119** cmdt
418000070119OVRDBF FILE(FNBLP01L) TOFILE(
