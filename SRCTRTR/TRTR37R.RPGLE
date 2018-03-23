000100910926     H DECEDIT('0,') DATEDIT(*DMY.)
000200170109     H* TRTR37R *----------------------------------------------------*
000300911114     H*  RICEZIONE IN SEDE VARIAZIONE BOLLE                          *
000400000000     H*--------------------------------------------------------------*
000500070119     Ffnblp01l  IF   E           K DISK    usropn
000600070119     FTNTAM01L  IF   E           K DISK
000700990719     FTITPT01L  IF   E           K DISK
000800990719     FTITPD01L  IF   E           K DISK
000900981001     F*  ARCHIVIO BOLLA SEDE MITTENTE DESTINATARIO
001000981001     FTNTMD01L  UF A E           K DISK
001100981001     F*
001200170109     FFiarbt3c  UF   E           K DISK    infds(infdsarbr)
001300170110     Ffnarbg2t  iF   E             disk    rename(fnarbg00:fnarbg)
001400170110     Ffnarbd2t  iF   E             disk    rename(fnarbd00:fnarbd)
001500170110     Ffnarbk2t  iF   E             disk    rename(fnarbk00:fnarbk)
001600170110     Ffiarbt2t  iF   E             disk    rename(fiarbt00:fiarbt)
001700170110     F***FNARBM1r  UF   E           K DISK
001800170110     F***                                     RENAME(FNARBM00:FNARBMrr)
001900170110     F***FNARBD1R  UF   E           K DISK
002000170110     F***                                     RENAME(FNARBD00:FNARBDRR)
002100170109     FFiARBU2l  UF   E           K DISK
002200170109     F                                     RENAME(FIARBU00:FIARBUtt)
002300991014     FFIARBE2C  IF A E           K DISK
002400950201     F                                     RENAME(FNARBD00:FNARBDEE)
002500991014     F                                     RENAME(FIARBT00:FIARBTEE)
002600950201     F                                     RENAME(FNARBK00:FNARBKEE)
002700950201     F                                     RENAME(FNARBM00:FNARBMEE)
002800950201     F                                     RENAME(FNARBG00:FNARBGEE)
002900991014     FFIARBU1E  O  A E           K DISK
003000991014     F                                     RENAME(FIARBU00:FIARBUEE)
003100950201     F*
003200990624     FTITAS30C  UF A E           K DISK
003300990629     FTITAA30C  UF A E           K DISK
003400990624     FTITA730C  UF A E           K DISK
003500010126     FTITA430C  UF A E           K DISK
003600151120     Ffiar531c  uF a E           K DISK
003700000000     FTABEL00F  IF   E           K DISK
003800900426     FAZORG01L  IF   E           K DISK
003900941121     FFNLBL01L  IF   E           K DISK
004000941121     FTNCSB03L  UF A E           K DISK
004100941121     FTNCSA01L  UF A E           K DISK
004200941121     FTNCSV00F  O  A E             DISK
004300981118     Fndppa01L  IF   E           K DISK
004400990518     FNdcpa01L  uF   E           K DISK
004500941130     FTNBLA01L  IF   E           K DISK
004600121107
004700170109     D*CMD1            S             51    DIM(1) CTDATA PERRCD(1)
004800170109     D*CM2             S              1    DIM(51) CTDATA PERRCD(51)
004900170109     D*CM3             S              1    DIM(51) CTDATA PERRCD(51)
005000170109     D*CM5             S              1    DIM(213) CTDATA PERRCD(66)
005100170109     D*C6              S              1    DIM(213)
005200920113     D*
005300991019     D ERR             S             35    DIM(42) CTDATA PERRCD(1)             COSTANTI ERRORA
005400950202     D K35             S              1    DIM(35)                              COMODO        A
005500070119     d cmdt            s             60    dim(01) ctdata perrcd(1)
005600011218
005700011218     d sksv            s              1    dim(20)                              sk sigla varie
005800011218     d skva            s             11  3 dim(20)                              sk imp.varie
005900011218
006000151008     d infdsarbr       ds
006100151008     d   recname             261    270
006200151008     D   nrri                397    400i 0
006300970916     D WLBDAT          DS                  INZ
006400970916     D  G02DAT                 1      8  0
006500970916     D  G02INV                 9     16  0
006600970916     D  G02ERR                17     17
006700970916     D  G02TGI                18     22  0
006800970916     D WGIDAT          DS                  INZ
006900970916     D  GIODAT                 1      8  0
007000970916     D  GIOINV                 9     16  0
007100970916     D  GIOTGI                17     21  0
007200941130     D CSBDS         E DS                  EXTNAME(TNCSB00F)
007300941130     D               E DS                  EXTNAME(TNCSV00F)
007400941130     D  CSVDS                 16    229
007500920113     D KPJBA         E DS
007600170109     D* LIBSYS                92    101
007700981118     D  wsoc                  98    100
007800170109     D* LEGFIL               483    492
007900170109     D* MBRFIL               493    502
008000170109     D* ALFFIL               494    496
008100151008     d fnlvv1ds      e ds
008200151008      * ATTENZIONE
008300151008     d* Se dovesse cambiare la lunghezza record di una delle seguenti DS
008400151008     d* riverificare la definizione del campo parvar
008500151008     d dsarbg          ds                  qualified
008600151008     d   fnarbg                            likerec(fnarbg) overlay(dsarbg)
008700151008     d dsarbk          ds                  qualified
008800151008     d   fnarbk                            likerec(fnarbk) overlay(dsarbk)
008900151008     d dsarbd          ds                  qualified
009000151008     d   fnarbd                            likerec(fnarbd) overlay(dsarbd)
009100151008     d dsarbt          ds                  qualified
009200151008     d   fiarbt                            likerec(fiarbt) overlay(dsarbt)
009300151008      * Definizione del seguente campo in base alla dsarbd che è la più lunga
009400151008      * di tutte.
009500151008     dparvar           s            224
009600151008     dpartitas         s                   like(titasds)
009700151008     dparcsb           s                   like(dscsb)
009800970714     D* DS PER TISI95R - CONTROLLO CAP
009900970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
010000021014     D ddatiute      e ds
010100021014     D azuteds       e ds                  extname(AZUTE00F)
010200020604     D tibs34ds      E DS
010300021112     D tibs36ds      E DS
010400070201     D tibs55ds      E DS
010500930514     D DSTB          E DS
010600930514     D DS3A          E DS
010700920113     D DS7L          E DS
010800150108     D DS7R          E DS
010900950202     D DS1A          E DS
011000010126     D og143         E DS
011100010126     D dsbl4j        E DS
011200060317     D dta4a         E DS
011300971215     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011400981001     D DSBS69        E DS                  EXTNAME(TIBS69DS)
011500981001     D DSACO         E DS                  EXTNAME(CNACO00F)
011600981001     D DSIND         E DS                  EXTNAME(CNIND00F)
011700981001     D DSCLP         E DS                  EXTNAME(CNCLP00F)
011800981001     D DSCLS         E DS                  EXTNAME(FNCLS00F)
011900121115     D dstitas       E DS                  EXTNAME(titas00f) qualified
012000121116     D titasds       E DS                  EXTNAME(titas00f)
012100121115     D dscsb         E DS                  EXTNAME(tncsb00f) qualified
012200080408     D SAVDS3a       E DS                  extname(ds3a) prefix(S_)
012300120510     d*dvpoDECO      e ds
012400121114     D Tibs02DS      E DS
012500151119     D dar5fat       E DS
012600071102     d
012700990505     DWDELARBU         s              1
012800990505     dwctb             s                   like(ppactb) inz('CG')
012900981118     dwkcc             s                   like(ppakcc) inz('000151')
013000981118     dwksc             s                   like(ppaksc) inz('00000000')
013100981118     dwdft             s                   like(ppadtpar)
013200981118     dwnft             s                   like(ppanrpar)
013300981118     dwfiv             s                   like(ppaserpar) inz
013400981118     dsavfiv           s                   like(tasfiv)
013500981118     dsavnft           s                   like(tasnft)
013600981118     dsavdft           s                   like(tasdft)
013700981118     dsavksc           s                   like(tasksc)
013800151012     dtasdsp           s                   like(tamddt)
013900990505     dWCAS             s                   like(arbcas)
014000990625     d§trc             s                   like(ta4trc)
014100991102     dwcsbvca          s                   like(csbvca)
014200151119     dkar5trd          s                   like(ar5trd)
014300011218
014400011218     d flgriorg        s              1
014500011218     d xx              s              2  0                                      indice sk varie
014600070119     d commt           s             80
014700070119     d lenght          s             15  5 inz(80)
014800120424     D k               s              5  0                                      indice
014900121205     D wanncas         s              1
015000151008     D                SDS
015100151008     D  NOMPGM                 1     10
015200941130     I*
015300950201     IFNARBD00      11
015400950201     IFNARBK00      12
015500991014     IFIARBT00      13
015600950201     IFNARBG00      15
015700950201     IFNARBM00      16
015800941121     I*
015900990624     ITITAS000      02
016000990624     ITITAS010      03
016100990624     ITITASP00      04
016200010126     I*
016300010126     ITITA4000      05
016400010126     ITITA4P00      06
016500071029     I*
016600071029     ITITAA000      07
016700071029     ITITAAP00      08
016800920113     I/SPACE 3
016900920113     C****************************************************************
017000920113     C*  RIEPILOGO INDICATORI
017100920113     C***************************************************************
017200950201     C* 01    - LETTURA SUL FILE DEGLI ERRORI DI FNARBE
017300010126     C* 02/06 - INDICATORI DI TIPO RECORD
017400941130     C* 17    - STAMPO ERRORE VARIAZIONE CONS PARTICOLARE
017500920228     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
017600930514     C* 23    - OFF SPEDIZIONE STORNATA
017700930513     C* 24    - NON AGGIORNO TUTTI I CAMPI SE 2BOLL GIA' CONTABILIZZATA
017800930513     C* 25    - NON ESISTE ANAGRAFICA C/A
017900981118     C* 26    - NON ESISTE RECORD IN partita
018000930513     C* 27    - NON AGGIORNO TUTTI I CAMPI SE 1BOLL GIA' CONTABILIZZATA
018100930512     C* 29    - ON  33 ON  - NON ESISTE NEMMENO        SU BLTAS10
018200930702     C*       - ON  33 OFF - SPEDIZIONE CONTABILIZZATA SU BLTAS
018300950201     C* 30    - LETTURA FNARBR3C
018400920212     C* 31    - FILE CONTRASSEGNI NON TROVATO O GIA' INCASATO
018500920212     C* 32    - COMODO
018600920228     C* 33    - ON SPEDIZIONE ESISTENTE SU BLTAS10
018700920212     C* 34    - COMODO
018800170110     C* 35    -
018900170110     C* 36    - Libero
019000920213     C* 37    - RECORD DA NON CANCELLARE DOPO WRITE ERRORE
019100920226     C* 38    - SPEDIZIONE GIA' TASSATA
019200920227     C* 39    - DI COMODO
019300920303     C* 40    - SPEDIZIONE ANNULLATA
019400941130     C* 41    - ESISTENTE TNCSB000
019500930303     C* 42    - ESISTENTE LEGAME BOLLA (C/ASSEGNO SULLA MAMMA)
019600981001     C* 48    - NON ESISTE RECORD SU TNTMD
019700920113     C*****************************************************************
019800920113     C*
019900920113     C* ALLOCO MEMBRI
020000170109     C***                EXSR      CAEX
020100170109     C***20              GOTO      FINE
020200170110
020300170110     C     *ENTRY        PLIST
020400170110     C                   PARM                    KPJBA
020500170110     C                   PARM                    chiudi            1
020600170110     c
020700170110     c                   exsr      sr_date
020800920113     C*
020900170110     c     *loval        setll     fiarbt3c
021000170110     C                   READ      FIARBT3C                               30
021100920113     C*
021200930512    1C     *IN30         DOWEQ     '0'
021300170110
021400170110     c                   if        %shtdn
021500170110     c                   eval      chiudi='S'
021600170110     c                   goto      fine
021700170110     c                   endif
021800170110
021900170110     C*****              SETOFF                                       242736
022000170110     C                   SETOFF                                       2427
022100930622     C*
022200981001     C                   MOVEL     ' '           DMDD              1
022300930514     C                   MOVEL     ' '           ST1
022400981001     C* SOLO NEL CASO IN CUI SI TRATTI DI LETTURA DI VARIAZIONI MITTENTE E
022500981001     C* DESTINATARIO ( *IN11 *IN16 ON ) CONTROLLO SE SI TRATTA DI VARIAZIONI
022600981001     C* DM / DD PERCHE' IN QUESTO CASO DEVO SCRIVERE IL FILE DATI ANAGRAFICI
022700981001     C* SPEDIZIONI CHE VIENE UTILIZZATO PER STAMPE RELATIVE AI DANNI
022800981001     C* INOLTRE SEMPRE PER 11 E 16 ACCESO PER LE VARIAZIONI I0 / MI (VARIAZIONI
022900981001     C* DESTINATARIO E MITTENTE) DEVO AGGIORNARE QUESTO FILE INDIPENDENTEMENTE
023000981001     C* DAL FATTO CHE SIA RELATIVO AD UNA BOLLA GIA' CONTABILIZZATA
023100981001     C     *IN11         IFEQ      '1'
023200981001     C     *IN16         OREQ      '1'
023300981001     C                   EXSR      AGTMD
023400981001     C* SE HO LETTO UNA VARIAZIONE DM O DD LEGGO IL RECORD SUCCESSIVO
023500981001     C* ALTRIMENTI PROSEGUO
023600981001     C     DMDD          IFNE      ' '
023700981001     C                   GOTO      NEXT
023800981001     C                   ENDIF
023900981001     C*
024000981001     C                   ENDIF
024100930513     C*
024200930513     C* PRENDO  C O D I C E   B O L L A
024300920113     C                   MOVEL     '3A'          COD
024400920113     C                   MOVEL     *BLANKS       KEY
024500991014     C  N12              MOVEL     ARBCBO        KEY
024600950203     C   12              MOVEL     ARBCBP        KEY
024700920113     C     KTAB          CHAIN     TABEL                              33
024800920114     C*
024900920212     C* SE NON ESISTE TIPO BOLLA: ERRORE E NON CANCELLO
025000930513    2C     *IN33         IFEQ      '1'
025100991014     C                   MOVEL     ERR(2)        ARBDER
025200930513     C*
025300000703     C                   Z-ADD     102           COMFEV
025400930513     C                   SETON                                        37
025500920212     C*
025600930513     C                   EXSR      WTRERR
025700930513     C*
025800930513   X2C                   ELSE
025900930513     C*
026000930513     C                   MOVEL     TBLUNI        DS3A
026100950203     C* IMPOSTO IL TIPO BOLLA PER EVENTUALE ERRORE
026200950203     C                   MOVEL     §3ATB1        TBLERR            1
026300990519     C                   MOVEL     §3ARBL        TBLRBL            1
026400930513     C*
026500930513     C* PRENDO  C A U S A L E   V A R I A Z I O N E
026600991014     C                   MOVEL     ARBCVB        COMCVB            2
026700930513     C*
026800930513     C                   MOVEL     '7L'          COD
026900930513     C                   MOVEL     *BLANKS       KEY
027000930513     C                   MOVEL     COMCVB        KEY
027100930513     C     KTAB          CHAIN     TABEL                              33
027200930513     C*
027300930513     C* 33 ON - NON ESISTE CAUSALE VARIAZIONE: STAMPA E NON ANNULLO
027400930513    3C     *IN33         IFEQ      '1'
027500991014     C                   MOVEL     ERR(3)        ARBDER
027600930513     C*
027700930513     C                   Z-ADD     102           COMFEV
027800930513     C                   SETON                                        37
027900930513     C*
028000930513     C                   EXSR      WTRERR
028100930513    3C                   END
028200930513     C*
028300930513     C  N33              MOVEL     TBLUNI        DS7L
028400930513    2C                   END
028500920130     C*
028600930513    2C     *IN33         IFEQ      '0'
028700920226     C                   MOVEL     COMCVB        FLGCVB            1
028800920226     C*
028900990701     C                   CLEAR                   KSC42BO
029000990701     c*
029100920226     C* CONTROLLO ESISTENZA 2   BOLLA
029200950202    3C     FLGCVB        IFNE      'C'
029300950202     C     §3ATB2        ANDNE     '  '
029400970514     C* NON CONTROLLO LA 2 BOLLA SE TASSAZIONE IN PARTENZA PERCHE' NON
029500970514     C*  LA VA MAI AD AGGIORNARE
029600970514     C     COMCVB        ANDNE     'TP'
029700920226     C                   MOVEL     §3ATB2        COMTBL
029800920226     C                   EXSR      CTRSPE
029900941130     C*
030000941130     C* SE NON TROVATI ERRORI --> SALVO CAMPI CHE MI SERVONO
030100941130    4C     *IN29         IFEQ      *OFF
030200941130     C                   MOVE      TASKSC        SAVKS4            4 0
030300981118     C                   MOVE      TASKSC        SAVKsc
030400941130     C                   MOVEL     TASFIV        SAVFIV
030500941130     C                   MOVE      TASDFT        SAVDFT
030600941130     C                   MOVEL     TASNFT        SAVNFT
030700960625     C                   MOVEL     TASTBL        SAVTBL
030800941130     C*
030900941130     C* 24 ON - 2 BOLLA GIA' CONTABILIZZATA
031000941130    5C     *IN02         IFEQ      *ON
031100941130     C     TASFIC        ANDNE     ' '
031200941130     C     *IN02         OREQ      *OFF
031300941130     C                   SETON                                        24
031400941130    5C                   ENDIF
031500941130    4C                   ENDIF
031600930513    3C                   END
031700920226     C*
031800920226     C* SE TROVATA 2 BOLLA CONTROLLO LA 1
031900930513    3C  N29FLGCVB        IFNE      '2'
032000920226     C                   MOVEL     §3ATB1        COMTBL
032100920226     C                   EXSR      CTRSPE
032200941130     C*
032300941130    4C     *IN29         IFEQ      *OFF
032400981118     C* 27 ON - 1 BOLLA GIA' CONTABILIZZATA --> AGGIORNO ANCHE yncpa
032500941130    5C     *IN02         IFEQ      *ON
032600941130     C     TASFIC        ANDNE     ' '
032700941130     C     *IN02         OREQ      *OFF
032800941130     C                   SETON                                        27
032900941130    5C                   END
033000941130    4C                   END
033100930513    3C                   END
033200990414     C*
033300941130     C*
033400941130     C* 29 OFF - AGGIORNO BOLLA SE NON SONO STORNATE ENTRAMBI LE BOLLE
033500930514     C  N29ST1           IFEQ      ' '
033600991021     c     flgcvb        oreq      'C'
033700930514     C                   EXSR      AGGBOL
033800930514     C                   END
033900941130     C*
034000930514    2C                   END
034100920113     C*
034200950203    2C  N37              DO
034300920212     C   11              ADD       1             NUMRBD            7 0
034400920113     C   12              ADD       1             NUMRBK            7 0
034500920113     C   13              ADD       1             NUMRBT            7 0
034600950201     C   15              ADD       1             NUMRBG            7 0
034700950201     C   16              ADD       1             NUMRBM            7 0
034800920219     C* CANCELLO RECORD
034900950201     C   11              DELETE    FNARBD00
035000950201     C   12              DELETE    FNARBK00
035100170109     C***12KARBM         DELETE    FNARBMRR                           32
035200991014     C   13              DELETE    FIARBT00
035300100303     c   13              if        st1='1'
035400170109     C     KARBU         SETLL     FIARBU2L
035500170109     C     KARBU         READE     FIARBU2L                               30
035600100303     C*
035700100303     C     *IN30         DOWEQ     *OFF
035800170109     C                   DELETE    FIARBUtt
035900100303     C*
036000170109     C     KARBU         READE     FIARBU2L                               30
036100100303     C                   ENDDO
036200100303     c
036300100303     c                   endif
036400100303     c
036500950201     C   15              DELETE    FNARBG00
036600950201     C   16              DELETE    FNARBM00
036700930513    2C                   END
036800920113     C*
036900981001     C     NEXT          TAG
037000981001     C*
037100920113     C                   SETOFF                                       111213
037200991014     C                   SETOFF                                         1516
037300950201     C                   SETOFF                                       353738
037400941130     C                   SETOFF                                       2933
037500170109     C                   READ      FIARBT3C                               30
037600930512    1C                   END
037700970714     C*
037800170110     c     fine          tag
037900170110     c                   if        chiudi='S'
038000970714     C                   CLEAR                   DSSI95
038100970714     C                   MOVEL     'C'           I95TLA
038200970714     C                   CALL      'TISI95R'
038300970714     C                   PARM                    DSSI95
038400971215     C*
038500971215     C                   MOVEL     'C'           D55TLA
038600971215     C                   CALL      'FNLV55R'
038700971215     C                   PARM                    DSLV55
038800981001     C*
038900981001     C                   CLEAR                   DSBS69
039000981001     C                   MOVEL     'C'           I69TLA
039100981001     C                   CALL      'TIBS69R'
039200981001     C                   PARM                    DSBS69
039300981001     C                   PARM                    DSACO
039400981001     C                   PARM                    DSIND
039500981001     C                   PARM                    DSCLP
039600981001     C                   PARM                    DSCLS
039700020604     c*
039800021112     C                   CLEAR                   tibs36ds
039900021112     C                   MOVEL     'C'           I36TLA
040000021112     C                   CALL      'TIBS36R'
040100021112     C                   PARM                    TIBS36DS
040200120424
040300121114
040400121114     C                   CLEAR                   tibs02ds
040500121114     C                   MOVEL     'C'           t02tla
040600121114     C                   CALL      'TIBS02R'
040700121114     C                   PARM                    kpjba
040800121114     C                   PARM                    tibs02ds
040900151009
041000151009     C                   MOVEL     'C'           iv1tla
041100151009     c                   movel     fnlvv1ds      kpjbu
041200151009     C                   CALL      'FNLVV1R'
041300151009     C                   PARM                    kpjba
041400920113     C*
041500920113     C*
041600920113     C                   SETON                                        LR
041700170110     c                   else
041800170110     c                   seton                                        rt
041900170110     c                   endif
042000920113     C**---------------------------------------------------------
042100920113     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
042200920113     C**---------------------------------------------------------
042300170109     C*    CAEX          BEGSR
042400170109     C*                  MOVE      ')'           VAR11            11
042500170109     C*                  MOVEL     MBRfil        VAR11
042600170109     C*                  Z-ADD     51            LUNG             15 5
042700170109     C*                  Z-ADD     213           LUNG2            15 5
042800920113     C* CHK E ADD SU TUTTI I FILE FISICI
042900170109     C*                  MOVEL     'FNARBD0R'    FISICO           10
043000170109     C*                  CALL      'TRUL50C'
043100170109     C*                  PARM                    FISICO
043200170109     C*                  PARM                    MBRFIL
043300170109     C*                  PARM                    LIBSYS
043400170109     C*                  PARM                    LOGICO           10
043500170109     C*                  PARM                    MBRLOG           10
043600170109     C*                  PARM                    ULFLG             1
043700170109     C*                  MOVEL     'FNARBD1R'    LOGICO
043800170109     C*                  MOVEL     MBRFIL        MBRLOG
043900170109     C*                  CALL      'TRUL50C'
044000170109     C*                  PARM                    FISICO
044100170109     C*                  PARM                    MBRFIL
044200170109     C*                  PARM                    LIBSYS
044300170109     C*                  PARM                    LOGICO           10
044400170109     C*                  PARM                    MBRLOG           10
044500170109     C*                  PARM                    ULFLG             1
044600950202     C*
044700170109     C*                  CLEAR                   LOGICO
044800170109     C*                  CLEAR                   MBRLOG
044900170109     C*                  MOVEL     'FIARBT0R'    FISICO
045000170109     C*                  CALL      'TRUL50C'
045100170109     C*                  PARM                    FISICO
045200170109     C*                  PARM                    MBRFIL
045300170109     C*                  PARM                    LIBSYS
045400170109     C*                  PARM                    LOGICO           10
045500170109     C*                  PARM                    MBRLOG           10
045600170109     C*                  PARM                    ULFLG             1
045700170109     C*                  MOVEL     'FNARBK0R'    FISICO
045800170109     C*                  CALL      'TRUL50C'
045900170109     C*                  PARM                    FISICO
046000170109     C*                  PARM                    MBRFIL
046100170109     C*                  PARM                    LIBSYS
046200170109     C*                  PARM                    LOGICO           10
046300170109     C*                  PARM                    MBRLOG           10
046400170109     C*                  PARM                    ULFLG             1
046500950202     C*
046600170109     C*                  MOVEL     'FNARBM0R'    FISICO
046700170109     C*                  CALL      'TRUL50C'
046800170109     C*                  PARM                    FISICO
046900170109     C*                  PARM                    MBRFIL
047000170109     C*                  PARM                    LIBSYS
047100170109     C*                  PARM                    LOGICO           10
047200170109     C*                  PARM                    MBRLOG           10
047300170109     C*                  PARM                    ULFLG             1
047400170109     C*                  MOVEL     'FNARBM1R'    LOGICO
047500170109     C*                  MOVEL     MBRFIL        MBRLOG
047600170109     C*                  CALL      'TRUL50C'
047700170109     C*                  PARM                    FISICO
047800170109     C*                  PARM                    MBRFIL
047900170109     C*                  PARM                    LIBSYS
048000170109     C*                  PARM                    LOGICO           10
048100170109     C*                  PARM                    MBRLOG           10
048200170109     C*                  PARM                    ULFLG             1
048300950202     C*
048400170109     C*                  CLEAR                   LOGICO
048500170109     C*                  CLEAR                   MBRLOG
048600170109     C*                  MOVEL     'FIARBU0R'    FISICO
048700170109     C*                  CALL      'TRUL50C'
048800170109     C*                  PARM                    FISICO
048900170109     C*                  PARM                    MBRFIL
049000170109     C*                  PARM                    LIBSYS
049100170109     C*                  PARM                    LOGICO           10
049200170109     C*                  PARM                    MBRLOG           10
049300170109     C*                  PARM                    ULFLG             1
049400170109     C*                  MOVEL     'FIARBU1R'    LOGICO
049500170109     C*                  MOVEL     MBRFIL        MBRLOG
049600170109     C*                  CALL      'TRUL50C'
049700170109     C*                  PARM                    FISICO
049800170109     C*                  PARM                    MBRFIL
049900170109     C*                  PARM                    LIBSYS
050000170109     C*                  PARM                    LOGICO           10
050100170109     C*                  PARM                    MBRLOG           10
050200170109     C*                  PARM                    ULFLG             1
050300950202     C*
050400170109     C*                  CLEAR                   LOGICO
050500170109     C*                  CLEAR                   MBRLOG
050600170109     C*                  MOVEL     'FNARBG0R'    FISICO
050700170109     C*                  CALL      'TRUL50C'
050800170109     C*                  PARM                    FISICO
050900170109     C*                  PARM                    MBRFIL
051000170109     C*                  PARM                    LIBSYS
051100170109     C*                  PARM                    LOGICO           10
051200170109     C*                  PARM                    MBRLOG           10
051300170110     C*                  PARM                    ULFLG             1
051400920113     C*
051500920113     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE FACCIO ADDLFM
051600170109     C*                  SETOFF                                       20
051700170109     C*                  MOVE      VAR11         CMD1
051800170109     C*                  MOVEL     *BLANKS       COMMAN
051900170109     C*                  MOVEA     CMD1(1)       COMMAN           80
052000170109     C*                  CALL      'QCMDEXC'                            20
052100170109     C*                  PARM                    COMMAN
052200170109     C*                  PARM                    LUNG
052300920113     C*
052400170109     C*  20              DO
052500170109     C*                  MOVEA     CM5           C6
052600170109     C*
052700170109     C*                  MOVEA     MBRFIL        C6(27)
052800170109     C*                  MOVEA     MBRFIL        C6(69)
052900170109     C*                  MOVEA     MBRFIL        C6(102)
053000170109     C*                  MOVEA     MBRFIL        C6(135)
053100170109     C*                  MOVEA     MBRFIL        C6(168)
053200170109     C*                  MOVEA     MBRFIL        C6(201)
053300170109     C*                  SETOFF                                       20
053400170109     C*                  MOVEL     *BLANKS       COMMA2
053500170109     C*                  MOVEA     C6            COMMA2          213
053600170109     C*                  CALL      'QCMDEXC'                            H1
053700170109     C*                  PARM                    COMMA2
053800170109     C*                  PARM                    LUNG2
053900170109     C*  20              GOTO      ENDCAE
054000170109     C*                  END
054100020827
054200170109     C*                  MOVE      '))'          VAR12            12
054300170109     C*                  MOVEL     MBRfil        VAR12
054400920113     C*
054500920114     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
054600041006     C**                 SETOFF                                       20
054700041006     C**                 MOVEa     VAR12         CM2(40)
054800041006     C**                 MOVEA     'FIARBR3C'    CM2(13)
054900041006     C**                 MOVEL     *BLANKS       COMMAN
055000041006     C**                 MOVEA     CM2           COMMAN
055100041006     C**                 CALL      'QCMDEXC'                            20
055200041006     C**                 PARM                    COMMAN
055300041006     C**                 PARM                    LUNG
055400920113     C*
055500041006     C** 20              GOTO      ENDCAE
055600041006     C**
055700930212     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
055800041006     C**                 SETOFF                                       20
055900041006     C**                 MOVEa     VAR12         CM2(40)
056000041006     C**                 MOVEA     'FNARBM1R'    CM2(13)
056100041006     C**                 MOVEL     *BLANKS       COMMAN
056200041006     C**                 MOVEA     CM2           COMMAN
056300041006     C**                 CALL      'QCMDEXC'                            20
056400041006     C**                 PARM                    COMMAN
056500041006     C**                 PARM                    LUNG
056600041006     C**
056700041006     C** 20              GOTO      ENDCAE
056800041006     C**
056900041006     C**                 SETOFF                                       20
057000041006     C**                 MOVEa     VAR12         CM2(40)
057100041006     C**                 MOVEA     'FNARBD1R'    CM2(13)
057200041006     C**                 MOVEL     *BLANKS       COMMAN
057300041006     C**                 MOVEA     CM2           COMMAN
057400041006     C**                 CALL      'QCMDEXC'                            20
057500041006     C**                 PARM                    COMMAN
057600041006     C**                 PARM                    LUNG
057700041006     C**
057800041006     C** 20              GOTO      ENDCAE
057900041006     C**
058000041006     C**ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
058100041006     C**                 SETOFF                                       20
058200041006     C**                 MOVEa     VAR12         CM2(40)
058300041006     C**                 MOVEA     'FIARBU1R'    CM2(13)
058400041006     C**                 MOVEL     *BLANKS       COMMAN
058500041006     C**                 MOVEA     CM2           COMMAN
058600041006     C**                 CALL      'QCMDEXC'                            20
058700041006     C**                 PARM                    COMMAN
058800041006     C**                 PARM                    LUNG
058900041006     C**
059000041006     C** 20              GOTO      ENDCAE
059100041006     C**OVRDBF
059200170109     C*                  SETOFF                                       20
059300170109     C*                  MOVEa     VAR11         CM3(41)
059400170109     C*                  MOVEA     'FNARBM1R'    CM3(13)
059500170109     C*                  MOVEL     *BLANKS       COMMAN
059600170109     C*                  MOVEA     CM3(1)        COMMAN
059700170109     C*                  CALL      'QCMDEXC'                            20
059800170109     C*                  PARM                    COMMAN
059900170109     C*                  PARM                    LUNG
060000920212     C*
060100170109     C*  20              GOTO      ENDCAE
060200170109     C*                  SETOFF                                       20
060300170109     C*                  MOVEa     VAR11         CM3(41)
060400170109     C*                  MOVEA     'FIARBU1R'    CM3(13)
060500170109     C*                  MOVEL     *BLANKS       COMMAN
060600170109     C*                  MOVEA     CM3(1)        COMMAN
060700170109     C*                  CALL      'QCMDEXC'                            20
060800170109     C*                  PARM                    COMMAN
060900170109     C*                  PARM                    LUNG
061000950202     C*
061100170109     C*  20              GOTO      ENDCAE
061200920214     C* OVRDBF
061300170109     C*                  SETOFF                                       20
061400170109     C*                  MOVEa     VAR11         CM3(41)
061500170109     C*                  MOVEA     'FNARBD1R'    CM3(13)
061600170109     C*                  MOVEL     *BLANKS       COMMAN
061700170109     C*                  MOVEA     CM3(1)        COMMAN
061800170109     C*                  CALL      'QCMDEXC'                            20
061900170109     C*                  PARM                    COMMAN
062000170109     C*                  PARM                    LUNG
062100920214     C*
062200170109     C*  20              GOTO      ENDCAE
062300920212     C*
062400170109     C*                  MOVEA     'FIARBR3C'    CM3(13)
062500170109     C*                  MOVEL     *BLANKS       COMMAN
062600170109     C*                  MOVEA     CM3(1)        COMMAN
062700170109     C*                  CALL      'QCMDEXC'                            20
062800170109     C*                  PARM                    COMMAN
062900170109     C*                  PARM                    LUNG
063000170109     C*  20              GOTO      ENDCAE
063100151008      *
063200170109     C*                  MOVEA     'FNARBD0R'    CM3(13)
063300170109     C*                  MOVEL     *BLANKS       COMMAN
063400170109     C*                  MOVEA     CM3(1)        COMMAN
063500170109     C*                  CALL      'QCMDEXC'                            20
063600170109     C*                  PARM                    COMMAN
063700170109     C*                  PARM                    LUNG
063800170109     C*  20              GOTO      ENDCAE
063900170109     C*                  MOVEA     'FNARBK0R'    CM3(13)
064000170109     C*                  MOVEL     *BLANKS       COMMAN
064100170109     C*                  MOVEA     CM3(1)        COMMAN
064200170109     C*                  CALL      'QCMDEXC'                            20
064300170109     C*                  PARM                    COMMAN
064400170109     C*                  PARM                    LUNG
064500170109     C*  20              GOTO      ENDCAE
064600170109     C*                  MOVEA     'FNARBG0R'    CM3(13)
064700170109     C*                  MOVEL     *BLANKS       COMMAN
064800170109     C*                  MOVEA     CM3(1)        COMMAN
064900170109     C*                  CALL      'QCMDEXC'                            20
065000170109     C*                  PARM                    COMMAN
065100170109     C*                  PARM                    LUNG
065200170109     C*  20              GOTO      ENDCAE
065300170109     C*                  MOVEA     'FIARBT0R'    CM3(13)
065400170109     C*                  MOVEL     *BLANKS       COMMAN
065500170109     C*                  MOVEA     CM3(1)        COMMAN
065600170109     C*                  CALL      'QCMDEXC'                            20
065700170109     C*                  PARM                    COMMAN
065800170109     C*                  PARM                    LUNG
065900170109     C*  20              GOTO      ENDCAE
066000920113     C*
066100170109     C* N20              OPEN      FIARBR3C
066200170109     C* N20              OPEN      FNARBM1R
066300170109     C* N20              OPEN      FNARBD1R
066400170109     C* N20              OPEN      FIARBU1R
066500170109     C* N20              OPEN      FNARBD0R
066600170109     C* N20              OPEN      FNARBK0R
066700170109     C* N20              OPEN      FNARBG0R
066800170109     C* N20              OPEN      FIARBT0R
066900920113     C*
067000170109     C*    ENDCAE        ENDSR
067100920113     C**---------------------------------------------------------
067200920113     C** AGGIORNO BOLLA PARTENZE
067300920113     C**---------------------------------------------------------
067400920113     C     AGGBOL        BEGSR
067500920130     C*
067600941130     C                   SELECT
067700061110     C     FLGCVB        WHENEQ    'F'
067800061110     C                   EXSR      AGARBF
067900061110     C*
068000941130     C     FLGCVB        WHENEQ    'I'
068100920113     C                   EXSR      AGARBI
068200941130     C*
068300941130     C     FLGCVB        WHENEQ    'K'
068400920127     C                   EXSR      AGARBK
068500920228     C*
068600941130     C     FLGCVB        WHENEQ    'T'
068700920113     C                   EXSR      AGARBT
068800920113     C*
068900941130     C     FLGCVB        WHENEQ    '2'
069000920113     C                   EXSR      AGAR2T
069100950202     C*
069200950202     C     FLGCVB        WHENEQ    'M'
069300950202     C                   EXSR      AGARBM
069400950202     C*
069500950202     C     FLGCVB        WHENEQ    'C'
069600950202     C                   EXSR      AGARBC
069700060317     C*
069800060317     C     FLGCVB        WHENEQ    'R'
069900060317     C                   EXSR      AGARBR
070000941130     C                   ENDSL
070100920113     C*
070200930513     C                   ENDSR
070300061110     C**---------------------------------------------------------
070400061110     C** AGGIORNO CODICE VARIAZIONI FX - codice fiscale
070500061110     C**---------------------------------------------------------
070600061110     C     AGARBF        BEGSR
070700061110     c
070800061110     C                   SETOFF                                       3126
070900061110     C                   SETOFF                                       2225
071000061110     C                   MOVE      TASKSC        KSC4              4 0
071100071029     c* Vedo quale bolla aggiornare
071200071029     c                   clear                   bolla             1
071300071029     c* Destinatario
071400071029    1c                   if        arbrd2='D'
071500071029     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
071600071029    2C     *IN27         IFEQ      '1'
071700071029     C     KSC4          ANDEQ     9999
071800071029     C     TASNFT        ANDGT     0
071900071029     c                   eval      bolla ='1'
072000071029    2c                   endif
072100071029     c
072200071029    2C     *IN24         ifeq      '1'
072300071029     C     SAVKS4        ANDEQ     9999
072400071029     C     SAVNFT        ANDGT     0
072500071029     c                   eval      bolla ='2'
072600071029    2c                   endif
072700071029     c
072800071029   x1c                   else
072900071029     c* Mittente
073000071029    2C     *IN27         ifeq      '1'
073100071029     C     KSC4          ANDEQ     8888
073200071029     C     TASNFT        ANDGT     0
073300071029     c                   eval      bolla ='1'
073400071029    2c                   endif
073500071029    1c                   endif
073600071029     c*
073700061110     C*
073800061110     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
073900071029    1c                   if        bolla<>' '
074000061110     C*
074100071029     C* SE SI TRATTA DI storno ignoro
074200071029    2c                   if        bolla='2'
074300061110     C                   MOVEL(P)  SAVTBL        KEY
074400061110     C                   ELSE
074500061110     C                   MOVEL(P)  TASTBL        KEY
074600061113    2C                   ENDIF
074700061110     C*
074800061110     C                   MOVEL     'TB'          COD
074900061110     C     KTAB          CHAIN     TABEL                              32
075000061110     C  N32              MOVEL     TBLUNI        DSTB
075100061110     C   32              MOVEL     *ALL'1'       DSTB
075200071029     c
075300061110     C*  FCB="1" ASSEGNATO BUONO
075400061113    2C     §TBFCB        IFEQ      '1'
075500061110     C*
075600061110     c                   clear                   wdft
075700061113    3c                   select
075800071029     c***                when      *in27
075900071029     c                   when      bolla='1'
076000061110     c                   move      tasnft        wnft
076100061110     c                   if        tasdft > 0
076200061110     c                   move      tasdft        wdft
076300061110     c                   end
076400061110     c                   move      tasksc        wksc
076500071029     c***                when      *in24
076600071029     c                   when      bolla='2'
076700061110     c                   move      savnft        wnft
076800061110     c                   if        savdft > 0
076900061110     c                   move      savdft        wdft
077000061110     c                   end
077100061110     c                   move      savksc        wksc
077200061113    3c                   endsl
077300061110     c
077400061110     C     Kppa          CHAIN     ndppa01l                           26
077500061113    3C     *IN26         IFEQ      *OFF
077600061110     c     kycpa         chain     ndcpa01l                           26
077700061113    4C     *IN26         IFEQ      *OFF
077800071102     c
077900061110     C                   MOVEL     ARBCPI        cpacdfisc
078000071102     c
078100071102     c* Solo se reperita o partita la procedura aggiorno
078200120510     c*****              if        arbrsd<>*blanks or arbdtv>=§vpopiv
078300071029     C                   MOVEL     ARBRSD        cpapartiva
078400120510     c*****              endif
078500071102     c
078600061110     C                   UPDATE    ndcpa000
078700061113    4C                   ENDIF
078800061113    3C                   ENDIF
078900061110     C*
079000061113    2C                   END
079100061113    1C                   END
079200061110     c
079300061110     C* Aggiorna record Q di TITA4
079400061110     C                   movel     'Q'           §trc
079500061110     c                   setoff                                       0506
079600061110     C     ktita         chain     tita430c                           32
079700061113     c* se non c'e' lo scrivo
079800061113    1c                   if        *in32
079900061113     c                   movel     arbaas        ta4aas
080000061113     c                   movel     arblnp        ta4lnp
080100061113     c                   movel     arbnrs        ta4nrs
080200061113     c                   movel     arbnsp        ta4nsp
080300061113     c                   movel     'Q'           ta4trc
080400061113     c                   clear                   ta4not
080500061113    1c                   endif
080600061113     c
080700061110     c* SE mittente a SX , destinatario a DX
080800071029    1c                   if        arbrd2='M'
080900061110     c                   movel     arbcpi        ta4not
081000061110     c                   else
081100061110     c                   move      arbcpi        ta4not
081200061113    1c                   endif
081300061110     c*
081400061113    1c                   if        not *in32
081500061113    2C                   IF        TA4NOT<>*BLANKS
081600061110     C   05              update    tita4000
081700061110     C   06              update    tita4p00
081800061113     C                   ELSE
081900061113     C*SE eliminata e non c'e' più nulla, cancello il record
082000061113     c   05              delete    tita4000
082100061113     c   06              delete    tita4p00
082200061113    2c                   endif
082300061113     c
082400061113   x1c                   else
082500061113    2c                   if        ta4not<>*blanks
082600061113     C  N04              write     tita4000
082700061113     C   04              write     tita4p00
082800061113    2c                   endif
082900061113     c*
083000061113    1c                   endif
083100071029     c
083200071029     c* Aggiorno partita iva
083300071102     c*                      solo se presente o partita la procedura
083400120510    0c******             if        arbrsd<>*blanks or arbdtv>=§vpopiv
083500071029     c* Destinatario --> in titas
083600071029    1c                   if        arbrd2='D'
083700071029     c                   movel     arbrsd        tascpd
083800071029     C* AGGIORNO TITAS
083900071029     C   02              UPDATE    TITAS000
084000071029     C   03              UPDATE    TITAS010
084100071029     C   04              UPDATE    TITASP00
084200071102   x1c                   else
084300071029     c* Mittente --> in titaa
084400071029     c* Aggancio TITAA e aggiorno la partita del mittente
084500071029     c                   movel     'M'           §trc
084600071029     c                   setoff                                       0708
084700071029     c     ktita         chain     titaa30c                           30
084800071102IF  2C                   if        %found(titaa30c)
084900071029     C                   MOVEL     ARBrsd        TAacpi
085000071029     c   07              update    titaa000
085100071029     c   08              update    titaap00
085200071102    2c                   endif
085300071102    1c                   endif
085400071029     C*
085500071029     C* S E C O N D A   B O L L A
085600071029     C*
085700071029     c* solo per destinatario
085800071029    1c                   if        arbrd2='D'  and §3atb2<>'  '
085900071029     C                   MOVEL     §3ATB2        COMTBL
086000071029     C                   SETOFF                                       020304
086100071029     C     KBLP          CHAIN     TiTAS30C                           39
086200071029     c* Aggiorno partita iva
086300071029     c                   movel     arbrsd        tascpd
086400071029     C* AGGIORNO TITAS
086500071029     C   02              UPDATE    TITAS000
086600071029     C   03              UPDATE    TITAS010
086700071029     C   04              UPDATE    TITASP00
086800071102    1C                   END
086900120510    0C*****              END
087000061110     C*
087100061110     C                   ENDSR
087200920113     C**---------------------------------------------------------
087300920113     C** AGGIORNO CODICE VARIAZIONI IX
087400920113     C**---------------------------------------------------------
087500920211     C     AGARBI        BEGSR
087600941130     C                   SETOFF                                       3126
087700941130     C                   SETOFF                                       2225
087800930513     C                   MOVE      TASKSC        KSC4              4 0
087900930513     C*
088000981118     C* chain su YNCPA01L per aggiornare la PIVA e dati destinatario
088100930513    1C     *IN27         IFEQ      '1'
088200930513     C     KSC4          ANDEQ     9999
088300960625     C     TASNFT        ANDGT     0
088400930513     C     *IN24         OREQ      '1'
088500930513     C     SAVKS4        ANDEQ     9999
088600960625     C     SAVNFT        ANDGT     0
088700960625     C*
088800960625     C* SE SI TRATTA DI ASSEGNATO STORNO IGNORO
088900960625     C     *IN24         IFEQ      '1'
089000960625     C     SAVKS4        ANDEQ     9999
089100960625     C                   MOVEL(P)  SAVTBL        KEY
089200960625     C                   ELSE
089300960625     C                   MOVEL(P)  TASTBL        KEY
089400960625     C                   ENDIF
089500960625     C*
089600960625     C                   MOVEL     'TB'          COD
089700960625     C     KTAB          CHAIN     TABEL                              32
089800960625     C  N32              MOVEL     TBLUNI        DSTB
089900960625     C   32              MOVEL     *ALL'1'       DSTB
090000960625     C*  FCB="1" ASSEGNATO BUONO
090100960625   1AC     §TBFCB        IFEQ      '1'
090200930513     C*
090300981118     c                   clear                   wdft
090400981118     c                   select
090500981118     c                   when      *in27
090600981118     c                   move      tasnft        wnft
090700981118     c                   if        tasdft > 0
090800981118     c                   move      tasdft        wdft
090900981118     c                   end
091000981118     c***                move      tasfiv        wfiv
091100981118     c                   move      tasksc        wksc
091200981118     c                   when      *in24
091300981118     c                   move      savnft        wnft
091400981118     c                   if        savdft > 0
091500981118     c                   move      savdft        wdft
091600981118     c                   end
091700981118     c***                move      savfiv        wfiv
091800981118     c                   move      savksc        wksc
091900981118     c                   endsl
092000981118     C     Kppa          CHAIN     ndppa01l                           26
092100960625    2C     *IN26         IFEQ      *OFF
092200990518     c     kycpa         chain     ndcpa01l                           26
092300981118    2C     *IN26         IFEQ      *OFF
092400990518     C                   MOVEL     ARBRSD        cpadescr
092500990518     C                   MOVEL     ARBIND        cpaindiriz
092600981118     C                   MOVEL     ARBCAD        cpacap
092700990518     C                   MOVEL     ARBLOD        cpalocalit
092800981118     C                   MOVEL     ARBPRD        cpaprov
092900981118     c                   movel     'IT  '        cpastato
093000981118     c     arbnzd        ifne      *blank
093100981118     c     arbnzd        andne     'I  '
093200981118     c     arbnzd        andne     'IT '
093300981118     c                   movel     arbnzd        cpastato
093400981118     c                   endif
093500981118     C                   MOVEL     ARBCPI        cpapartiva
093600930513     C*
093700990518     C                   UPDATE    ndcpa000
093800950302    2C                   ENDIF
093900981118    2C                   ENDIF
094000930514     C*
094100960625    1C                   END
094200981118    1C                   END
094300930513     C*
094400950203     C                   MOVEL     §3ATB1        UNOTB1            1
094500920601     C*
094600941130     C* SE CAUSALE VARIAZIONE I0 --> AGGANCIO ARCHIVIO C/ASSEGNI
094700140407    2C*    COMCVB        IFEQ      'I0'
094800140407     C*    §3AFCA        ANDNE     0
094900140407     c                   if        (comcvb='I0' or comcvb='I7' or comcvb='I8'
095000140407     c                             or comcvb='I9') and §3afca<>0
095100930513     C*
095200920212     C                   EXSR      CONCSB
095300941130     C* ERRORE --> NON TROVATO RECORD IN CSB E NON TROVATO LEGAME
095400941130     C   25              GOTO      ENDAGI
095500950227     C* SE IL C/A E' SOLO INCASSATO, POSSO EFFETTUARE LA VARIAIZONE DI
095600950227     C*  DESTINATARIO
095700950227     C  N42CSBBNA        IFEQ      0
095800950227     C                   SETOFF                                       31
095900950227     C                   ENDIF
096000920130     C*
096100950227     C* CONTRASSEGNO ESISTENTE GIA' PAGATO
096200941130    3C     *IN31         IFEQ      '1'
096300930303     C*
096400930303     C* SE ESISTE LEGAME BOLLA (42 ON) NON AGGIORNO ANAGRAFICA C/ASS
096500930303     C* MA AGGIORNO LA BOLLA
096600930303     C*
096700930513    4C     *IN42         IFEQ      '1'
096800930303     C                   SETOFF                                       31
096900930513   X4C                   ELSE
097000941202     C* CONTROLLO SE LA RAGIONE SOCIALE DESTINATARIO E' MODIFICATA
097100941202     C*  --> ERRORE SE C/A GIA' INCASSATO-PAGATO
097200941202     C                   EXSR      CTRDES
097300930513     C*
097400930513    4C                   END
097500920601     C* STAMPO ERRORE
097600941130     C     *IN31         IFEQ      *ON
097700941130     C     *IN42         ANDEQ     *OFF
097800110214     c* ES-14 02 11 -da oggi non stampo più C/Ass pagato per la 102 ma lo stampo
097900110214     c*  per la linea di arrivo se ci fossero problemi o contestazioni la colpa...è sua
098000110214     c* FNARBM non mi serve non accendo il 36
098100130326     c****               movel     taslna        COMFEV
098200110214     C****               SETON                                        36
098300110214     C****               Z-ADD     102           COMFEV
098400130326     c                   z-add     46            COMFEV
098500941130     C                   MOVEL     ERR(10)       ARBDER
098600941130     C                   EXSR      WTRERR
098700930513    3C                   END
098800941130    3C                   END
098900920601     C*
099000941130    3C  N42*IN31         IFEQ      '0'
099100040721     C**  N27
099200040721     C**ANN24              MOVEL     ARBFIN        CSBFIN
099300920115     C                   MOVEL     ARBLOD        CSBPAR
099400920115     C                   MOVEL     ARBPRD        CSBPRO
099500920115     C                   MOVEL     ARBRSD        CSBRSD
099600941130     C                   UPDATE    TNCSB000
099700930513    3C                   END
099800930513    2C                   END
099900920115     C*
100000990625     c                   exsr      agarbi1
100100920113     C*
100200920113     C* S E C O N D A   B O L L A
100300920113     C*
100400930513    3C     §3ATB2        IFNE      '  '
100500920113     C                   MOVEL     §3ATB2        COMTBL
100600941130     C                   SETOFF                                       020304
100700990624     C     KBLP          CHAIN     TiTAS30C                           39
100800121108     c                   exsr      memo_dstitas
100900990628     c                   move      tasksc        ksc4
101000990625     c                   exsr      agarbi1
101100930513    3C                   END
101200941130     C*
101300941130     C     ENDAGI        TAG
101400920113     C*
101500920113     C                   ENDSR
101600990625     C**---------------------------------------------------------
101700990625     C** AGGIORNO CODICE VARIAZIONI IX
101800990625     C**---------------------------------------------------------
101900990630     C     AGARBI1       BEGSR
102000990628     C* I SEGUENTI CAMPI LI AGGIORNO SOLO SE 1 BOLLA NON CONTABILIZZATA
102100990628    1C     *in27         IFEQ      *off
102200990628     C                   Z-ADD     ARBPKb        TASPKB
102300990628     C                   Z-ADD     ARBPKF        TASPKf
102400151103     c* non devo mai aggiornare questo flag perchè quando si tassa fino all'imponibile
102500151103     c*  viene impostato sia qui che nel record  FAT e non deve più essere modificato
102600151103     C********           movel     'R'           TASFPF
102700990625     C*
102800990628    2C     TASFVF        IFNE      'T'
102900990625     C     TASVLF        ANDNE     ARBVLF
103000990628    3C     TASFVF        IFEQ      'Z'
103100990625     C     TASVLF        ANDLT     ARBVLF
103200990625     C     TASFVF        ORNE      'Z'
103300990625     C                   Z-ADD     ARBVLF        TASVLF
103400990625     C                   MOVEL     ARBFVF        TASFVF
103500990625     C                   Z-ADD     ARBVLB        TASVLB
103600990625     C                   MOVEL     ARBFVB        TASFVB
103700990628    3C                   END
103800990628    2C                   END
103900990625     C*
104000990630    2c     ksc42bo       ifne      9999
104100990625     C* DETERMINO IL CODICE DI TASSAZIONE
104200990630    3C     COMCVB        ifeq      'I0'
104300140407    3C     COMCVB        oreq      'I7'
104400140407    3C     COMCVB        oreq      'I8'
104500140407    3C     COMCVB        oreq      'I9'
104600990628     c* la call al tisi mi basta farla solo la prima volta e cioe quando
104700990625     c* sono sulla prima bolla
104800990630    4c     comtbl        ifeq      §3atb1
104900120322     c                   clear                   savcts
105000120322     c                   exsr      c_tisi95
105100120322     C* SE ERRORE da tisi95R LO SCRIVO NEL FILE DEGLI ERRORI E NON LO ANDRO'
105200990625     C* A SOSTITUIRE
105300990630    5C     O95ERR        IFNE      *BLANKS
105400990625     C                   MOVEL     ERR(30)       ARBDER
105500990625     C                   Z-ADD     102           COMFEV
105600990625     C                   EXSR      WTRERR
105700990630   X5C                   ELSE
105800990630     C                   MOVEL     O95CTS        SAVCTS
105900990630    5C                   END
106000990630    4c                   END
106100990630    4c     SAVCTS        IFNE      *BLANKS
106200990630     C                   MOVEL     SAVCTS        TASCTS
106300990630    4C                   END
106400990630    3C                   END
106500990625
106600170518     C****               MOVEL     ARBFIN        TASFIN
106700990625     C                   MOVEL     ARBCAD        TASCAD
106800990625     C                   MOVEL     ARBNZD        TASNZD
106900990630    2C                   END
107000990630    1C                   END
107100990625     C*
107200990625RM*  C                   MOVEL     ARBFFD        TASFFD
107300990625     C*
107400990628     c* memorizzo se sono variati inoltro,cap o nazione
107500990625     c                   clear                   flgmod
107600990628    1c     arbfin        ifne      tasfin
107700990625     c     arbcad        orne      tascad
107800990625     c     arbnzd        orne      tasnzd
107900990625     c                   move      '1'           flgmod            1
108000990628    1c                   endif
108100990625     c
108200170518     C                   MOVEL     ARBFIN        TASFIN
108300170518
108400990628     c* Se ho la seconda bolla contabilizzata 9999 non aggiornerò mai i
108500990628     c* i seguenti campi nemmeno sulla prima bolla.
108600990630     c* Dopodichè posso aggiornarli solo se la 1bolla non è contabilizzata
108700990628     c* o se non sono stati modificati inoltro cap e nazione
108800990628    1c     ksc42bo       ifne      9999
108900990628    2c     *in27         ifeq      *off
109000990628     c     flgmod        oreq      *blank
109100990625     C                   MOVEL(P)  ARBRSD        TASRSD
109200170123     c* aggiorno la seconda ragione sociale
109300170123     c                   exsr      sr_rd2
109400990625     C                   MOVEL(P)  ARBIND        TASIND
109500990625     C                   MOVEL(P)  ARBLOD        TASLOD
109600990625     C                   MOVEL(P)  ARBPRD        TASPRD
109700990701     c* Dalla filiale posso ricevere la partita iva del destinatario
109800990701     c* solo se la bolla è assegnato o solo in caso di doppia bolla
109900990701     c* In ogni caso vado SEMPRE in sostituzione di tascpd
110000990625     C                   MOVEL     ARBCPI        TASCPD
110100140219    2c                   endif
110200140219    1c                   endif
110300990625     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
110400130325     C***                MOVEL     ERR(9)        ARBDER
110500130325     c***                exsr      wtrerr
110600140219     c* La scrittura/aggiornamento del tivrb viene fatta sempre se la
110700140219     c* prima bolla è contabilizzata.
110800140219     c* Viene fatta a prescindere dal contenuto di FLGMOD perchè
110900140219     c* deve receprire anche eventuali variazioni che riportano
111000140219     c* i dati alla situazione originale ciè quella di titas
111100140219     c* Es: se prima vario inoltor da "P" a "I" e poi rimetto
111200140219     c* "P" devo receprie sul tivrb anche quest'ultima variazione
111300140219    1c                   if        *in27=*on
111400120321     c                   exsr      ctr_vrb
111500140219    1c                   END
111600140219   x1c*                  else
111700990628     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
111800130325     C***                MOVEL     ERR(9)        ARBDER
111900130325     c***                exsr      wtrerr
112000140219     c*                  exsr      ctr_vrb
112100140219    1c*                  END
112200990625     C* AGGIORNO TITAS
112300990625     C   02              UPDATE    TITAS000
112400990625     C   03              UPDATE    TITAS010
112500990625     C   04              UPDATE    TITASP00
112600990625     C*
112700990625     c                   endsr
112800170123     C**---------------------------------------------------------
112900170123     C** Aggiornamento seconda ragione sociale destinatario
113000170123     C**---------------------------------------------------------
113100170123     C     sr_rd2        BEGSR
113200170123     C* Aggiorna record Q di TITA4
113300170123     C                   movel     'D'           §trc
113400170123     c                   setoff                                       0506
113500170123     C     ktita         chain     tita430c                           32
113600170123     c* se non c'e' lo scrivo
113700170123    1c                   if        *in32
113800170123     c                   movel     arbaas        ta4aas
113900170123     c                   movel     arblnp        ta4lnp
114000170123     c                   movel     arbnrs        ta4nrs
114100170123     c                   movel     arbnsp        ta4nsp
114200170123     c                   movel     'D'           ta4trc
114300170123     c                   clear                   ta4not
114400170123    1c                   endif
114500170123     c
114600170123     c                   eval      ta4not = arbrd2
114700170123     c*
114800170123    1c                   if        not *in32
114900170123    2C                   IF        TA4NOT<>*BLANKS
115000170123     C   05              update    tita4000
115100170123     C   06              update    tita4p00
115200170123     C                   ELSE
115300170123     C*SE eliminata e non c'e' più nulla, cancello il record
115400170123     c   05              delete    tita4000
115500170123     c   06              delete    tita4p00
115600170123    2c                   endif
115700170123     c
115800170123   x1c                   else
115900170123    2c                   if        ta4not<>*blanks
116000170123     C  N04              write     tita4000
116100170123     C   04              write     tita4p00
116200170123    2c                   endif
116300170123     c*
116400170123    1c                   endif
116500170123     c                   endsr
116600120322     C**---------------------------------------------------------
116700120322     C** Richiamo tisi95 per determinazione codice tassazione
116800120322     C**---------------------------------------------------------
116900120322     C     C_tisi95      BEGSR
117000120322     C                   CLEAR                   DSSI95
117100120322     C                   MOVEL     ARBCAD        I95CAP
117200120322     C                   MOVEL     ARBNZD        I95NAR
117300120322     C                   MOVEL     ARBLOD        I95LOC
117400120322     C                   MOVEL     ARBPRD        I95PRV
117500121116     c                   movel     tasaas        i95dat
117600121116     c                   move      tasmgs        i95dat
117700121116     C****               MOVEL     TASDSP        I95DAT
117800120322     C                   MOVEL     '7'           I95TCN
117900120322      * Se network FedEx passo flag apposito per dire di prendere i dati
118000120322      * dalla tabella della nazione
118100120322     c     taslna        chain     azorg01l
118200120322     c                   if        %found(azorg01l) and orgfva = *blanks
118300120322     c                   eval      og143 = orgde3
118400120322     c     §ogntw        ifeq      'FED'
118500120322     c                   eval      i95fi1 = 'S'
118600120322     c                   endif
118700120322     c                   endif
118800120322
118900120322     C                   MOVEL     §3ATB1        I95TPO
119000120322    5C     §3AFCA        IFNE      0
119100120322     C                   MOVEL     'S'           I95FCA
119200120322    5C                   ENDIF
119300120322     C**
119400120322     C                   CALL      'TISI95R'
119500120322     C                   PARM                    DSSI95
119600120322     c                   endsr
119700950202     C**---------------------------------------------------------
119800950202     C** AGGIORNO CODICE VARIAZIONI MX
119900950202     C**---------------------------------------------------------
120000950202     C     AGARBM        BEGSR
120100950203     C* VEDO SE ESISTE CODICE
120200950202     C                   MOVE      ARBCCM        KSC4
120300950202     C*
120400950202     C* SE E' CODIFICATO CHAIN SU ANAGRAFICA
120500950202     C     KSC4          IFNE      8888
120600981001     C                   CLEAR                   DSBS69
120700981001     C                   MOVEL     KNSIF         I69SIF
120800981001     C                   MOVEL     KCI           I69KCC
120900981001     C                   MOVEL     ARBCCM        I69KAC
121000981001     C                   MOVEL     ARBCCM        I69KIN
121100981001     C                   CALL      'TIBS69R'
121200981001     C                   PARM                    DSBS69
121300981001     C                   PARM                    DSACO
121400981001     C                   PARM                    DSIND
121500981001     C                   PARM                    DSCLP
121600981001     C                   PARM                    DSCLS
121700950202     C*
121800950203     C* DO ERRORE SOLO SE SI TRATTA DI CLIENTE DI ALTRA FILIALE
121900981001     C     O69ERR        IFNE      ' '
122000170109     C*  ATTENDO L'ARRIVO DEL PDC
122100950203     C                   SETON                                        37
122200950202     C                   GOTO      ENDGBM
122300960919     C                   ELSE
122400960919     C* SE CLIENTE BLOCCATO CARICO LO STESSO MA SEGNALO
122500100303     C***  ACOABL        IFEQ      '8'
122600100303     C***                MOVEL     ERR(29)       ARBDER
122700100303     C***                EXSR      WTRERR
122800100303     C***                END
122900950202     C                   ENDIF
123000950202     C                   ENDIF
123100950203     C*
123200950203     C* VERIFICO ESISTENZA NAZIONE E PROVINCIA MITTENTE
123300970714     C                   CLEAR                   DSSI95
123400970714     C                   MOVEL     ARBCAM        I95CAP
123500121116     C                   MOVEL     ARBNZM        I95NAR
123600121116     c                   movel     tasaas        i95dat
123700121116     c                   move      tasmgs        i95dat
123800121116     C****               MOVEL     TASDSP        I95DAT
123900970714     C                   MOVEL     '3'           I95TCN
124000020311      * Se network FedEx passo flag apposito per dire di prendere i dati
124100020311      * dalla tabella della nazione
124200020311     c     arblnp        chain     azorg01l
124300020311     c                   if        %found(azorg01l) and orgfva = *blanks
124400020311     c                   eval      og143 = orgde3
124500020311     c     §ogntw        ifeq      'FED'
124600020311     c                   eval      i95fi1 = 'S'
124700020311     c                   endif
124800020311     c                   endif
124900020311
125000970714     C                   CALL      'TISI95R'
125100970714     C                   PARM                    DSSI95
125200950203     C*
125300970714     C     O95ERR        IFNE      *BLANKS
125400130326     C                   Z-ADD     102           COMFEV
125500950203     C                   MOVEL     ERR(26)       ARBDER
125600950203     C                   EXSR      WTRERR
125700950203     C                   GOTO      ENDGBM
125800950203     C                   ENDIF
125900950203     C*
126000950203     C* VERIFICO SE ESISTEN L'ANAGRAFICA DEI C/A E SE GIA' INCASSATO
126100950222    1C     §3AFCA        IFNE      0
126200950202     C                   EXSR      CONCSB
126300950222     C* INESISTENTE  --> NON AGGIORNO
126400950222     C   25              GOTO      ENDGBM
126500950203     C***
126600950203     C*** A G G I O R N O   C / A S S E G N O
126700950222     C* AGGIORNO SOLO SE NON E' INCASSATO
126800950222   1AC     *IN31         IFEQ      *OFF
126900950202     C     KANN          CHAIN     TNCSA000                           30
127000950202     C*
127100950202     C* NON E' 8888 :SE CREATA CANCELLO ANAGRAFICA C/A
127200950202IF  2C     KSC4          IFNE      8888
127300970916IF  2C     KSC4          ANDNE     9999
127400950202     C  N30              DELETE    TNCSA000
127500950202X   2C                   ELSE
127600950202     C                   CLEAR                   TNCSA000
127700950202     C*
127800950202     C                   MOVEL     CSBAAS        CSAAAS
127900950202     C                   MOVEL     CSBLNP        CSALNP
128000950202     C                   MOVEL     CSBNRS        CSANRS
128100950202     C                   MOVEL     CSBNSP        CSANSP
128200950202     C                   MOVEL     ARBRSM        CSARSD
128300950202     C                   MOVEL     ARBINM        CSAVID
128400950202     C                   MOVEL     ARBCAM        CSACAD
128500950202     C                   MOVEL     ARBLOM        CSACID
128600950202     C                   MOVEL     ARBPRM        CSAPRM
128700950202     C                   MOVEL     ARBNZM        CSANZM
128800950202     C*
128900950202     C  N30              UPDATE    TNCSA000
129000950202     C   30              WRITE     TNCSA000
129100950202E   2C                   ENDIF
129200950202     C*
129300950202     C                   MOVEL     ARBCCM        CSBCDI
129400950202     C*
129500950202     C* SE C/A SU 1 BOLLA IMPOSTO CODICE BOLLA AMCHE
129600950202    2C     §3AFCA        IFEQ      1
129700950202     C                   MOVEL     ARBCCM        CSBKSC
129800950202    2C                   ENDIF
129900950202     C*
130000950202     C                   UPDATE    TNCSB000
130100950222   1AC                   ENDIF
130200950222    1C                   ENDIF
130300950202     C*
130400950203     C* A G G I O R N O   L A   1  B O L L A
130500970714     C                   MOVEL     O95CTS        TASMCT
130600950202     C                   MOVEL     ARBCAM        TASCAM
130700950202     C                   MOVEL     ARBNZM        TASNZM
130800950202     C                   MOVEL     ARBFAP        TASFAP
130900950202     C                   MOVEL     ARBCCM        TASKSC
131000990628     C                   MOVEL     ARBCCM        TASccm
131100950202     C*
131200950202     C*
131300990628     c                   movel     'M'           §trc
131400990629     c     ktita         chain     titaa000                           30
131500950202IF  3C     KSC4          IFEQ      8888
131600990628     c   30              z-add     tasaas        taaaas
131700990628     c   30              z-add     taslnp        taalnp
131800990628     c   30              z-add     tasnrs        taanrs
131900990628     c   30              z-add     tasnsp        taansp
132000990628     c   30              movel     §trc          taatrc
132100990628     C                   MOVEL     ARBRSM        TAaRSc
132200990628     C                   MOVEL     ARBINM        TAaIND
132300990628     C                   MOVEL     ARBCAM        TAaCAP
132400990628     C                   MOVEL     ARBLOM        TAaLOc
132500990628     C                   MOVEL     ARBPRM        TAaPRV
132600990628     C                   MOVEL     ARBNZM        TAaNAZ
132700990628     C                   MOVEL     ARBCPI        TAacpi
132800990628     c  n30              update    titaa000
132900990628     c   30              write     titaa000
133000990628     c                   else
133100990628     c* se prima della variazione era un 8888 lo vado a cancellare da
133200990628     c* titaa00f
133300990628     c  n30              delete    titaa000
133400990628     c
133500950202E   3C                   ENDIF
133600950202     C*
133700990628     C   02              UPDATE    TiTAS000
133800071029     C***
133900950203     C*** A G G I O R N O   2   B O L L A
134000950202    3C     §3ATB2        IFNE      '  '
134100950202     C                   MOVEL     §3ATB2        COMTBL
134200950202     C                   SETOFF                                       020304
134300990628     C     KBLP          CHAIN     TiTAS30C                           39
134400950202     C*
134500950203     C     *IN39         IFEQ      *OFF
134600950202     C                   MOVEL     ARBCCM        TASCCM
134700950202     C*
134800950202     C                   MOVEL     ARBCAM        TASCAM
134900950202     C                   MOVEL     ARBNZM        TASNZM
135000950202     C                   MOVEL     ARBFAP        TASFAP
135100970714     C                   MOVEL     O95CTS        TASMCT
135200950202     C*
135300990628     C   02              UPDATE    TiTAS000
135400990628     C   03              UPDATE    TiTAS010
135500990628     C   04              UPDATE    TiTASP00
135600950202     C                   ENDIF
135700950202     C                   ENDIF
135800950202     C*
135900950202     C     ENDGBM        ENDSR
136000950202     C**---------------------------------------------------------
136100950202     C** AGGIORNO CODICE VARIAZIONI CX
136200950202     C**---------------------------------------------------------
136300950202     C     AGARBC        BEGSR
136400010126     C     arblnp        ifne      SAVLNP
136500010126     C     arblnp        chain     azorg01l                           32
136600010126     C  n32              movel     orgde3        og143
136700010126     C   32              clear                   og143
136800010126     C                   z-add     arblnp        savlnp
136900010126     C                   endif
137000010126     C* SE BOLLA POSTE ED AGGIUNTO O TOLTO IL FUORI MISURA BISOGNA
137100010126     C* AGGIORNARE tita4 TIPO RECORD "J"
137200010126     C     §ogpt         ifeq      'S'
137300010126     C                   clear                   wtcp
137400010126     C                   clear                   wsvtcp
137500010126    2C     arbtc1        ifeq      'F'
137600010126     C     arbtc2        oreq      'F'
137700010126     C                   movel     'F'           wtcp
137800010126    2C                   endif
137900010126    2C     tasftc        ifeq      'F'
138000010126     C     tastc2        oreq      'F'
138100010126     C                   movel     'F'           wsvtcp
138200010126    2C                   endif
138300010126    2C                   select
138400010126     C* SE NON E' STATA NE' AGGIUNTA NE' TOLTA LA "F" NON FACCIO NIENTE
138500010126     C     arbtc1        whenne    'F'
138600010126     C     arbtc2        andne     'F'
138700010126     C     tasftc        andne     'F'
138800010126     C     tastc2        andne     'F'
138900010126     C     wtcp          oreq      'F'
139000010126     C     wsvtcp        andeq     'F'
139100010126     C                   other
139200010126     C                   movel     'J'           §trc
139300010126     c                   setoff                                       0506
139400010126     C     ktita         chain     tita430c                           32
139500010126    3C     *in32         ifeq      *off
139600010126     C                   movel     ta4not        dsbl4j
139700010126     C* TOLTO LA 'F'
139800010126    4C     wtcp          ifeq      *blanks
139900010126     C     wsvtcp        andeq     'F'
140000010126     C                   clear                   §b4bai
140100010126   X4C                   else
140200010126     C* AGGIUNTA 'F'
140300010126    5C     wtcp          ifeq      'F'
140400010126     C     wsvtcp        andeq     *blanks
140500010126     C                   movel     'S'           §b4bai
140600010126    5C                   endif
140700010126    4C                   endif
140800010126     C                   movel     dsbl4j        ta4not
140900010126     C   05              update    tita4000
141000010126     C   06              update    tita4p00
141100010126   X3C                   else
141200010126     C* AGGIUNTA 'F' --> CREO RECORD "J"
141300010126    4C     wtcp          ifeq      'F'
141400010126     C     wsvtcp        andeq     *blanks
141500010126     C                   clear                   tita4000
141600010126     C                   z-add     tasaas        ta4AAS
141700010126     C                   z-add     taslnp        ta4LNP
141800010126     C                   z-add     tasnrs        ta4NRS
141900010126     C                   z-add     tasnsp        ta4NSP
142000010126     C                   movel     'J'           ta4TRC
142100010126     C                   clear                   dsbl4j
142200010126     C                   clear                   ta4not
142300010126     C                   movel     'S'           §B4BAI
142400010126     C                   movel     dsbl4j        ta4NOT
142500010126     C  N04              write     tita4000
142600010126     C   04              write     tita4p00
142700010126    4C                   endif
142800010126    3C                   endif
142900010126    2C                   endsl
143000010126     C                   endif
143100950202     C* AGGIORNO SEMPRE  LE CONSEGNE PARTICOLARI SOLO SU  1 BOLLA
143200991018     c* idem per gg chiusura e data consegna richiesta
143300950202     C                   MOVEL     ARBTC1        TASFTC
143400950202     C                   MOVEL     ARBTC2        TASTC2
143500991018     c                   movel     arbtcr        tastcr
143600991018     c                   z-add     arbdcr        tasdcr
143700991018     c                   z-add     arbhcr        tashcr
143800991018     C                   MOVEL     arbgC1        tasgc1
143900991018     C                   MOVEL     arbgC2        tasgc2
144000990628     C   02              UPDATE    TiTAS000
144100990628     C   03              UPDATE    TiTAS010
144200990628     C   04              UPDATE    TiTASP00
144300950202     C                   ENDSR
144400950202     C*
144500920113     C**---------------------------------------------------------
144600920113     C** AGGIORNO CODICE VARIAZIONI TX
144700920113     C**---------------------------------------------------------
144800920113     C     AGARBT        BEGSR
144900920113     C*
145000920226     C                   EXSR      CTR2
145100920226     C*
145200941130    1C  N29              DO
145300950202     C*
145400991102     c
145500990419     c* controllo l'importo da assicurare se la spedizione e' tassata
145600990419     c*  o con tot.imponibile
145700011029     c                   if        tasias > 0  and tasvas = *blanks
145800011029     c                   movel     'ITL'         tasvas
145900011029     c                   endif
146000990419     c                   if        arbias <> tasias or
146100990419     c                             arbvas <> tasvas
146200990419     c*
146300990419     c                   exsr      AGGIAS
146400990419     c                   endif
146500950202     C*
146600950202     C                   Z-ADD     ARBIAS        TASIAS
146700950202     C                   MOVEL     ARBVAS        TASVAS
146800990824     c                   clear                   TASFCM
146900920113     C*
147000941130    2C     §3ATB2        IFEQ      '  '
147100970514     C     COMCVB        OREQ      'TP'
147200950222     C* AGGIORNO KSC SOLO SE VARIAZIONE DA ARRIVO E NON DA PARTENZA
147300950222     C*  PERCHE' SI TRATTA DI VARIAZIONE DI MITTENTE E NON DI TASSAZION
147400950222     C     ARBCVB        IFNE      'TP'
147500920113     C                   Z-ADD     ARBKSC        TASKSC
147600950222     C                   ENDIF
147700051219     c* Non vado sopra al codice tariffa se bolla in assegnato tassata con
147800051219     c* tariffa mittente
147900051219     c     tasftm        ifeq      *blanks
148000051219     C     comcvb        oreq      'TP'
148100950222     C                   MOVEL     ARBCTR        TASCTR
148200051219     c                   endif
148300950202     C                   Z-ADD     ARBQFT        TASQFT
148400991015     c                   MOVEL     ARBDIV        TASDIV
148500920113     C                   Z-ADD     ARBPOR        TASPOR
148600920113     C                   MOVEL     ARBSV1        TASSV1
148700920113     C                   Z-ADD     ARBVA1        TASVA1
148800920113     C                   MOVEL     ARBSV2        TASSV2
148900941130     C                   Z-ADD     ARBVA2        TASVA2
149000950202     C                   MOVEL     ARBSV3        TASSV3
149100950202     C                   Z-ADD     ARBVA3        TASVA3
149200941130     C*
149300990628     C* SE ESITONO GIA' DELLE VARIE IN TITA700F LE CANCELLO
149400990628     C     KBLP          SETLL     TITA7000
149500990628     C     KBLP          READE     TITA7000                               30
149600941130     C*
149700941130    3C     *IN30         DOWEQ     *OFF
149800991015     C                   DELETE    TITA7000
149900991015     C     KBLP          READE     TITA7000                               30
150000941130    3C                   ENDDO
150100170109     C     KARBU         SETLL     FIARBU2l
150200170109     C     KARBU         READE     FIARBU2l                               30
150300950202     C*
150400950202     C     *IN30         DOWEQ     *OFF
150500990628     C                   CLEAR                   TiTA7000
150600950202     C                   Z-ADD     ARBAAS        TA7AAS
150700950202     C                   Z-ADD     ARBLNP        TA7LNP
150800950202     C                   Z-ADD     ARBNRS        TA7NRS
150900950202     C                   Z-ADD     ARBNSP        TA7NSP
151000950202     C                   MOVEL     TASTBL        TA7TBL
151100950202     C                   MOVEL     ARBSVN        TA7SVN
151200950202     C                   Z-ADD     ARBVAN        TA7VAN
151300990629     C                   WRITE     TITA7000
151400950202     C*
151500950202     C                   ADD       1             NUMRBU            7 0
151600170109     C                   DELETE    FIARBUtt
151700950202     C*
151800170110     C     KARBU         READE     FIARBU2L                               30
151900950202     C                   ENDDO
152000990628     c
152100941130     C*
152200920113     C                   Z-ADD     ARBDFT        TASDFT
152300920113     C                   Z-ADD     ARBNFT        TASNFT
152400920113     C                   Z-ADD     ARBFIV        TASFIV
152500941130     C                   MOVEL     ARBCEI        TASFEI
152600920113     C*
152700990624     C                   z-add     arbimv        TASimv
152800151120     c* Totale imponibile = 0 --> Cancello record FAT se presente e imposto fpf='R'
152900151120    3c                   if        tasimv=0
153000151120     c                   Eval      kar5trd='FAT'
153100151120     c     kar5          Chain     Fiar531c
153200151120     c                   if        %found(fiar531c)
153300151120     c   02              delete    fiar5000
153400151120     c                   endif
153500151118     c                   eval      tasfpf='R'
153600151120   x3c                   else
153700151120     c* Totale imponibile > 0 --> Scrivo record FAT se manca imposto in base a record FAT
153800151120     c*                           Imposto TASFPF in base al record FAT
153900151118     c                   clear                   dar5fat
154000151118     c                   Eval      kar5trd='FAT'
154100151118     c     kar5          Chain     Fiar531c
154200151120    4c                   If        %Found(Fiar531c)
154300151118     c                   Movel     ar5uni        dar5fat
154400151120   x4c                   else
154500151120     c                   Eval      Ar5Aas = ArbAas
154600151120     c                   Eval      Ar5Lnp = ArbLnp
154700151120     c                   Eval      Ar5Nrs = ArbNrs
154800151120     c                   Eval      Ar5Nsp = ArbNsp
154900151120     c                   Eval      Ar5Trd = 'FAT'
155000151120     c                   movel     tasvlf        §ar5vltas
155100151120     c                   movel     tasfvf        §ar5fvtas
155200151120     c                   movel     taspkf        §ar5pktas
155300151120     c                   movel     'R'           §ar5fptas
155400151120     c                   eval      §AR5DATAS=%dec(%date())
155500151120     c                   eval      §AR5HMTAS=%dec(%time())
155600151120     c                   eval      §AR5PRTAS=knmus
155700151120     c                   eval      §AR5NJTAS=knmtd
155800151120     c                   Movel(p)  dar5fat       Ar5uni
155900151120     c                   eval      ar5dac=%dec(%date())
156000151120     c                   eval      ar5orc=%dec(%time())
156100151120     c                   Eval      AR5FT1='T'
156200151120     c                   Eval      AR5DT1=99999999
156300151120     c                   Eval      AR5FT2='T'
156400151120     c                   Eval      AR5DT2=99999999
156500151120     c                   Eval      AR5FT3='T'
156600151120     c                   Eval      AR5DT3=99999999
156700151120     c   02              write     fiar5000
156800151120    4c                   endif
156900151120     c* Imposto TASFPF
157000151118     c                   select
157100151118     c                   when      §AR5FPTAS='R'
157200151118     c                   eval      tasfpf='R'
157300151118     c                   when      §AR5FPTAS='D'
157400151118     c                   eval      tasfpf='D'
157500151118     c                   other
157600151118     c                   eval      tasfpf='V'
157700151118     c                   endsl
157800151120    3c                   endif
157900941130    3C     ARBIFT        IFGT      0
158000941130     C                   MOVEL     '1'           TASFTS
158100920113     C                   ELSE
158200941130     C                   MOVEL     ' '           TASFTS
158300941130    3C                   END
158400941130    2C                   END
158500920113     C* AGGIORNO
158600990628     C   02              UPDATE    TiTAS000
158700941130    1C                   ENDDO
158800920113     C*
158900920113     C                   ENDSR
159000920113     C**---------------------------------------------------------
159100920113     C** AGGIORNO CODICE VARIAZIONI 2T
159200920113     C**---------------------------------------------------------
159300920113     C     AGAR2T        BEGSR
159400920113     C*
159500920226     C* SECONDA BOLLA
159600920113     C     §3ATB2        IFNE      '  '
159700920226     C                   EXSR      CTR2
159800920114     C*
159900920226     C  N29              DO
160000991015     C                   Z-ADD     ARBKSC        TASKSC
160100991015     C                   MOVEL     ARBCTR        TASCTR
160200991015     C                   movel     arbdiv        tasdiv
160300991015     C                   MOVEL     ARBSV1        TASSV1
160400991015     C                   Z-ADD     ARBVA1        TASVA1
160500991015     C                   MOVEL     ARBSV2        TASSV2
160600991015     C                   Z-ADD     ARBVA2        TASVA2
160700040114     c                   Movel     ArbSv3        TasSv3
160800040114     c                   Z-Add     ArbVa3        TasVa3
160900040114      * Se esistono già della varie su Tita7 le devo eliminare
161000040114     c     Kblp          Setll     Tita7000
161100040114     c                   Do        *Hival
161200040114     c     Kblp          Reade     Tita7000
161300040114     c                   If        %Eof
161400040114     c                   Leave
161500040114     c                   EndIf
161600040114     c                   Delete    Tita7000
161700040114     c                   EndDo
161800040114      * Scrivo il nuovo Tita7 e cancello il file di ricezione
161900170109     c     Karbu         Setll     Fiarbu2l
162000040114     c                   Do        *Hival
162100170109     c     Karbu         Reade     Fiarbu2l
162200170109     c                   If        %Eof(Fiarbu2l)
162300040114     c                   Leave
162400040114     c                   EndIf
162500040114     c                   Clear                   Tita7000
162600040114     c                   Z-add     ArbAas        Ta7Aas
162700040114     c                   Z-add     ArbLnp        Ta7Lnp
162800040114     c                   Z-add     ArbNrs        Ta7Nrs
162900040114     c                   Z-add     ArbNsp        Ta7Nsp
163000040114     c                   Movel     TasTbl        Ta7Tbl
163100040114     c                   Movel     ArbSvn        Ta7Svn
163200040114     c                   Z-add     ArbVan        Ta7Van
163300040114     c                   Write     Tita7000
163400040114     c                   Add       1             NumRbu
163500170109     c                   Delete    Fiarbutt
163600040114     c                   EndDo
163700040114
163800991015     C                   Z-ADD     ARBDFT        TASDFT
163900991015     C                   Z-ADD     ARBNFT        TASNFT
164000991015     C                   Z-ADD     ARBFIV        TASFIV
164100991015     C                   MOVEL     ARBCEI        TASFEI
164200040114     C**!!!              z-add     arbva1        TASimv
164300040114     c                   Z-add     ArbImv        TasImv
164400991015     C     ARBIFT        IFGT      0
164500941130     C                   MOVEL     '1'           TASFTS
164600920114     C                   ELSE
164700941130     C                   MOVEL     ' '           TASFTS
164800920114     C                   END
164900920113     C*
165000040114      * Aggiorno Titas
165100990628     C   02              UPDATE    TiTAS000
165200920113     C                   END
165300920130     C                   END
165400920113     C*
165500920113     C                   ENDSR
165600920127     C**---------------------------------------------------------
165700920127     C** AGGIORNO CODICE VARIAZIONI KX
165800920127     C**---------------------------------------------------------
165900920127     C     AGARBK        BEGSR
166000121109     c                   if        *in11='0' and *in16='0'
166100121109     c                   exsr      memo_csb
166200121109     c                   endif
166300121205     c                   clear                   wanncas
166400920212     C                   SETOFF                                       31
166500941130     C                   CLEAR                   TNCSB000
166600950202     C* NON AGGIORNO SE HO LETTO
166700950202    1C     *IN11         IFEQ      *OFF
166800950202     C     *IN16         ANDEQ     *OFF
166900920127     C* VARIA CONTRASSEGNO
167000941130    2C     ARBCBP        IFEQ      ARBCBN
167100920127     C* VEDO CHE TIPO BOLLA AGGANCIARE DAL FLAG C/ASSEGNO
167200920131     C* RIAGGANCIO SOLO LA SECONDA PERCHE' LA PRIMA L'HO GIA' AGGANCIAT
167300941130    3C     §3AFCA        IFEQ      2
167400920127     C                   MOVEL     §3ATB2        COMTBL
167500941130     C                   SETOFF                                       020304
167600990625     C     KBLP          CHAIN     TITAS30C                           39
167700121108     c                   exsr      memo_dstitas
167800941130    3C                   END
167900920228     C*
168000920228     C* CONTROLLO SPEDIZIONE SE OK
168100990414     C                   EXSR      CTR3
168200920210     C*
168300920212     C* CONTROLLO ARCHIVIO C/ASSEGNO SE ERA PREVISTO
168400941130    3C     §3AFCA        IFNE      0
168500920212     C                   EXSR      CONCSB
168600941130    3C                   END
168700920210     C* SE NO ERRORE
168800950324    3C  N31CSBDDC        IFEQ      0
168900950324     C     CSBBNA        ANDEQ     0
169000941130     C                   MOVEL     *BLANKS       ARBDER
169100941130     C*
169200920228     C*** AGGIORNO CONTRASSEGNO
169300920228     C*** PRIMA     ESISTEVA ORA     ESISTE: V A R I A Z I O N E
169400941130    4C     CSBCAS        IFNE      0
169500950202     C     ARBCAS        ANDNE     0
169600950202     C* SCRIVO SOLO SE IMPORTO DIVERSO
169700950202    5C     ARBCAS        IFNE      CSBCAS
169800991103    5C     ARBVCA        ORNE      WCSBVCA
169900000703     c*
170000000703     c     csbsta        oreq      9
170100920212     C* SCRIVO STORICO
170200920212     C                   MOVEL     'VACA'        CSVCAV
170300920212     C                   EXSR      CSVWTR
170400950202     C*
170500950202     C                   EXSR      AGGCSB
170600950202     C*
170700941130     C                   UPDATE    TNCSB000
170800920228     C*
170900120420     C* E R R O R E : NON MODIFICABILE IMPORTO VARIA G
171000130325     C***29              MOVEL     ERR(22)       ARBDER
171100120322     C   29              exsr      ctr_vrb
171200941130    5C                   END
171300941130    4C                   END
171400920228     C***
171500920228     C*** PRIMA     ESISTEVA ORA NON ESISTE: A N N U L L O
171600941130    4C     CSBCAS        IFNE      0
171700950202     C     ARBCAS        ANDEQ     0
171800920212     C                   EXSR      ANNCSB
171900920226     C*
172000920228     C* E R R O R E : NON ELIMINABILE IMPORTO VARIA G
172100920316     C                   MOVEL     *BLANKS       ARBDER
172200990505    5C   29              DO
172300941130     C* CONTROLLO SE ESISTE VARIA "G"
172400130326     C****               EXSR      ESVARG
172500130325    6C***  WSIVAR        IFEQ      'S'
172600130325     C***                MOVEL     ERR(14)       ARBDER
172700130325    6C***                END
172800121205     c                   eval      wanncas='S'
172900121203     c                   exsr      ctr_vrb
173000941130    5C                   END
173100941130    4C                   END
173200920228     C***
173300920228     C*** PRIMA NON ESISTEVA ORA     ESISTE: C R E O  RECORD C/ASSEGNO
173400941130    4C     CSBCAS        IFEQ      0
173500950202     C     ARBCAS        ANDNE     0
173600920212     C                   MOVEL     §3ATB1        TBLCA
173700941130     C                   MOVEL     §3ATB1        FLGTB1
173800941130     C*
173900941130     C                   MOVEL     TASLOD        DESPAR
174000990625     C                   MOVEL     TASPRD        DESPRV
174100950913     C*
174200920212     C                   EXSR      CRTCSB
174300920228     C* E R R O R E : NON POSSO AGGIUNGERE IMPORTO VARIA G
174400130325     C***29              MOVEL     ERR(18)       ARBDER
174500130325     c***29              setoff                                       36
174600120322     C   29              exsr      ctr_vrb
174700941130    4C                   END
174800920228     C***
174900920228     C*** AGGIORNO SPEDIZIONE
175000941130     C                   MOVEL     'S'           TASFCA
175100920228     C* SE GIA' TASSATA METTO A POSTO PER VARIA G
175200110203     c                   if        *in38
175300110203     c                   Z-ADD     arbcas        wcas
175400110203     C                   EXSR      VARIAG
175500110203     c
175600110203      * riorganizzo le varie se è necessario
175700110203     c                   if        flgriorg = '1'
175800110203     c                   exsr      sr_riorg
175900110203     c                   endif
176000110203     c                   endif
176100110203
176200950202     C* SCRIVO ERRORE SE GIA' CONTABILIZZATA/STORICIZZATA
176300941130    4C     ARBDER        IFNE      *BLANKS
176400920309     C                   EXSR      WTRERR
176500941130    4C                   END
176600920228     C*
176700990625     C   02              UPDATE    TITAS000
176800990625     C   03              UPDATE    TITAS010
176900990625     C   04              UPDATE    TITASP00
177000941130    3C                   END
177100920127     C*
177200941130   X2C                   ELSE
177300920228     C******
177400920228     C****** M O D I F I C A T O   T I P O   B O L L A
177500920228     C******
177600920212     C                   SETOFF                                       31
177700920127     C                   MOVEL     §3ATB1        SAVTB1            2
177800920127     C                   MOVEL     §3ATB2        SAVTB2            2
177900920227     C                   MOVEL     §3AFCA        SAVFCA            1 0
178000920227     C                   MOVEL     §3AFTD        SAVFTD            1 0
178100941130    3C     §3ABCM        IFNE      ' '
178200920324     C                   MOVEL     0             SAVFCA
178300941130    3C                   END
178400920127     C* NUOVO TIPO BOLLA
178500920127     C                   MOVEL     '3A'          COD
178600941130     C                   MOVEL(P)  ARBCBN        KEY
178700920212     C     KTAB          CHAIN     TABEL                              34
178800941130     C*
178900941130     C* COD BOLLA NUOVO NON TROVATO
179000941130    3C     *IN34         IFEQ      *ON
179100941130     C                   MOVEL     ERR(6)        ARBDER
179200920212     C*
179300941130     C                   SETON                                        37
179400941130     C                   Z-ADD     102           COMFEV
179500941130     C                   EXSR      WTRERR
179600941130     C**
179700941130   X3C                   ELSE
179800941130     C**
179900941130     C* COD BOLLA NUOVO TROVATO
180000920131     C                   MOVEL     TBLUNI        DS3A
180100941130    4C     §3ABCM        IFNE      ' '
180200920324     C                   Z-ADD     0             §3AFCA
180300941130    4C                   END
180400920131     C*
180500920212     C* ANNULLAMENTO CONTRASSEGNO
180600941130    4C     SAVFCA        IFNE      0
180700920212     C     §3AFCA        ANDEQ     0
180800920212     C                   EXSR      CONCSB
180900920212     C*
181000941209    5C     *IN31         IFEQ      *OFF
181100941209     C* IMPOSTO IL NUOVO TIPO BOLLA
181200941209    6C     SAVFCA        IFEQ      2
181300941209     C     §3ATB2        ANDNE     *BLANKS
181400941209     C                   MOVEL     §3ATB2        CSBTBL
181500941209     C                   ELSE
181600941209     C                   MOVEL     §3ATB1        CSBTBL
181700941209    6C                   ENDIF
181800941209     C*
181900941209     C                   EXSR      ANNCSB
182000941209    5C                   END
182100941209    4C                   END
182200950202     C***************
182300920212     C* SE C/A PASSA A  "F"  A  "A"  O VICEVERSA POSSO FARLO ANCHE SE
182400920212     C*  INCASSATO - BASTA CHE NON SIA MODIFICATO L'IMPORTO
182500941130    4C     SAVFCA        IFEQ      1
182600920212     C     §3AFCA        ANDEQ     2
182700920212     C                   EXSR      CONCSB
182800950630     C* Se da bolla singola a bolla doppia e C/A sulla 2 bolla non
182900950630     C* aggiorno adesso TNCSB ma solo dopo scrittura della 2 bolla su
183000950630     C* TNTAS
183100950630    5C     SAVTB2        IFEQ      '  '
183200950630     C     §3ATB2        ANDNE     '  '
183300950630     C                   UNLOCK    TNCSB03L                             99
183400950630     C                   ELSE
183500941130     C* ANAGRAFICA C/A TROVATA
183600950630    6C     *IN31         IFEQ      *OFF
183700941130     C* CREO STORICO SE MODIFICATO IMPORTO
183800950630    7C     CSBCAS        IFNE      ARBCAS
183900991103    7C     WCSBVCA       ORNE      ARBVCA
184000920212     C                   MOVEL     'VACA'        CSVCAV
184100920212     C                   EXSR      CSVWTR
184200950630    7C                   END
184300920212     C*
184400941130     C                   Z-ADD     9999          CSBKSC
184500941130     C                   MOVEL     TASLNA        CSBKSC
184600950202     C                   MOVEL     §3ATB2        CSBTBL
184700950202     C*
184800950202     C                   EXSR      AGGCSB
184900941130     C                   UPDATE    TNCSB000
185000950630    6C                   END
185100950630    5C                   END
185200941130    4C                   END
185300950202     C***************
185400941130    4C     SAVFCA        IFEQ      2
185500920212     C     §3AFCA        ANDEQ     1
185600920228     C                   EXSR      CHTB2
185700920212     C*
185800920228     C                   EXSR      CONCSB
185900941130     C*
186000941130    5C     *IN31         IFEQ      *OFF
186100920212     C* CREO STORICO SE MODIFICATO IMPORTO
186200950202    6C     ARBCAS        IFNE      CSBCAS
186300991102    6C     ARBVCA        ORNE      WCSBVCA
186400920212     C                   MOVEL     'VACA'        CSVCAV
186500920212     C                   EXSR      CSVWTR
186600941130    6C                   END
186700920212     C*
186800941130     C                   MOVEL     CSBCDI        CSBKSC
186900941130     C                   MOVEL     §3ATB1        CSBTBL
187000950202     C                   EXSR      AGGCSB
187100941130     C                   UPDATE    TNCSB000
187200941130    5C                   END
187300941130    4C                   END
187400950202     C***************
187500950202     C* MODIFICATO IMPORTO C/ASSEGNO
187600941130    4C     SAVFCA        IFEQ      1
187700920227     C     §3AFCA        ANDEQ     1
187800941130     C* CONTROLLO IL C/A
187900920227     C                   EXSR      CONCSB
188000941130     C*
188100950202    5C  N31CSBCAS        IFNE      ARBCAS
188200991103    5C     WCSBVCA       ORNE      ARBVCA
188300920227     C                   MOVEL     'VACA'        CSVCAV
188400920227     C                   EXSR      CSVWTR
188500941209    5C                   END
188600950202     C*
188700950202     C                   EXSR      AGGCSB
188800950202     C*
188900941209     C* IMPOSTO IL NUOVO TIPO BOLLA
189000941209     C                   MOVEL     §3ATB1        CSBTBL
189100941209     C  N31              UPDATE    TNCSB000
189200941130    4C                   END
189300950202     C***************
189400941130    4C     SAVFCA        IFEQ      2
189500920227     C     §3AFCA        ANDEQ     2
189600920228     C                   EXSR      CHTB2
189700920227     C*
189800920227     C                   EXSR      CONCSB
189900941130     C*
190000950202    5C  N31CSBCAS        IFNE      ARBCAS
190100991103    5C     WCSBVCA       ORNE      ARBVCA
190200920227     C                   MOVEL     'VACA'        CSVCAV
190300920227     C                   EXSR      CSVWTR
190400941209    5C                   END
190500950202     C*
190600950202     C                   EXSR      AGGCSB
190700950202     C*
190800941209     C* IMPOSTO IL NUOVO TIPO BOLLA
190900941209     C                   MOVEL     §3ATB2        CSBTBL
191000941209     C  N31              UPDATE    TNCSB000
191100941130    4C                   END
191200920212     C***
191300920227     C*** A  G  G  I  O  R  N  O       B  O  L  L  A
191400920212     C***
191500080408     c  N31              if        VariataSede=' '
191600941130     C                   CLEAR                   DESPAR
191700970916     C                   CLEAR                   DESPRV
191800920228     C***
191900920228     C*** DA BOLLA SINGOLA A BOLLA SINGOLA
192000941130    5C     SAVTB2        IFEQ      '  '
192100920227     C     §3ATB2        ANDEQ     '  '
192200110315     c
192300110315     c* Non lo faccio se devo aggiungere solo il C/Ass perchè la bolla
192400110315     c*  non la trovo col cod bolla vecchio ma col cod bolla nuovo
192500110315    6c                   if        SoloCAss=' '
192600110315     c
192700941130     C                   MOVEL     'N'           VARFCA            1
192800920228     C                   EXSR      AGGTB1
192900941130     C*
193000941130     C* IMPOSTO IL PAESE DI ARRIVO
193100941130     C                   MOVEL     TASLOD        DESPAR
193200990625     C                   MOVEL     TASPRD        DESPRV
193300110315    6C                   END
193400110315    5C                   END
193500920228     C***
193600920228     C*** DA BOLLA DOPPIA A BOLLA DOPPIA
193700941130    5C     SAVTB2        IFNE      '  '
193800920227     C     §3ATB2        ANDNE     '  '
193900920228     C* AGGIORNO 1 BOLLA
194000941130     C                   MOVEL     'N'           VARFCA            1
194100920228     C                   EXSR      AGGTB1
194200941130     C* AGGANCIO 2 BOLLA
194300941130     C                   EXSR      CHTB2
194400941130     C* IMPOSTO IL PAESE DI ARRIVO
194500941130     C                   MOVEL     TASLOD        DESPAR
194600990625     C                   MOVEL     TASPRD        DESPRV
194700920227     C*
194800941130     C   02
194900941130    6CANN29              DO
195000920227     C*
195100941130    7C   38SAVFCA        IFEQ      2
195200920228     C     §3AFCA        OREQ      2
195300990505     c* Se il C/A si trova sulla 2 bolla, li imposto, altrimenti lo azzero
195400990505     c*  per il controllo della varia G
195500990505     c                   if        §3afca=2
195600990505     c                   eval      wcas=arbcas
195700990505     c                   else
195800990505     c                   clear                   wcas
195900990505     c                   endif
196000990505     c
196100920228     C                   EXSR      VARIAG
196200941130    7C                   END
196300920227     C*
196400920227     C                   MOVEL     §3ATB2        TASTBL
196500990629     C                   MOVEL     arbcbn        TASCBO
196600011218
196700011218      * riorganizzo le varie se è necessario
196800011218     c                   if        flgriorg = '1'
196900011218     c                   exsr      sr_riorg
197000011218     c                   endif
197100011218
197200920227     C*
197300941201     C* ALTRIMENTI CAMBIO IL TIPO BOLAA ANCHE NEL TA7
197400990625     C     KBLP          SETLL     TiTA7000
197500990625     C     KBLP          READE     TiTA7000                               30
197600941201    8C     *IN30         DOWEQ     *OFF
197700941201     C                   MOVEL     §3ATB2        TA7TBL
197800990625     C                   UPDATE    TITA7000
197900990625     C     KBLP          READE     TITA7000                               30
198000941201    8C                   ENDDO
198100941130    6C                   END
198200920227     C*
198300990625     C   02              UPDATE    TITAS000
198400990625     C   03              UPDATE    TITAS010
198500990625     C   04              UPDATE    TITASP00
198600920227     C*
198700920227     C* ERRORI
198800941130     C  N02
198900941130     COR 02
199000941130    6CAN 29              DO
199100941130    7C     SAVTB2        IFNE      §3ATB2
199200130410     c* solo se tipo bolla nuovo non è uno storno
199300130410     c                   movel     'TB'          cod
199400130410     c                   movel(p)  §3atb2        key
199500130410     c                   clear                   dstb
199600130410     C     KTAB          CHAIN     TABEL
199700130410    8c                   if        %found(tabel00f)
199800130410     c                   movel     tbluni        dstb
199900130410    8c                   endif
200000130410    8c                   if        §tbfcb='1'
200100130326     c                   z-add     46            comfev
200200930513     C                   MOVEL     ERR(16)       ARBDER                          MODIF BOLLA
200300130410    8c                   endif
200400941130   X7C                   ELSE
200500920228     C*
200600130326     c                   z-add     46            comfev
200700930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF A3
200800941130    7C                   END
200900920228     C*
201000920227     C                   EXSR      WTRERR
201100941130    6C                   END
201200941130    5C                   END
201300920228     C***
201400920228     C*** DA BOLLA DOPPIA A BOLLA SINGOLA
201500941130    5C     SAVTB2        IFNE      '  '
201600920227     C     §3ATB2        ANDEQ     '  '
201700950630     C*
201800920228     C* AGGANCIO 2 BOLLA
201900920228     C                   EXSR      CHTB2
202000941130     C* IMPOSTO IL PAESE DI ARRIVO
202100941130     C                   MOVEL     TASLOD        DESPAR
202200990625     C                   MOVEL     TASPRD        DESPRV
202300920227     C*
202400941130     C   02
202500941130    6CANN29              DO
202600920227     C                   MOVEL     'AP'          TASTBL
202700991021     c                   movel     arbcbn        tascbo
202800941130     C*
202900941130     C* TOLGO FLAG C/A SOLO SE DALLA 2 BOLLA E' PASSATO ALLA 1
203000941130     C     SAVFCA        IFEQ      2
203100941130     C     §3AFCA        ANDEQ     1
203200941130     C                   MOVEL     ' '           TASFCA
203300941130     C                   ENDIF
203400941130     C*
203500990625     C                   UPDATE    TITAS000
203600941130    6C                   END
203700920227     C*
203800920227     C* ERRORI
203900990505    6C   29              DO
204000130326     c                   z-add     46            comfev
204100930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF BOLLA
204200920227     C                   EXSR      WTRERR
204300941130    6C                   END
204400920227     C**
204500920228     C** AGGIORNO 1 BOLLA
204600941130     C                   MOVEL     'S'           VARFCA
204700920228     C                   EXSR      AGGTB1
204800941130    5C                   END
204900920228     C***
205000920228     C*** DA BOLLA SINGOLA A BOLLA DOPPIA
205100941130    5C     SAVTB2        IFEQ      '  '
205200920227     C     §3ATB2        ANDNE     '  '
205300950630     C*
205400920228     C* AGGIORNO PRIMA BOLLA
205500941130     C                   MOVEL     'S'           VARFCA
205600920228     C                   EXSR      AGGTB1
205700920227     C*
205800000816     c* Prima di creare la seconda bolla cancello eventualmente bolla tipo
205900000816     c* "AP" che potrei avere in seguito ad un precedente cambio da bolla
206000000816     c* doppia a bolla singola
206100000816     c                   movel     'AP'          comtbl
206200000816     c                   setoff                                       020304
206300000816     c     kblp          chain     titas30c                           39
206400000816     c     *in39         ifeq      *off
206500000816     c     *in02         andeq     *on
206600000816     c     tasdft        ifeq      *zeros
206700000816     c     tasfic        oreq      ' '
206800000816     c                   delete    titas000
206900000816     c                   endif
207000000816     c                   endif
207100920227     C* CREO SECONDA BOLLA
207200920227     C*
207300950201     C* AGGANCIO FILE FNARBD1R
207400170109     c*  per sicurezza chaino bolla in FNBLP e aggiorno i dati
207500170109     c*  del destinatario da blp.
207600170109     C**   ARBORV        ADD       1             COMORV
207700170109     C**   KARBD         CHAIN     FNARBDRR                           32
207800170109     c
207900170109     C**N32              ADD       1             NUMRBD
208000920227     C* 32 ON - INESISTENTE: NON SCRIVO DESTINATARIO E SEGNALO ERRORE
208100170109    6C** 32              DO
208200170109     C**                 MOVEL     *BLANKS       ARBRSD
208300170109     C**                 MOVEL     *BLANKS       ARBRD2
208400170109     C**                 MOVEL     *BLANKS       ARBIND
208500170109     C**                 MOVEL     *BLANKS       ARBLOD
208600170109     C**                 MOVEL     *BLANKS       ARBCAD
208700170109     C**                 MOVEL     *BLANKS       ARBPRD
208800170109     C**                 MOVEL     *BLANKS       ARBNZD
208900920227     C*
209000170109     C**                 MOVEL     ERR(12)       ARBDER
209100170109     C**                 Z-ADD     102           COMFEV
209200170109     C**                 EXSR      WTRERR
209300170109    6C***                END
209400920227     C*
209500920227     C*
209600941130     C                   MOVEL     §3ATB2        TASTBL
209700990629     C                   MOVEL     ARBCBN        TASCBO
209800941130     C                   MOVE      TASKSC        KSC4
209900941130     C*
210000941130     C                   CLEAR                   TASRSD
210100941130     C                   CLEAR                   TASIND
210200990625     C                   CLEAR                   TASCAd
210300941130     C                   CLEAR                   TASLOD
210400990625     C                   CLEAR                   TASPRd
210500990625     C                   CLEAR                   TAScpd
210600170109     c
210700170109     c     kann          chain     fnblp01l
210800941130     C*
210900941130     C* MUOVO L'INIDIRIZZO COMPLETO DEL DESTINATARIO
211000170109     c                   if        %found(fnblp01l)
211100170109     C                   MOVEL     blpRSD        TASRSD
211200170109     C                   MOVEL     blpIND        TASIND
211300170109     C                   MOVEL     blpCAD        TASCAD
211400170109     C                   MOVEL     blpLOD        TASLOD
211500170109     C                   MOVEL     blpLOD        DESPAR
211600170109     C                   MOVEL     blpPRD        DESPRV
211700170109     C                   MOVEL     blpPRD        TASPRD
211800170109     C                   MOVEL     blpNZD        TASNZD
211900990701     c* Muovo comunque la p.iva in tascpd anche se in questo caso
212000990701     c* sarà sempre vuota perchè visto che prima della variazione
212100990701     c* la bolla era franco non poteva esserci in filiale la p.iva
212200990701     c* del destinatario memoziazzata su fnarb
212300170109     C*********          MOVEL     ARBCPI        TASCPD
212400170109     c                   endif
212500941130     C*
212600941130     C                   CLEAR                   TASPVL
212700941130     C                   CLEAR                   TASFTC
212800950202     C                   CLEAR                   TASTC2
212900941130     C                   CLEAR                   TASIAS
213000990824     C                   CLEAR                   TASFCM
213100950202     C                   CLEAR                   TASVAS
213200941130     C                   MOVEL     *ZEROS        TASCTR
213300930604     C                   MOVEL     *BLANKS       TASFTM
213400991018     C                   MOVEL     *BLANKS       TASDIV
213500920227     C                   MOVE      *ZERO         TASPOR
213600920227     C                   MOVE      *BLANK        TASSV1
213700920227     C                   MOVE      *ZERO         TASVA1
213800920227     C                   MOVE      *BLANK        TASSV2
213900920227     C                   MOVE      *ZERO         TASVA2
214000930212     C                   MOVE      *BLANK        TASSV3
214100930212     C                   MOVE      *ZERO         TASVA3
214200920227     C                   Z-ADD     0             TASDFT
214300920227     C                   Z-ADD     0             TASNFT
214400920227     C                   Z-ADD     0             TASFIV
214500930212     C                   MOVE      ' '           TASFA2
214600920227     C                   Z-ADD     9999          TASKSC
214700920227     C                   MOVEL     TASLNA        TASKSC
214800920227     C* CONTRASSEGNO A SECONDA DEL FLAG
214900941130    6C     §3AFCA        IFEQ      2
215000941130     C                   MOVEL     'S'           TASFCA
215100920227     C                   MOVEL     §3ATB2        TBLCA             2
215200920227     C                   ELSE
215300941130     C                   MOVEL     ' '           TASFCA
215400941130    6C                   END
215500920227     C*
215600990624     C                   MOVE      *zeros        TASimv
215700941130     C                   MOVEL     ' '           TASFEI
215800941130     C                   MOVEL     ' '           TASFTS
215900941130     C                   MOVEL     ' '           TASFIC
216000920227     C*
216100990625     C                   WRITE     TITAS000                             99
216200950630     C*
216300950630     C     SAVFCA        IFEQ      1
216400950630     C     §3AFCA        ANDEQ     2
216500950630     C     KANN          CHAIN     TNCSB000                           31
216600991103     c                   move      csbvca        wcsbvca
216700991103     c     csbcas        ifgt      0
216800991103     c     wcsbvca       andeq     *blanks
216900991103     c                   movel     'ITL'         wcsbvca
217000991103     c                   endif
217100950630     C     CSBCAS        IFNE      ARBCAS
217200991103     C     wcsbvca       ORNE      ARBVCA
217300950630     C                   MOVEL     'VACA'        CSVCAV
217400950630     C                   EXSR      CSVWTR
217500950630     C                   END
217600950630     C*
217700950630     C                   Z-ADD     9999          CSBKSC
217800950630     C                   MOVEL     TASLNA        CSBKSC
217900950630     C                   MOVEL     §3ATB2        CSBTBL
218000950630     C*
218100950630     C                   EXSR      AGGCSB
218200950630     C                   UPDATE    TNCSB000
218300950630     C                   END
218400920227     C*
218500941130     C* REIMPOSTO ORA DI VARIAZIONE DI ARBK: ORA DI ARBD - 1
218600170110     C* N32              SUB       1             ARBORV
218700170110     C* N32              DELETE    FNARBDRR
218800941130    5C                   END
218900920227     C*
219000920227     C* CREO SE NECESSARIO RECORD IN FILE CONTRASSEGNI
219100941130    5C     SAVFCA        IFEQ      0
219200941130    6C     §3AFCA        IFEQ      1
219300920227     C                   MOVEL     §3ATB1        TBLCA
219400941130     C* RICHAINO LA BOLLA
219500941130     C                   MOVEL     §3ATB1        COMTBL
219600941130     C                   SETOFF                                       020304
219700990625     C     KBLP          CHAIN     TITAS30C                           30
219800110315     c* Se solo C/Ass--> aggiungo il flag
219900110315     c                   if        SoloCAss='S'
220000110315     c                   eval      TASFCA='S'
220100110315     C                   MOVEL     TASLOD        DESPAR
220200110315     C                   MOVEL     TASPRD        DESPRV
220300110315     C   02              UPDATE    TITAS000
220400110315     C   03              UPDATE    TITAS010
220500110315     C   04              UPDATE    TITASP00
220600110315     c                   endif
220700950913     C*
220800920227     C                   EXSR      CRTCSB
220900941130     C*
221000920227     C                   ELSE
221100941130     C*
221200941130    7C     §3AFCA        IFEQ      2
221300920227     C                   MOVEL     §3ATB2        TBLCA
221400941130     C* RICHAINO LA BOLLA
221500941130     C                   MOVEL     §3ATB2        COMTBL
221600941130     C                   SETOFF                                       020304
221700990625     C     KBLP          CHAIN     TITAS30C                           30
221800920227     C                   EXSR      CRTCSB
221900941130    7C                   END
222000941130    6C                   END
222100941130    5C                   END
222200920227     C*
222300941130    4C                   END
222400941130    3C                   END
222500941130    2C                   END
222600930513     C*
222700930513     C* 11 ON - RECORD DESTINATARIO PER CREARE 2 BOLLA
222800941130   X1C                   ELSE
222900950202     C* NON CANCELLO IL RECORD : IN ARBM LO LASCIO LI'
223000950202     C                   SETON                                        37
223100950202     C*
223200950202     C     *IN11         IFEQ      *ON
223300930513     C                   MOVEL     102           COMFEV
223400930513     C                   MOVEL     ERR(15)       ARBDER
223500930513     C                   EXSR      WTRERR
223600950202     C                   ENDIF
223700950202    1C                   ENDIF
223800920227     C*
223900920227     C                   ENDSR
224000060317     C**---------------------------------------------------------
224100060317     C** AGGIORNO CODICE VARIAZIONE RM
224200060317     C**---------------------------------------------------------
224300060317     C     AGARBR        BEGSR
224400060317     c* rma in tita4 record "A"
224500060317     c                   setoff                                       0506
224600060317     C                   movel     'A'           §trc
224700060317     C     ktita         chain     tita430c                           32
224800060317    3C     *in32         ifeq      *off
224900060317     C                   movel     ta4not        dta4a
225000060317     c                   movel     arbrsd        §TA4ARMA
225100160121     C                   MOVEL     ARBrd2        §TA4anas
225200060317     C                   movel     dta4a         ta4not
225300060317     C   05              update    tita4000
225400060317     C   06              update    tita4p00
225500060317     c                   endif
225600060317     c*
225700160412     C****               MOVEL     ARBrd2        TASnas
225800160412     C** 02              UPDATE    TiTAS000
225900160412     C** 03              UPDATE    TiTAS010
226000160412     C** 04              UPDATE    TiTASP00
226100160412     c**
226200160412     c                   unlock    titas30c
226300060317     c*
226400060317     C                   ENDSR
226500920228     C**---------------------------------------------------------
226600920228     C** AGGIORNO PRIMA BOLLA
226700920228     C**---------------------------------------------------------
226800920228     C     AGGTB1        BEGSR
226900920228     C* AGGANCIO 1 BOLLA
227000920228     C                   EXSR      CHTB1
227100920228     C*
227200941201     C* SE E' DA AGGIUNGERE LO AGGIUNGO
227300920228     C     §3AFCA        IFEQ      1
227400941130     C                   MOVEL     'S'           TASFCA
227500941201     C*
227600920228     C                   ELSE
227700941201     C* LO TOLGO SOLO SE FALG IMPOSTATO = "S"
227800941201     C*
227900941201     C     VARFCA        IFEQ      'S'
228000941130     C                   MOVEL     ' '           TASFCA
228100920228     C                   END
228200941130     C                   END
228300920228     C*
228400941130     C   02
228500941130     CANN29              DO
228600920228     C   38SAVFCA        IFEQ      1
228700920228     C     §3AFCA        OREQ      1
228800990505     c* Devo impostare l'importo C/A nella routine varia G :
228900990505     c*  se il C/A e' stato tolto dalla 1 bolla, richiamo con importo=0
229000990505     c                   if        §3afca=1
229100990505     c                   eval      wcas=arbcas
229200990505     c                   else
229300990505     c                   clear                   wcas
229400990505     c                   endif
229500990505     c
229600920228     C                   EXSR      VARIAG
229700920228     C                   END
229800920228     C*
229900920228     C                   MOVEL     §3ATB1        TASTBL
230000990629     C                   MOVEL     ARBCBN        TASCBO
230100011219
230200011219      * riorganizzo le varie se è necessario
230300011219     c                   if        flgriorg = '1'
230400011219     c                   exsr      sr_riorg
230500011219     c                   endif
230600011219
230700000209     C* AGGIORNO TITA7000 SE ESISTE
230800000209     C     KBLP          SETLL     TITA7000
230900000209     C     KBLP          READE     TITA7000                               30
231000000209     C*
231100000209     C     *IN30         DOWEQ     *OFF
231200000209     C                   MOVEL     §3ATB1        TA7TBL
231300000209     C                   UPDATE    TITA7000
231400000209     C     KBLP          READE     TITA7000                               30
231500000209     C                   ENDDO
231600920228     C                   END
231700941130     C*
231800920228     C*
231900990625     C   02              UPDATE    TITAS000
232000990625     C   03              UPDATE    TITAS010
232100990625     C   04              UPDATE    TITASP00
232200920228     C*
232300990505     C* spedizione gia' contabilizzata
232400990505     C   29              DO
232500920228     C*
232600920312     C                   MOVEL     *BLANKS       ARBDER
232700920228     C     §3ATB1        IFNE      SAVTB1
232800130410     c* solo se tipo bolla nuovo non è uno storno
232900130410     c                   movel     'TB'          cod
233000130410     c                   movel(p)  §3atb1        key
233100130410     c                   clear                   dstb
233200130410     C     KTAB          CHAIN     TABEL
233300130410    3c                   if        %found(tabel00f)
233400130410     c                   movel     tbluni        dstb
233500130410    3c                   endif
233600130410    3c                   if        §tbfcb='1'
233700130326     c                   z-add     46            comfev
233800930513     C                   MOVEL     ERR(16)       ARBDER                         MODIF BOLLA
233900130410    3c                   endif
234000920228     C                   ELSE
234100920228     C*
234200920228     C     SAVFCA        IFNE      1
234300920228     C     §3AFCA        ANDEQ     1
234400130325     C***                MOVEL     ERR(18)       ARBDER                         AGGIUN VARIA
234500130325     c***                setoff                                       36
234600120423     c                   exsr      ctr_vrb
234700920228     C                   END
234800920228     C*
234900920228     C     SAVFCA        IFEQ      1
235000920228     C     §3AFCA        IFNE      1
235100941130     C*
235200941130     C* CONTROLLO SE ESISTE VARIA "G"
235300130326     C***                EXSR      ESVARG
235400130325     C***  WSIVAR        IFEQ      'S'
235500130325     C***                MOVEL     ERR(14)       ARBDER                         TOLTA VARIA
235600130325     c***                setoff                                       36
235700130325     C***                END
235800121205     c                   eval      wanncas='S'
235900121203     c                   exsr      ctr_vrb
236000941130     C*
236100920228     C                   ELSE
236200130325     C****               MOVEL     ERR(22)       ARBDER                         MODIF VARIA
236300130325     c****               setoff                                       36
236400120423     c                   exsr      ctr_vrb
236500920228     C                   END
236600920228     C                   END
236700920228     C                   END
236800920228     C*
236900920312     C     ARBDER        IFNE      *BLANKS
237000920228     C                   EXSR      WTRERR
237100920228     C                   END
237200920312     C                   END
237300920228     C                   ENDSR
237400920227     C**---------------------------------------------------------
237500920227     C** CALCOLO O ANNULLO VARIA  G
237600120322     C**---------------------------------------------------------
237700920227     C     VARIAG        BEGSR
237800920228     C                   MOVE      TASKSC        KSC4
237900990505     c                   clear                   ARBDER
238000011218
238100011218     c                   clear                   flgriorg
238200941130     C*
238300990414     c* Se data fattura a zero e cliente codificato vado a togliere la varia G
238400990505     C     TASDFT        IFEQ      *ZEROS
238500990414    1C     KSC4          ANDNE     9999
238600920303     C     KSC4          ANDNE     8888
238700920228     C*
238800990505     c* C/A tolto
238900990505     c                   if        wcas=0
239000941130     C                   SELECT
239100941130     C     TASSV1        WHENEQ    'G'
239200011218     c                   eval      flgriorg = '1'
239300991019     c     tasimv        ifeq      tasva1
239400991019     c                   clear                   tasdiv
239500991019     c                   endif
239600920227     C                   MOVEL     ' '           TASSV1
239700920227     C                   Z-ADD     0             TASVA1
239800990624     C                   MOVE      *zeros        TASimv
239900941130     C     TASSV2        WHENEQ    'G'
240000011218     c                   eval      flgriorg = '1'
240100991019     c     tasimv        ifeq      tasva2
240200991019     c                   clear                   tasdiv
240300991019     c                   endif
240400920227     C                   MOVEL     ' '           TASSV2
240500920227     C                   Z-ADD     0             TASVA2
240600990624     C                   MOVE      *zeros        TASimv
240700941130     C     TASSV3        WHENEQ    'G'
240800011218     c                   eval      flgriorg = '1'
240900991019     c     tasimv        ifeq      tasva3
241000991019     c                   clear                   tasdiv
241100991019     c                   endif
241200930212     C                   MOVEL     ' '           TASSV3
241300930212     C                   Z-ADD     0             TASVA3
241400990624     C                   MOVE      *zeros        TASimv
241500941130     C*
241600941130     C                   OTHER
241700941130     C*
241800941130     C                   MOVEL     'G'           WSV1
241900990625     C     KBLPV         DELETE    TITA7000                           30
242000990624     C  n30              MOVE      *zeros        TASimv
242100941130     C                   ENDSL
242200990419     c                   else
242300990419     c* C/A aggiunto o midificato
242400990419     c                   EXSR      ESVARG
242500990419     c* Se la varia non c'e' tolto totale imponibile cosi' la calcola
242600990419     c                   if        wsivar=' '
242700990624     c                   clear                   TASimv
242800990419     c                   else
242900990419     c* Se gia' esiste stampo errore --> no modificata varia G
243000130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
243100130326     C***                EXSR      WTRERR
243200130326     c***                eval      wcntab=' '
243300130326     c***                exsr      ctr_vrb
243400990419     c                   endif
243500990419     c                   endif
243600920228     C*
243700941130   X1C                   ELSE
243800990414     C* CLIENTE NON CODIFICATO O CON DATA FATTURA VALORIZZATA
243900991019     C* COMUNQUE SE ANNULLATO C/A ELIMINO VARIA G  E ADEGUO TOT.IMPONIBILE
244000990414     C* Se c'è importo del contrassegno e la data fattura è uguale a zero posso
244100990414     c* togliere la varia "G"
244200990505    2C     WCAS          IFEQ      0
244300990505     C     TASDFT        ANDEQ     0
244400990414     c*
244500941130    3C                   SELECT
244600941130     C     TASSV1        WHENEQ    'G'
244700011218     c                   eval      flgriorg = '1'
244800991019     c                   sub       tasva1        tasimv
244900991019     c     tasimv        ifeq      0
245000991019     c                   clear                   tasdiv
245100991019     c                   endif
245200920228     C                   MOVEL     ' '           TASSV1
245300920228     C                   Z-ADD     0             TASVA1
245400941130     C     TASSV2        WHENEQ    'G'
245500011218     c                   eval      flgriorg = '1'
245600991019     c                   sub       tasva2        tasimv
245700991019     c     tasimv        ifeq      0
245800991019     c                   clear                   tasdiv
245900991019     c                   endif
246000920228     C                   MOVEL     ' '           TASSV2
246100920228     C                   Z-ADD     0             TASVA2
246200941130     C     TASSV3        WHENEQ    'G'
246300011218     c                   eval      flgriorg = '1'
246400991019     c                   sub       tasva3        tasimv
246500991019     c     tasimv        ifeq      0
246600991019     c                   clear                   tasdiv
246700991019     c                   endif
246800930212     C                   MOVEL     ' '           TASSV3
246900930212     C                   Z-ADD     0             TASVA3
247000941130     C                   OTHER
247100941130     C                   MOVEL     'G'           WSV1
247200991019     C     KBLPV         chain     TITA7000                           30
247300991019     c  n30              sub       ta7van        tasimv
247400991019     c  n30              delete    tita7000
247500941130    3C                   ENDSL
247600920228     C*
247700941130   X2C                   ELSE
247800990414     c*
247900990414     c* Se è già stato fatturato oppure arbcas > 0
248000920228     C*
248100941130     C* VEDO SE LA VARIA "G" C'ERA
248200130326     C*****              EXSR      ESVARG
248300941130     C*
248400991019     c* se c'è  l'importo del contrassegno per non codificati
248500991019     c*                                      o data fattura maggiore di zero
248600130326    3c***  wcas          ifgt      *zeros
248700990414     c*
248800130326    4C***  WSIVAR        IFEQ      ' '
248900130326     C***                MOVEL     ERR(24)       ARBDER                         AGGIUN VARIA
249000130326     C***                EXSR      WTRERR
249100130326     c***                eval      wcntab=' '
249200130326     c***                exsr      ctr_vrb
249300130326     C***                ELSE
249400130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
249500130326     C***                EXSR      WTRERR
249600130326     c***                eval      wcntab=' '
249700130326     c***                exsr      ctr_vrb
249800130326    4C***                END
249900130326    3c****               endif
250000990505     c*
250100990414     c* se l'importo è uguale a zero e prima c'era la varia "G"
250200130326    3c***  wcas          ifeq      *zeros
250300130326     c***  wsivar        andne     ' '
250400130326     C***                MOVEL     ERR(23)       ARBDER                          no tolta varia
250500130326     C***                EXSR      WTRERR
250600130326    3C***                END
250700941130    2C                   END
250800941130    1C                   END
250900920227     C*
251000920227     C                   ENDSR
251100011218      *----------------------------------------------------------
251200011218      * Riorganizzo le varie sul titas
251300011218      *----------------------------------------------------------
251400011218     c     sr_riorg      begsr
251500011218
251600011218     c                   clear                   sksv
251700011218     c                   clear                   skva
251800011218     c                   eval      xx = 1
251900011218
252000011219      * carico la sk delle varie dal titas
252100011218     c                   if        tassv1 <> *blanks
252200011218     c                   eval      sksv(xx) = tassv1
252300011218     c                   eval      skva(xx) = tasva1
252400020107     c                   clear                   tassv1
252500020107     c                   clear                   tasva1
252600011218     c                   add       1             xx
252700011218     c                   endif
252800011218     c                   if        tassv2 <> *blanks
252900011218     c                   eval      sksv(xx) = tassv2
253000011218     c                   eval      skva(xx) = tasva2
253100020107     c                   clear                   tassv2
253200020107     c                   clear                   tasva2
253300011218     c                   add       1             xx
253400011218     c                   endif
253500011218     c                   if        tassv3 <> *blanks
253600011218     c                   eval      sksv(xx) = tassv3
253700011218     c                   eval      skva(xx) = tasva3
253800020107     c                   clear                   tassv3
253900020107     c                   clear                   tasva3
254000011218     c                   add       1             xx
254100011218     c                   endif
254200011218
254300011219      * carico la sk delle varie dal tita7
254400011219     c     kblp          setll     tita7000
254500011218     c                   do        *hival
254600011219     c     kblp          reade     tita7000
254700011219     c                   if        %eof
254800011218     c                   leave
254900011219     c                   endif
255000011218     c                   eval      skva(xx) = ta7van
255100011218     c                   eval      sksv(xx) = ta7svn
255200011218     c                   add       1             xx
255300011219     c                   delete    tita7000
255400011218     c                   enddo
255500011219
255600011219      * scrivo le varie su titas
255700011219     c                   if        sksv(1) <> *blanks
255800011219     c                   eval      tassv1 = sksv(1)
255900011219     c                   eval      tasva1 = skva(1)
256000011219     c                   endif
256100011219     c                   if        sksv(2) <> *blanks
256200011219     c                   eval      tassv2 = sksv(2)
256300011219     c                   eval      tasva2 = skva(2)
256400011219     c                   endif
256500011219     c                   if        sksv(3) <> *blanks
256600011219     c                   eval      tassv3 = sksv(3)
256700011219     c                   eval      tasva3 = skva(3)
256800011219     c                   endif
256900011219
257000011219      * scrivo le varie su tita7
257100011219     c     4             do        20            xx
257200011219     c                   if        sksv(xx) <> *blanks
257300011219     c                   eval      ta7aas = arbaas
257400011219     c                   eval      ta7lnp = arblnp
257500011219     c                   eval      ta7nrs = arbnrs
257600011219     c                   eval      ta7nsp = arbnsp
257700011219     c                   eval      ta7tbl = comtbl
257800011219     c                   eval      ta7svn = sksv(xx)
257900011219     c                   eval      ta7van = skva(xx)
258000011219     c                   write     tita7000
258100011219     c                   endif
258200011219     c                   enddo
258300011218
258400011218     c                   endsr
258500011218
258600920210     C**---------------------------------------------------------
258700920210     C** CREO RECORD CONTRASSEGNO
258800920210     C**---------------------------------------------------------
258900920210     C     CRTCSB        BEGSR
259000941130     C                   MOVEL     ARBDER        SAVDER
259100920303     C*
259200920303     C* SE ESISTE ANNULLATO LO DELETO E RISCRIVO
259300941130     C     KANN          CHAIN     TNCSB000                           41
259400941130    1C     *IN41         IFEQ      *ON
259500941130     C                   CLEAR                   TNCSB000
259600941130     C                   ELSE
259700941130     C     KANN          CHAIN     TNCSA000                           39
259800941130     C  N39              DELETE    TNCSA000
259900941130    1C                   ENDIF
260000920303     C*
260100950201     C* AGGANCIO FILE FNARBM0R
260200170109     C***  KARBM         CHAIN     FNARBMRR                           35
260300170109     C**N35              ADD       1             NUMRBM            7 0
260400170109     c** non utilizzo più il mittente da trasmissione in FNARBM
260500170109     c*  ma aggancio direttamente l'anagrafica mittente in TITAA30C
260600170109     c
260700920212     C* INESISTENTE: STAMPO MA CREO UGUALMENTE ARCHIVIO (DA COMPLETARE)
260800170109    1C** 35              DO
260900170109     C**                 MOVEL     ERR(11)       ARBDER
261000170109     C**                 Z-ADD     102           COMFEV
261100170109     C**                 EXSR      WTRERR
261200920212     C* AZZERO CAMPI MITTENTE
261300170109     C**                 MOVEL     *BLANKS       ARBRSM
261400170109     C**                 MOVEL     *BLANKS       ARBINM
261500170109     C**                 MOVEL     *BLANKS       ARBCAM
261600170109     C**                 MOVEL     *BLANKS       ARBLOM
261700170109     C**                 MOVEL     *BLANKS       ARBPRD
261800170109     C**                 MOVEL     *ZEROS        ARBCCM
261900170109    1C**                 END
262000920212     C*
262100920210     C                   MOVEL     TASLNP        CSALNP
262200920210     C                   MOVEL     TASNRS        CSANRS
262300920210     C                   MOVEL     TASAAS        CSAAAS
262400920210     C                   MOVEL     TASNSP        CSANSP
262500920210     C*
262600920210     C                   MOVEL     TBLCA         CSBTBL
262700920210     C                   Z-ADD     TASLNP        CSBLNP
262800920210     C                   Z-ADD     TASAAS        CSBAAS
262900920210     C                   Z-ADD     TASNRS        CSBNRS
263000920210     C                   Z-ADD     TASNSP        CSBNSP
263100941130     C                   Z-ADD     TASMGS        CSBMGS
263200990629     C                   Z-ADD     TASTFP        CSBFLE
263300941130     C                   MOVEL     DESPAR        CSBPAR                         PAESE ARRIVO
263400970916     C                   MOVEL     DESPRV        CSBPRO                         PROV.DEST.
263500030310     C                   MOVE      *ZERO         CSBSTA
263600950202     C                   EXSR      AGGCSB
263700920304     C*
263800941130     C                   MOVEL     TBLCA         FLGTB1
263900920304     C*
264000920304     C* SE BLANK E CODIFICATO DESTINATARIO CHIAN
264100941130    2C     TASRSD        IFEQ      *BLANKS
264200990625     c* N.B.la call al tibs69 sarà da togliere quando tutte le bolla avranno
264300990625     c* il destinatario pieno
264400990625     c     flgtb1        ifeq      'A'
264500981001     C                   CLEAR                   DSBS69
264600981001     C                   MOVEL     KCI           I69KCC
264700981001     C                   MOVEL     KNSIF         I69SIF
264800981001     C                   MOVEL     TASKSC        I69KAC
264900981001     C                   CALL      'TIBS69R'
265000981001     C                   PARM                    DSBS69
265100981001     C                   PARM                    DSACO
265200981001     C                   PARM                    DSIND
265300981001     C                   PARM                    DSCLP
265400981001     C                   PARM                    DSCLS
265500981001     C**
265600981001     C     O69ERR        IFEQ      ' '
265700981001     C                   MOVEL     ACORAG        CSBRSD
265800981001     C                   ELSE
265900981001     C                   MOVEL     *BLANKS       CSBRSD
266000981001     C                   ENDIF
266100990625     C                   ENDIF
266200981001     C                   ELSE
266300981001     C                   MOVEL     TASRSD        CSBRSD
266400941130    2C                   END
266500920304     C*
266600920304     C*
266700920210     C                   Z-ADD     TASRMN        CSBRMN
266800990625     c* prendo il riferimento mittente alfabetico da estensione (tita4)
266900990625     c                   movel     'A'           §trc
267000010126     c     ktita         chain(N)  tita430c                           39
267100990625     c  n39              movel     ta4not        csbrma
267200990625     c   39              clear                   csbrma
267300941207     C                   Z-ADD     TASLNA        CSBLNA
267400040721     C**                   MOVEL     TASFIN        CSBFIN
267500920210     C                   Z-ADD     TASKSC        CSBKSC
267600920210     C                   MOVE      CSBKSC        CSBCDI
267700931108     C   41              MOVEL     'R'           CSBFTR
267800931108     C   41              MOVE      DATEU         CSBDTR
267900920303     C*
268000941130    1C     FLGTB1        IFEQ      'A'
268100941130     C                   Z-ADD     99999         CSBKSC
268200941130     C                   MOVEL     TASLNA        CSBKSC
268300941206     C                   MOVE      CSBKSC        CSBCDI                         WB/LB
268400941206    1C                   END                                                    WB/LB
268500941130     C*
268600170109     C* VEDO SE tasccm   e'GENERICo
268700170109     C                   MOVE      tasccm        W0040             4 0
268800170111     c                   if        w0040 <>8888 and w0040 <>9999
268900941130     C                   MOVEL     TASCCM        CSBCDI
269000170109     c                   endif
269100170109     c
269200941206    1C*************        END                             WB/LB
269300941130     C*
269400920210     C** RAGGRUPPAMENTO C/ASSEGNI
269500920210     C     TASLNA        CHAIN     AZORG                              51
269600920210     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
269700920210     C   51              Z-ADD     TASLNA        CSBRGF
269800941130     C*
269900941130     C* VEDO SE CSBCDI E' INTESTAZIONE GENERICA
270000941130     C                   MOVE      CSBCDI        W0040             4 0
270100920210     C*
270200920210     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
270300941130     C     W0040         IFEQ      8888
270400941130     C     W0040         OREQ      9999
270500170109     C**                 MOVEL     ARBRSM        CSARSD                         MITTENTE
270600170109     C**                 MOVEL     ARBINM        CSAVID                            '
270700170109     C**                 MOVEL     ARBCAM        CSACAD                            '
270800170109     C**                 MOVEL     ARBLOM        CSACID                            '
270900170109     C**                 MOVEL     ARBPRM        CSAPRM
271000170109     C**                 MOVEL     ARBNZM        CSANZM
271100170109
271200170109     c                   movel     'M'           §trc
271300170109     c     ktita         chain(N)  titaa30c
271400170109     c                   if        %found(titaa30c)
271500170109     C                   MOVEL     taarsc        CSARSD                         MITTENTE
271600170109     C                   MOVEL     taaind        CSAVID                            '
271700170109     C                   MOVEL     taacap        CSACAD                            '
271800170109     C                   MOVEL     taaloc        CSACID                            '
271900170109     C                   MOVEL     taaprv        CSAPRM
272000170109     C                   MOVEL     taanaz        CSANZM
272100170109     c                   else
272200170109     c                   clear                   csarsd
272300170109     c                   clear                   csavid
272400170109     c                   clear                   csacid
272500170109     c                   clear                   csaprm
272600170109     c                   clear                   csanzm
272700170109     c                   endif
272800920212     C*
272900941201     C                   WRITE     TNCSA000                             99
273000920212     C                   END
273100920212     C**
273200941201     C   41              WRITE     TNCSB000                             99
273300941130     C  N41              UPDATE    TNCSB000
273400950914     C*
273500950914     C* CREO RECORD DI INSERIMENTO C/ASSEGNO NELLE VARIAZIONI
273600950914     C                   MOVEL     'IMCA'        CSVCAV
273700950914     C                   EXSR      CSVWTR
273800941130     C*
273900941130     C** SE ERA IMPOSTATO LO RIMETTO COME ERA PRIMA
274000941130     C                   MOVEL     SAVDER        ARBDER
274100920210     C                   ENDSR
274200920210     C**---------------------------------------------------------
274300920210     C** CONTROLLO ESISTENZA ARCHIVIO CONTRASSEGNI
274400920210     C**---------------------------------------------------------
274500920210     C     CONCSB        BEGSR
274600930513     C                   SETOFF                                           42    25
274700920212     C* VARIO  CONTRASSEGNO SOLO SE NON PAGATO-INCASSATO
274800941130     C     KANN          CHAIN     TNCSB000                           31
274900930303     C*
275000941130     C   31KANN          SETLL     FNLBL01L                               42
275100920212     C*
275200930513     C* PER CASUALE VARIAZIONE "K" SEMPRE ERRORE
275300930303     C   31FLGCVB        IFEQ      'K'
275400930513     C                   SETON                                        25
275500930303     C                   MOVEL     ERR(5)        ARBDER
275600930303     C                   Z-ADD     102           COMFEV
275700930303     C                   EXSR      WTRERR
275800930513     C*
275900930303     C                   ELSE
276000930513     C*
276100930513     C* ALTRIMENTI SOLO SE NON C'E' IL LEGAME BOLLA
276200930513     C     *IN42         IFEQ      '0'
276300930513     C                   SETON                                        25
276400930513     C                   MOVEL     ERR(5)        ARBDER
276500930513     C                   Z-ADD     102           COMFEV
276600930513     C                   EXSR      WTRERR
276700930303     C                   END
276800930513     C                   END
276900991103     c*
277000991103     c     *in31         ifeq      *off
277100991103     c                   movel     csbvca        wcsbvca
277200991103     c     csbcas        ifgt      0
277300991103     c     csbvca        andeq     *blanks
277400991103     c                   movel     'ITL'         wcsbvca
277500991103     c                   endif
277600991103     c                   endif
277700930513     C*
277800920302     C* GIA' INCASSATO O PAGATO
277900950202    1C  N31CSBDDC        IFNE      0
278000920210     C     CSBBNA        ORNE      0
278100920212     C*
278200950324     C* SE NON MODIFICATO L'IMPORTO DEL C/A OK, SE VARIAZIONE "K"
278300950324     C     FLGCVB        IFEQ      'K'
278400950202    3C     ARBCAS        IFNE      CSBCAS
278500991102     C     ARBVCA        ORNE      WCSBVCA
278600920212     C                   SETON                                        31
278700110214     C                   Z-ADD     102           COMFEV
278800950202    2C                   END
278900110214     c
279000950324     C* ALTRIMENTI SEMPRE ERRORE
279100950324     C                   ELSE
279200950324     C                   SETON                                        31
279300110214     C                   Z-ADD     102           COMFEV
279400950324    2C                   END
279500920601     C*
279600950202    2C   31FLGCVB        IFNE      'I'
279700950222    2C     FLGCVB        ANDNE     'M'
279800170110     C***                SETON                                        36
279900920601     C                   MOVEL     ERR(10)       ARBDER
280000920601     C                   EXSR      WTRERR
280100950324     C                   ELSE
280200950324     C                   CLEAR                   COMFEV
280300950202    2C                   END
280400950202    1C                   END
280500920210     C                   ENDSR
280600950202     C*
280700950202     C**---------------------------------------------------------
280800950202     C** AGGIORNO RECORD CONTRASSEGNO
280900950202     C**---------------------------------------------------------
281000950202     C     AGGCSB        BEGSR
281100950202     C                   Z-ADD     ARBCAS        CSBCAS
281200950202     C                   MOVEL     ARBVCA        CSBVCA
281300950202     C                   MOVEL     ARBGCA        CSBGCA
281400950203     C                   Z-ADD     ARBCMB        CSBCMB
281500030310     c                   if        csbsta <> 3
281600000703     c                   movel     0             csbsta
281700030310     c                   end
281800950202     C* TIPO ASSEGNO: MITTENTE / BARTOLINI
281900950202     C                   MOVEL     '1A'          COD
282000950202     C                   MOVEL(P)  ARBTIC        KEY
282100950202     C     KTAB          CHAIN     TABEL                              30
282200950202     C  N30              MOVEL     TBLUNI        DS1A
282300950202     C  N30              MOVEL     §1AFMB        CSBFUS
282400950202     C   30              CLEAR                   CSBFUS
282500950202     C                   ENDSR
282600920212     C**---------------------------------------------------------
282700920212     C** ANNULLO RECORD CONTRASSEGNO
282800920212     C**---------------------------------------------------------
282900920212     C     ANNCSB        BEGSR
283000941209     C* CREO STORICO VARIAZIONE
283100920602     C     CSBSTA        IFNE      9
283200920212     C                   MOVEL     'ANCA'        CSVCAV
283300920212     C                   EXSR      CSVWTR
283400920212     C* ANNULLO
283500920212     C                   MOVEL     9             CSBSTA
283600941130     C                   UPDATE    TNCSB000
283700920212     C                   END
283800920212     C                   ENDSR
283900920212     C**---------------------------------------------------------
284000920212     C** ESEGUE WRITE BLCSV00F                                  -
284100920212     C**---------------------------------------------------------
284200920212     C     CSVWTR        BEGSR
284300950202     C* COME DATA E ORA VARIAZIONE METTO QUELLA DELLA FILIALE
284400941130     C                   Z-ADD     ARBDTV        CSVDTV
284500941130     C                   Z-ADD     ARBORV        CSVORV
284600020604     c*
284700021112     c                   clear                   tibs36ds
284800021112     c                   movel     arbpru        i36ute
284900140407     c                   eval      %subst(i36ute:10:1)=' '
285000021112     C                   CALL      'TIBS36R'
285100021112     C                   PARM                    tibs36ds
285200020604     c* non tovato profilo su azute
285300070420   1 c     o36err        ifne      *blanks
285400020604     c* richiamo fnlv55r per sapere se lna è gestita da altro p.o. e da chi
285500020604     c                   clear                   dslv55
285600020604     c                   movel     '6'           d55tpt
285700020604     c                   z-add     taslna        d55lin
285800020604     c                   z-add     csvdtv        d55drf
285900020604     c                   call      'FNLV55R'
286000020604     c                   parm                    dslv55
286100020604     C                   MOVEL     d55tfa        CSVFEV
286200070420   x1c                   else
286300020604     c* trovato profilo su azute
286400021112     c                   movel     o36pou        csvfev
286500070420    1c                   endif
286600020604     c*
286700920212     C                   MOVEL     CSBDS         CSVDS
286800941130     C* COME DATA TRSMISSIONE QUANDO HO CARICATO
286900941130     C                   Z-ADD     DATEU         CSVDTR
287000941130     C                   WRITE     TNCSV000
287100920212     C                   ENDSR
287200920130     C**---------------------------------------------------------
287300920130     C** CONTROLLO ESISTENZA BOLLA
287400920130     C**---------------------------------------------------------
287500990628     C     CTRSPE        BEGSR
287600920302     C                   SETOFF                                       332940
287700941130     C                   SETOFF                                       4217
287800941121     C                   SETOFF                                       020304
287900930514     C                   SETON                                        23
288000080408     c                   clear                   VariataSede       1
288100110315     c                   clear                   SoloCASS          1
288200941121     C*
288300991015     C     KBLP          CHAIN     TITAS30C                           29
288400920220     C*
288500941121     C* 29 ON  - S P E D I Z I O N E   I N E S I S T E N T E
288600941121    1C     *IN29         IFEQ      *ON
288700941121     C*
288800941121     C     KBLP          CHAIN     TNBLA000                           40
288900941121     C   40KANN          CHAIN     TNBLA000                           40
289000920602     C*
289100930804     C* 40 OFF - S P E D I Z I O N E   A N N U L L A T A
289200991015     C  N40              MOVEL     ERR(28)       ARBDER
289300070119     c
289400070119     c* Verifico se lnp estera, se record bolla chiuso in partenza
289500070119     c*  come merce mai affidata
289600070119    2c                   if        *in40
289700070119     c
289800070119     c     ArbLnp        Chain     Azorg01l
289900070119    3c                   If        %Found(Azorg01l)
290000070119     c                   Eval      Og143 = OrgDe3
290100070119    4c                   If        §OgNtw = 'DPD' or §ogntw='EEX' or
290200070119     c                             §ogntw='FED'
290300070119     c* controllo bolla se presente in fnblp
290400070119     c     kann          chain     fnblp01l
290500070119    5c                   if        %found(fnblp01l) and blpft1='N'
290600070119     c                             and blpcca='7'
290700070119     c                   setoff                                       23
290800070119    5c                   endif
290900070119    4c                   endif
291000070119    3c                   endif
291100070119    2c                   endif
291200920302     C*
291300920602     C* 40 ON - INESISTENTE: VEDO SE GIA' TRASFORMATO COD BOLLA
291400070119     c   23
291500070119    2Can 40FLGCVB        IFEQ      'K'
291600941121    3C     COMTBL        IFEQ      §3ATB1
291700930108     C                   MOVEL     ERR(1)        ARBDER
291800930108     C                   ELSE
291900930108     C                   MOVEL     ERR(7)        ARBDER
292000941121    3C                   END
292100930108     C*
292200920602     C                   MOVEL     §3ATB1        SAVTB1            2
292300920602     C                   MOVEL     §3ATB2        SAVTB2            2
292400080408     c                   movel     ds3a          savds3a
292500920602     C*
292600920602     C                   MOVEL     '3A'          COD
292700070119     c                   MOVEL(P)  ARBCBN        KEY
292800920602     C     KTAB          CHAIN     TABEL                              32
292900920602     C  N32              MOVEL     TBLUNI        DS3A
293000080408     c   32              clear                   ds3a
293100080408     c                   movel     §3afca        newfca            1 0
293200920602     C*
293300941121     C* 1BOLLA MODIFICATA
293400941121    3C  N32SAVTB1        IFNE      §3ATB1
293500920602     C                   MOVEL     §3ATB1        COMTBL
293600990624     C     KBLP          SETLL     TITAS30C                               42
293700920602     C  N42              MOVEL     ERR(1)        ARBDER
293800941121    3C                   END
293900920602     C*
294000941121     C* 2 BOLLA MODIFICATA
294100920602     C* SOLO SE ESISTEVA CONTROLLO
294200941121    3C  N32SAVTB2        IFNE      §3ATB2
294300920602     C     SAVTB2        ANDNE     '  '
294400920602     C*
294500941121    4C     §3ATB2        IFEQ      '  '
294600920602     C                   MOVEL     'AP'          COMTBL
294700941121     C                   ELSE
294800920602     C                   MOVEL     §3ATB2        COMTBL
294900941121    4C                   END
295000990624     C     KBLP          SETLL     TITAS30C                               42
295100070119     C  N42              MOVEL     ERR(7)        ARBDER
295200941121    3C                   END
295300080408     C***                MOVEL     SAVTB1        §3ATB1
295400080408     C***                MOVEL     SAVTB2        §3ATB2
295500080408     c                   movel     savds3a       ds3a
295600070119     c
295700070119     c* Se codice bolla vecchio = codice bolla nuovo, anche se
295800070119     c*  inesistente è inutile ristamparlo: già fatto precedentemente
295900080408    3c  n42              if        arbcbp=arbcbn and §3afca=0
296000070119     c                   setoff                                       23
296100080408    3c                   endif
296200080408     c* Se si tratta di tipo bolla con C/ASS prima e senza dopo
296300080408     c*  o una KB che toglie solo in C/ASS,
296400080408     c*  se bolla modificata in C/S --> vado avanti per il C/ASS
296500090504    3c***                if        not *in42 and *in23  and §3atb2=*blanks
296600090504    3c                   if        *in23  and §3atb2=*blanks
296700080408    4c                   if        (newfca=0 and §3afca=1) or
296800080408     c                             (arbcbp=arbcbn and §3afca=1 and arbcas=0)
296900080408     C                   MOVEL     'F7'          COMTBL
297000080408     C     KBLP          chain     TITAS30C
297100080408    5c                   if        %found(titas30c)
297200080408     c                   setoff                                       2923
297300080408     c                   eval      VariataSede='S'
297400160714   x5c                   else
297500160714
297600160714     C                   MOVEL     'A2'          COMTBL
297700160714     C     KBLP          chain     TITAS30C
297800160714    6c                   if        %found(titas30c)
297900170822    7c***                if        tascca='2' or tascca='6'
298000160714     c                   setoff                                       2923
298100160714     c                   eval      VariataSede='S'
298200170822    7c***                endif
298300160714   x6c                   else
298400160714
298500160714     C                   MOVEL     'F1'          COMTBL
298600160714     C     KBLP          chain     TITAS30C
298700160714    7c                   if        %found(titas30c)
298800170822    8c***                if        tascca='2' or tascca='6'
298900160714     c                   setoff                                       2923
299000160714     c                   eval      VariataSede='S'
299100170822    8c***                endif
299200170424   x7c                   else
299300170424     C                   MOVEL     'AS'          COMTBL
299400170424     C     KBLP          chain     TITAS30C
299500170424    8c                   if        %found(titas30c)
299600170822    9c***                if        tascca='2' or tascca='6'
299700170424     c                   setoff                                       2923
299800170424     c                   eval      VariataSede='S'
299900170822    9c***                endif
300000170424    8c                   endif
300100170424     c
300200160714    7c                   endif
300300160714    6c                   endif
300400080408    5c                   endif
300500080408    4c                   endif
300600080408    3c                   endif
300700110315     c
300800110315     c* se ho trovato la bolla con cod bolla nuovo ma era stato annullato il C/Ass  o
300900110315     c*   non c'era e ora c'e' --> lo aggiungo o riabilito
301000110315    3c                   if        *in23  and *in42 and §3atb2=*blanks  and
301100110315    4c                              newfca=1 and §3afca=0
301200110315     c                   setoff                                       2923
301300110315     c                   eval       soloCAss='S'
301400110315     c                   endif
301500920602     C*
301600941121   X2C                   ELSE
301700920602     C*
301800941130     C* CONTROLLO SE LA SPEDIZIONE E' STORNATA
301900930514     C                   EXSR      STORNO
302000991019     c* se causale variazione 'Cx' e la spedizione e' stornata
302100991019     c* spengo in29 per caricare ugualmente la variazione
302200991021    3c     flgcvb        ifeq      'C'
302300060317    3c     flgcvb        oreq      'R'
302400991021    4c     *in23         ifeq      *off
302500991021     c                   setoff                                       29
302600991021   x4c                   else
302700991021     c* altrimenti cerco la spedizione
302800991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
302900991021     c* (cioè il franco) faccio setgt e readpe invece che chain
303000991021     c     kann          setgt     titas30c
303100991021     c     kann          readpe    titas30c                               23
303200991019     c  n23              setoff                                       29
303300991021    4c                   endif
303400991021    3c                   endif
303500080408     c
303600080408     c* per "I0" e codice bolla "Z" cerco la bolla come A2 e poi
303700080408     c*  aggiorno secondo i modi già previsti in AGARBI
303800140407    3c                   if        (comcvb='I0' or
303900140407     c                              comcvb='I7' or
304000140407     c                              comcvb='I8' or
304100140407     c                              comcvb='I9' )
304200140407    3c                              and arbcbo='Z'
304300080408     C                   MOVEL     'A2'          COMTBL
304400080408     C     KBLP          chain     TITAS30C
304500080408    4c                   if        %found(titas30c)
304600080408     c                   setoff                                       2923
304700080408    4c                   endif
304800080408    3c                   endif
304900110214     c* per "I0" e codice bolla "A" cerco la bolla come F1 e poi
305000110214     c*  aggiorno secondo i modi già previsti in AGARBI
305100140407    3c***                if        comcvb='I0' and (arbcbo='A' or arbcbo='Q')
305200140407    3c                   if        (comcvb='I0' or
305300140407     c                              comcvb='I7' or
305400140407     c                              comcvb='I8' or
305500170424     c                              comcvb='I9' or
305600170424     c                              comcvb='TP' )
305700140407     c                             and (arbcbo='A' or arbcbo='Q')
305800110214     C                   MOVEL     'F1'          COMTBL
305900110214     C     KBLP          chain     TITAS30C
306000110214    4c                   if        %found(titas30c)
306100110214     c                   setoff                                       2923
306200110214    4c                   endif
306300110214    3c                   endif
306400160714     c
306500160714     c* se si tratta di causale variazione TB non stampo: la bolla è in arrivo
306600160714     c*  solo per tipo bolla AR
306700160714     c                   if        comcvb='TB' and §3atb1='AR'
306800160714     c                   setoff                                       23
306900160714     c                   seton                                        37
307000160714     c                   endif
307100930514     C*
307200941121    3C   23COMTBL        IFEQ      §3ATB1
307300991015     C                   MOVEL     ERR(1)        ARBDER
307400920130     C                   ELSE
307500991015     C                   MOVEL     ERR(7)        ARBDER
307600941121    3C                   END
307700941121    2C                   END
307800941130     C**
307900941130     C* 23 ON --> STAMPO ERRORE
308000941130    2C     *IN23         IFEQ      *ON
308100920602     C  N42
308200920602     CAN 40              SETON                                        37
308300920212     C*        PER C.E.D.
308400920603     C  N42
308500920603     CAN 40
308600920603     CORN40              Z-ADD     102           COMFEV
308700920602     C  N42
308800920602     CAN 40
308900920602     CORN40              EXSR      WTRERR
309000941121    2C                   END
309100941121     C*
309200991019    1C                   Endif
309300941121     C*
309400991019    1c     *in29         ifeq      *off
309500941121     C* 29 OFF - S P E D I Z I O N E       E S I S T E N T E
309600121108     c* Salvo contenuto del tracciato record di titas in ds apposita:
309700121108     c* serve per la scrittura dell'immagine di titas prima della prima variazione
309800121108     c* nel file TIVRB00F
309900121108     c                   exsr      memo_dstitas
310000941121     C*
310100941121     C* 02 OFF             --> GIA' CONTABILIZZATA IN BLTAS10 O BLTASP
310200941130     C* 02 ON  MA TASFIC=1 --> GIA' CONTABILIZZATA IN BLTAS00
310300930512     C*
310400941130    2C     *IN02         IFEQ      *OFF
310500941121     C     *IN02         OREQ      *ON
310600941130     C     TASFIC        ANDNE     ' '
310700941130     C**
310800950202     C* VARIAZIONE "Ix" --> indirizzo/ peso/ volume
310900941130     C**
311000941130    3C     FLGCVB        IFEQ      'I'
311100941130     C*
311200950202     C* SE VARIO PESO/VOLUME      E NON E' LA 2 BOLLA --> ERRORE
311300960416    4C     COMCVB        IFNE      'I0'
311400140407    4C     COMCVB        andne     'I7'
311500140407    4C     COMCVB        andne     'I8'
311600140407    4C     COMCVB        andne     'I9'
311700941130     C*
311800991021    5C     §3ATB2        IFNE      tastbl
311900930513     C                   SETON                                        29
312000930804     C*
312100140219    6c***  TASPKB        IFNE      ARBPKF
312200130325     C***                MOVEL     ERR(9)        ARBDER
312300130325     C***                EXSR      WTRERR
312400120313     c                   exsr      ctr_vrb
312500140219   X6C***                ELSE
312600930603     C*
312700140219    7C***  TASFVF        IFNE      'T'
312800140219     C***  TASVLF        ANDNE     ARBVLF
312900140219    8C***  TASFVF        IFEQ      'Z'
313000140219     C***  TASVLF        ANDLT     ARBVLF
313100140219     C***  TASFVF        ORNE      'Z'
313200130325     C***                MOVEL     ERR(9)        ARBDER
313300130325     C***                EXSR      WTRERR
313400140219     c***                eval      wcntab='S'
313500140219     c***                exsr      ctr_vrb
313600140219    8C***                END
313700140219    7C***                END
313800930603     C*
313900140219    6C***                END
314000930603     C*
314100941130    5C                   ENDIF
314200941130     C*
314300930512   X4C                   ELSE
314400930512     C*
314500950308RM*  C*§SE VARIATO FERMO DEPOSITO E NON E' LA 2 BOLLA --> ERRORE
314600950308RM* 5C*§         TASFFD    IFNE ARBFFD
314700950308RM*  C*§         §3ATB2    ANDNECOMTBL
314800950308RM*  C*§                   MOVELERR,9     ARBDER
314900950308RM*  C*§                   EXSR WTRERR
315000950308RM* 5C*§                   END
315100990625     C**
315200990625     C* MEMORIZZO ULTIME 4 CIFRE CODICE FATTURAZIONE 2 BOLLA
315300991021    4C     §3ATB2        IFEQ      tastbl
315400990625     C                   MOVE      TASKSC        KSC42BO           4 0
315500930512    4C                   END
315600990625     C*
315700990625    4C                   END
315800930512    3C                   END
315900950202     C**
316000950202     C* VARIAZIONE "Cx" --> consegne particolari
316100950202     C**
316200950202    3C     FLGCVB        IFEQ      'C'
316300950202     C* no errore se 2 bolla
316400991021    4C     §3ATB2        IFNE      tastbl
316500991019     c
316600991019     C                   MOVEL     'TB'          COD
316700991021     C                   MOVEL(P)  tastbl        KEY
316800991019     C     KTAB          CHAIN     TABEL                              32
316900991019     C  N32              MOVEL     TBLUNI        DSTB
317000991019     C   32              MOVEL     *ALL'1'       DSTB
317100991019   4aC     §TBFCB        IFEQ      '1'
317200950202     C*
317300950202     C* STAMPO SOLO SE UNO DEI TRE VARIATO
317400950202    5C     TASFTC        IFEQ      ARBTC1
317500950202     C     TASTC2        ANDEQ     ARBTC2
317600950202     C     TASFTC        OREQ      ARBTC2
317700950202     C     TASTC2        ANDEQ     ARBTC1
317800950202     C*
317900950202   X5C                   ELSE
318000950202     C* CONTROLLO SE LA TARIFFA TASSA LE CONSEGNE PARTICOLARI
318100140219     C***                EXSR      CONPAR
318200950202     C*
318300140219    6C**   *IN17         IFEQ      *ON
318400130325     C****               MOVEL     ERR(9)        ARBDER
318500130325     C****               EXSR      WTRERR
318600120320     c                   exsr      ctr_vrb
318700140219    6C**                ENDIF
318800950202    5C                   ENDIF
318900950202     C*
319000991019   4ac                   ENDIF
319100950202    4C                   ENDIF
319200950202    3C                   ENDIF
319300941130     C**
319400941130     C* VARIAZIONE "Tx" --> tassazione 1 bolla
319500941130     C**
319600941130    3C     FLGCVB        IFEQ      'T'
319700950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
319800130326     C***  ARBFIV        IFGT      0
319900130326     C***  TASFIV        ANDNE     ARBFIV
320000950324     C                   Z-ADD     46            COMFEV
320100130326     C***                ENDIF
320200950324     C*
320300930513     C                   MOVEL     ERR(9)        ARBDER
320400990505     c                   eval      wdelarbu='S'
320500920220     C                   EXSR      WTRERR
320600920226     C                   SETON                                        29
320700011029     c                   if        tasias > 0  and tasvas = *blanks
320800011029     c                   movel     'ITL'         tasvas
320900011029     c                   endif
321000990419     c* Controllo se variato l'importo da assicurare se contabilizzata     da assicurare
321100990419     c                   if        *in13 and (arbias <> tasias or
321200990419     c                             arbvas <> tasvas)
321300990419     c*
321400990419     c                   exsr      AGGIAS
321500990419     c*
321600990419     c                   endif
321700941130    3C                   END
321800941130     C**
321900941130     C* VARIAZIONE "2x" --> tassazione 2 bolla
322000941130     C**
322100941130    3C     FLGCVB        IFEQ      '2'
322200991021     C     tastbl        ANDEQ     §3ATB2
322300950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
322400130326     C**   ARBFIV        IFGT      0
322500130326     C**   TASFIV        ANDNE     ARBFIV
322600950324     C                   Z-ADD     46            COMFEV
322700130326     C**                 ENDIF
322800950324     C*
322900950203     C                   MOVEL     §3ATB2        TBLERR
323000930513     C                   MOVEL     ERR(9)        ARBDER
323100920226     C                   EXSR      WTRERR
323200920226     C                   SETON                                        29
323300941130    3C                   END
323400950202     C**
323500950202     C* VARIAZIONE "Mx" --> mittente
323600990628     c*                     la variazione del mittente dipende dalla conab
323700990628     c*                     della prima bolla, a prescindere da come è messa
323800990628     c*                     la seconda: se aggiono la  aggiorno anche la 2
323900950202     C**
324000950202    3C     FLGCVB        IFEQ      'M'
324100991021    5C     §3ATB2        ANDNE     tastbl
324200130326     C                   Z-ADD     46            COMFEV
324300950202     C                   MOVEL     ERR(9)        ARBDER
324400950202     C                   EXSR      WTRERR
324500950202     C                   SETON                                        29
324600950202    3C                   END
324700941130    2C                   END
324800930512    1C                   END
324900941130     C*
325000920226     C                   ENDSR
325100920228     C**---------------------------------------------------------
325200920228     C** CHAIN SU PRIMA BOLLA
325300920228     C**---------------------------------------------------------
325400920228     C     CHTB1         BEGSR
325500920228     C                   MOVEL     SAVTB1        COMTBL
325600941130     C                   SETOFF                                       020304
325700990625     C     KBLP          CHAIN     TITAS30C                           39
325800121108     c                   exsr      memo_dstitas
325900920228     C*
326000990414     C                   EXSR      CTR3
326100920228     C                   ENDSR
326200920228     C**---------------------------------------------------------
326300920228     C** CHAIN SU SECONDA BOLLA
326400920228     C**---------------------------------------------------------
326500920228     C     CHTB2         BEGSR
326600920228     C                   MOVEL     SAVTB2        COMTBL
326700941130     C                   SETOFF                                       020304
326800990625     C     KBLP          CHAIN     TITAS30C                           39
326900121108     c                   exsr      memo_dstitas
327000920228     C*
327100990414     C                   EXSR      CTR3
327200920228     C                   ENDSR
327300920226     C**---------------------------------------------------------
327400920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
327500920226     C**---------------------------------------------------------
327600920226     C     CTR2          BEGSR
327700920302     C                   MOVEL     COMTBL        FLGTB1            1
327800950202     C* SPEDIZIONE 1) GIA' IMPONIBILE     SE NO FATTURA E NO ASSEGNATO
327900950202     C*               SE VARIAZOINE DA ARRIVO
328000950202     C*            2) GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
328100950202     C*            --> NON POSSO VARIARE
328200950202    1C     TASDFT        IFGT      0
328300950202    2C     TASFIC        IFNE      ' '
328400920226     C                   SETON                                        29
328500130326     c                   z-add     46            comfev
328600991015     C                   MOVEL     ERR(9)        ARBDER
328700920226     C                   EXSR      WTRERR
328800950202    2C                   END
328900920226     C*
329000950202   X1C                   ELSE
329100920226     C*
329200990624    2C     TASimv        IFNE      *zeros
329300950202     C     COMCVB        ANDNE     'TP'
329400950202    3C     FLGTB1        ANDNE     'A'
329500920302     C                   SETON                                        29
329600920226     C*
329700991015     C
329800991015     C   29              MOVEL     ERR(8)        ARBDER
329900130326     c   29              z-add     46            comfev
330000920226     C   29              EXSR      WTRERR
330100950202    2C                   END
330200920302     C*
330300950202    1C                   END
330400920226     C                   ENDSR
330500920226     C**---------------------------------------------------------
330600920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
330700941130     C** PER LE VAIRAZIONI C/ASSEGNO O TIPI BOLLA
330800920226     C**---------------------------------------------------------
330900920226     C     CTR3          BEGSR
331000920303     C                   SETOFF                                       2938
331100920226     C* SPEDIZIONE GIA' IMPONIBILE     SE NO FATTURA
331200920226     C*            GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
331300920226     C*            NON POSSO VARIARE
331400920226     C*
331500170110     C**N02              SETON                                        2936
331600170110     C  N02              SETON                                        29
331700941202     C   02TASDFT        IFGT      0
331800990414     C     TASFIC        ANDNE     ' '
331900170110     C***                SETON                                        2936
332000170110     C                   SETON                                        29
332100920226     C*
332200920226     C                   ELSE
332300920226     C*
332400990624     C     TASimv        IFNE      *zeros
332500920226     C                   SETON                                        38
332600920226     C                   END
332700920226     C*
332800920226     C                   END
332900920226     C                   ENDSR
333000920211     C**---------------------------------------------------------
333100920211     C** SCRIVO FILE ERRORI
333200920211     C**---------------------------------------------------------
333300920211     C     WTRERR        BEGSR
333400110208     c                   clear                   arbdat
333500920211     C*
333600970916     C* CREO IL RECORD DI ERRORE SOLO SE LA DATA VARIAZIONE E' INFERIOR
333700970916     C* DI 3 GIORNI A OGGI PER LE STAMPA DI SEDE
333800000703    0C     COMFEV        IFEQ      102
333900110208     C     ARBDTV        ANDLE     DATAM3
334000000703     c     *in37         andeq     *on
334100100225
334200110208     c* Se si tratta di codice TP con importo da assicurare, stampo subito
334300100225    0C     COMFEV        oreq      102
334400100225     c     *in13         andeq     *on
334500100225     c     arbias        andgt     0
334600970916     C*é*
334700970916     C     COMFEV        ORNE      102
334800000703     C*é*
334900000703     C     *in37         ORNE      *on
335000970916     C**
335100991015     C     KERR          SETLL     FIARBE2C                               01
335200930702     C* SE NON TROVATO ERRORE
335300930706    1C     *IN01         IFEQ      *OFF
335400110208     c
335500110208     c* Se l'errore è bolla inesistente, aspetto 3 gg prima di ricrearlo
335600110208    2c                   if        arbder=err(1)
335700110208     c                   eval      arbdat=dateu
335800110208     C     KERR          SETLL     FIARBE2C                               01
335900110208    3c                   if        not  *in01
336000110208     c                   eval      arbdat=datam1
336100110208     C     KERR          SETLL     FIARBE2C                               01
336200110208    4c                   if        not  *in01
336300110208     c                   eval      arbdat=datam2
336400110208     C     KERR          SETLL     FIARBE2C                               01
336500110208    4c                   endif
336600110208    3c                   endif
336700110208    2c                   endif
336800110208    1c                   endif
336900110208     c
337000110208    1C     *IN01         IFEQ      *OFF
337100110209     c                   clear                   arbdat
337200930702     C* RISCRIVO ERRORE
337300950203     C* SE NON L'HO GIA' IMPOSTATO CERCO FILIALE A CUI INVIARE STAMPA
337400930706    2C     COMFEV        IFEQ      0
337500920212     C*
337600950203    3C     TBLERR        IFEQ      'F'
337700991015     C                   MOVEL     ARBLNP        ARBFEV
337800930706   X3C                   ELSE
337900991015     C                   MOVEL     TASLNA        ARBFEV
338000930706    3C                   END
338100971215     C* PRENDO TERMINAL DI PARTENZA
338200971215     C                   CLEAR                   DSLV55
338300971215     C                   MOVEL     'P'           D55TPT
338400991015     C                   MOVE      ARBFEV        D55LIN
338500971215     C                   Z-ADD     DATEU         D55DRF
338600971215     C                   CALL      'FNLV55R'
338700971215     C                   PARM                    DSLV55
338800971215     C     D55ERR        IFEQ      *BLANKS
338900991015     C                   MOVE      D55TFP        ARBFEV
339000971215     C                   END
339100920212     C*
339200930706   X2C                   ELSE
339300920212     C*
339400991015     C                   Z-ADD     COMFEV        ARBFEV
339500930706    2C                   END
339600991015     C     ARBFEV        IFEQ      102
339700070420     C*********          MOVE      MBR           ARBLNA
339800950202     C                   ELSE
339900950202     C                   MOVEL     TASLNA        ARBLNA
340000950202     C                   ENDIF
340100920211     C*
340200950201     C   11              WRITE     FNARBDEE
340300920212     C*
340400950201     C   12              WRITE     FNARBKEE
340500950201     C* SCRIVO ANCHE FNARBM0E SOLO SE RICHIESTO
340600170109    2C** 12*IN36         IFEQ      '1'
340700170109     C**   KARBM         CHAIN     FNARBMRR                           32
340800170109     C**N32              WRITE     FNARBMEE
340900170109     C**                 SETOFF                                       36
341000170109    2C**                 END
341100920212     C*
341200950203     C     *IN13         IFEQ      *ON
341300991015     C                   WRITE     FIARBTEE
341400950203     C*
341500991015     c* se devo cancellare fiarbu, lo faccio
341600991018     c     arbtrc        ifeq      '1'
341700170110     C     KARBU         SETLL     FIARBU2l
341800170110     C     KARBU         READE     FIARBU2l                               30
341900950203     C*
342000950203     C     *IN30         DOWEQ     *OFF
342100991015     C                   WRITE     FIARBUEE                             30
342200950203     C* SE POSSO LI DELETO
342300990505     c  n37              if        wdelarbu='S'
342400990505     C                   ADD       1             NUMRBU
342500170109     C                   DELETE    FIARBUtt
342600990505     c                   endif
342700950203     C*
342800170110     C     KARBU         READE     FIARBU2l                               30
342900950203     C                   ENDDO
343000991018     c                   endif
343100950203     C                   ENDIF
343200950203     C*
343300950202     C   15              WRITE     FNARBGEE
343400950202     C   16              WRITE     FNARBMEE
343500991214   X1c                   else
343600991214     c* se devo cancellare fiarbu, lo faccio
343700991214    2c     *in37         ifeq      *off
343800991214     c     wdelarbu      andeq     'S'
343900991214     c     arbtrc        andeq     '1'
344000170110     C     KARBU         SETLL     FIARBU2l
344100170110     C     KARBU         READE     FIARBU2l                               30
344200991214     C*
344300991214    3C     *IN30         DOWEQ     *OFF
344400991214     C                   ADD       1             NUMRBU
344500170109     C                   DELETE    FIARBUtt
344600170110     C     KARBU         READE     FIARBU2l                               30
344700991214    3C                   ENDDO
344800991214    2c                   endif
344900930706    1C                   ENDIF
345000970916    0C                   ENDIF
345100920212     C*
345200920212     C                   Z-ADD     0             COMFEV            3 0
345300950203     C                   MOVEL     §3ATB1        TBLERR
345400920211     C                   ENDSR
345500930514     C**---------------------------------------------------------
345600930514     C** CONTROLLO SE LA SPEDIZIONE INESISTENTE E' STORNATA
345700930514     C**---------------------------------------------------------
345800930514     C     STORNO        BEGSR
345900930514     C*
346000930514     C* SE E' SINGOLA BOLLA
346100930514     C     §3ATB2        IFEQ      '  '
346200930514     C*
346300991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
346400991021     c* (cioè il franco) faccio setgt e readpe invece che chain
346500991021     C     KANN          setgt     TITAS30C
346600991021     C     KANN          readpe    TITAS30C                               23
346700931119     C*
346800941130     C     *IN23         IFEQ      *OFF
346900930514     C                   MOVEL     'TB'          COD
347000941130     C                   MOVEL(P)  TASTBL        KEY
347100930514     C     KTAB          CHAIN     TABEL                              32
347200930514     C  N32              MOVEL     TBLUNI        DSTB
347300930514     C   32              MOVEL     *ALL'1'       DSTB
347400930514     C*
347500930514     C* SE NON E' STORNO STAMPO ERRORE
347600930514     C     §TBFCB        IFEQ      '1'
347700930514     C                   SETON                                        23
347800930514     C                   ELSE
347900930514     C                   MOVEL     '1'           ST1               1
348000930514     C                   END
348100930514     C                   END
348200930514     C*
348300930514     C                   END
348400930514     C                   ENDSR
348500930804     C*
348600950202     C**---------------------------------------------------------
348700950202     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE
348800950202     C**---------------------------------------------------------
348900930804     C     CONPAR        BEGSR
349000930804     C                   SETOFF                                       17
349100930804     C*
349200930804     C                   MOVE      TASKSC        KSC4              4 0
349300930804     C*
349400930804     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
349500930804    2C     KSC4          IFEQ      9999
349600930804     C     KSC4          OREQ      8888
349700930804     C                   SETON                                        17
349800930804     C*
349900930804   X2C                   ELSE
350000930804     C*
350100930804     C                   Z-ADD     999           TARPRG
350200930804     C                   Z-ADD     999           PREPRG
350300991021     C                   MOVEL     tastbl        UNOTB1
350400930804     C*
350500930805     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
350600930805     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
350700930804    3C     UNOTB1        IFEQ      'F'
350800930805     C                   MOVEL     'A'           TFP               1
350900930804     C                   ELSE
351000930805     C                   MOVEL     'F'           TFP
351100930804    3C                   END
351200930804     C*
351300930804     C* SE C'E' A ZERO NON DO' L'ERRORE
351400941130     C     KTAM          SETLL     TNTAM000
351500941130     C     KTAM          READE     TNTAM000                               35
351600930804     C*
351700930804    3C     *IN35         DOWEQ     *OFF
351800930804     C*
351900930804     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
352000930804    4C     TAMATB        IFNE      'A'
352100930804     C*
352200941205    5C     TAMBAP        IFNE      'B'
352300941205     C     TAMBAP        ANDNE     TFP
352400930804     C*
352500930804     C                   MOVE      TAMPRG        TARPRG            3 0
352600930804     C*
352700930804     C* VEDO SE E' TARIFFA VALIDA
352800121116     c                   movel     tasaas        tasdsp
352900121116     c                   move      tasmgs        tasdsp
353000120430    6C     TASDSP        IFGE      TAMDDT
353100120430     C     TASDSP        ANDLE     TAMDST
353200120430     C                   SETON                                        35
353300120430    6C                   END
353400930804     C*
353500930804   X5C                   ELSE
353600930804     C                   MOVE      TAMPRG        PREPRG            3 0
353700930804    5C                   END
353800930804    4C                   END
353900930804     C*
354000941130     C  N35KTAM          READE     TNTAM000                               35
354100930804    3C                   END
354200930804     C*
354300930804     C* NON TROVATA TARIFFA: STAMPO ERRORE
354400930804    3C     TARPRG        IFEQ      999
354500930804     C     PREPRG        ANDEQ     999
354600930804     C                   SETON                                        17
354700930804   X3C                   ELSE
354800930804     C*
354900930804    4C     TARPRG        IFNE      999
355000930804     C                   MOVEL     TARPRG        PRG
355100930804   X4C                   ELSE
355200930804     C                   MOVEL     PREPRG        PRG
355300930804    4C                   ENDIF
355400930804     C*
355500930804     C* VEDO SE TARIFFA PARTICOLARE A 0
355600950202     C     ARBTC1        IFNE      ' '
355700990719     C                   MOVEL(P)  ARBTC1        WFTC
355800950202     C                   EXSR      PARTIC
355900950202     C                   ENDIF
356000950202     C  N17ARBTC2        IFNE      ' '
356100990719     C                   MOVEL(P)  ARBTC2        WFTC
356200950202     C                   EXSR      PARTIC
356300950202     C                   ENDIF
356400950202     C*
356500930804    3C                   END
356600930804    2C                   END
356700930804     C*
356800930804     C                   ENDSR
356900950202     C**---------------------------------------------------------
357000950202     C* CONTROLLO IN TPT
357100950202     C**---------------------------------------------------------
357200950202     C     PARTIC        BEGSR
357300950202     C*
357400990719     C     KTAM2         CHAIN     TITPT000                           17
357500950202     C* 17 ON - NON ESISTE TARIFFA
357600950202    4C     *IN17         IFEQ      *OFF
357700950202     C*         NON ESISTE PERCHE' ANNULLATA
357800950202    5C     TPTATB        IFEQ      'A'
357900950202     C                   SETON                                        17
358000950202   X5C                   ELSE
358100950202     C* VEDO SE ESISTE DETTAGLIO TARIFFA PARTICOLARE
358200990719     C     KTAM2         SETLL     TITPD000
358300990719     C     KTAM2         READE     TITPD000                               35
358400950202     C*
358500950202    6C     *IN35         DOWEQ     *OFF
358600950202     C* NO ANNULLATO
358700950202    7C     TPDATB        IFEQ      ' '
358800950202     C* SE > 0 ERRORE
358900950202    8C     TPDITR        IFNE      0
359000950202     C     TPDMIN        ORNE      0
359100950202     C     TPDMAX        ORNE      0
359200950202     C                   SETON                                        1735
359300950202    8C                   END
359400950202    7C                   END
359500950202     C*
359600990719     C  N35KTAM2         READE     TITPD000                               35
359700950202    6C                   END
359800950202    5C                   END
359900950202    4C                   END
360000950202     C                   ENDSR
360100941130     C**---------------------------------------------------------
360200941130     C* CONTROLLO SE ESISTE VARIA "G"
360300941130     C**---------------------------------------------------------
360400941130     C     ESVARG        BEGSR
360500941130     C                   CLEAR                   WSIVAR
360600941130     C*
360700941130     C                   SETON                                        30
360800941130    3C     TASSV1        IFNE      'G'
360900941130     C     TASSV2        ANDNE     'G'
361000941130     C     TASSV3        ANDNE     'G'
361100941130     C                   MOVEL     'G'           WSV1
361200990625     C     KBLPV         SETLL     TITA730C                               30
361300941130    3C                   ENDIF
361400941130     C*
361500941130     C     *IN30         IFEQ      *ON
361600941130     C                   MOVEL     'S'           WSIVAR            1
361700941130     C                   ENDIF
361800941130     C                   ENDSR
361900941202     C**---------------------------------------------------------
362000941202     C* CONTROLLO SE HO MODIFICATO IL DESTINATARIO -------------------*
362100941202     C**---------------------------------------------------------
362200941202     C     CTRDES        BEGSR
362300941202     C*
362400950202     C                   MOVEL     CSBRSD        ALF35            35
362500941202     C* CONTROLLO LA 1 POSIZIONE <> ' '
362600950202     C     ' '           CHECKR    ALF35         C                 3 0    30
362700950202     C                   MOVE      ALF35         W001A             1
362800941202     C*
362900941202     C* CONFRONTO CON IL CAMPO CHE HO RICEVUTO
363000941202     C     *IN30         IFEQ      *ON
363100941215     C     W001A         ANDEQ     ' '
363200950202     C                   MOVEA     ARBRSD        K35
363300941202     C                   ADD       1             C
363400950202     C                   MOVEA     *BLANKS       K35(C)
363500950202     C                   MOVEA     K35           W035A            35
363600941202     C                   ELSE
363700950202     C                   MOVEL     ARBRSD        W035A
363800941202     C                   ENDIF
363900941202     C*
364000950202     C     W035A         IFEQ      ALF35
364100941202     C                   SETOFF                                       31
364200941202     C                   END
364300941202     C*
364400941202     C                   ENDSR
364500990415     C**------------------------------------------------------------------------------
364600990415     C** VARIAZIONE IMPORTO D'ASSICURARE PER SPEDIZIONE GIA' TASSATA / CONTABILIZZATA
364700990415     C**------------------------------------------------------------------------------
364800990415     c*
364900990419     c     AGGIAS        begsr
365000990415     c*
365100990415     c* controllo se la spedizione è già stata fatturata / contabilizzata oppure
365200990415     c* se è stato calcolato l'imponibile
365300990415     c*
365400990415     c                   exsr      ctr3
365500990415     c* se è acceso il  38 vuol dire che non è stata contabilizzata ma è stato calcolato
365600991018     c* fino all'imponibile e quindi devo andare a controllare se è già stata
365700990415     c* calcolata la varia "R"
365800990519     c                   select
365900990519    1c                   when      *in38
366000990421     c*
366100990421     c                   z-add     tasias        arbiap
366200990421     c                   movel     tasvas        arbvap
366300990421     c                   movel     'R'           arbvar
366400991019     c*
366500991019     c* verifico se devo registrare messagio per ufficio assicurazione
366600991019     C* relativo a spedizione avente tipo bolla non assicurabile per la
366700991019     c* quale è stato tolto, modificato o aggiunto l'importo da assicurare
366800991019     c* Escludo le bolle di recupero
366900991019     c     tblrbl        ifne      'R'
367000991019     c                   exsr      msgass
367100991019     c                   endif
367200131129
367300131129     c                   clear                   arbiap
367400131129     c                   clear                   arbvap
367500131129     c                   clear                   arbvar
367600990415     c*
367700990824     c* Non faccio niente se spedizione:
367800990824     c* - contabilizzata
367900990824     c* - tasias> 0
368000990824     c* - tasfcm="F" (tasias immesso dalla sede (dalla fatturazione))
368100990824     c* - arbias= 0
368200990824     c                   when      *in29 and tasias > 0 and tasfcm = 'F'
368300990824     c                                   and arbias = 0
368400990824     c*
368500990519     c* se è acceso il 29  vuol dire che è stata CONTABILIZZATA
368600990519    1c                   when      *in29
368700990505     c                   eval      wdelarbu='S'
368800990415     c
368900990415     c* verifico l'esistenza della varia "R"
369000990415     c                   exsr      esvarr
369100990419     c*
369200990419     c                   z-add     tasias        arbiap
369300990419     c                   movel     tasvas        arbvap
369400990419     c                   movel     'R'           arbvar
369500990415     c*
369600990519    2c                   SELECT
369700990415     c*
369800990415     c* importo in TAS = SI
369900990415     c* importo in ARB = NO
370000990415     c* varia   "R"    = SI
370100990415     c*                            errore no tolta varia "R"
370200991019     c                   when      tasias > 0 and arbias = 0
370300130325     c***                if        Wvariar = 'S'
370400130325     c***                movel     err(34)       arbder
370500130325     c***                exsr      WTRERR
370600130325     c***                endif
370700121203     c                   exsr      ctr_vrb
370800990415     c* messaggio all'Ufficio Assicurazione
370900990519     c* escludo le bolle di recupero
371000990519     c                   if        tblrbl<>'R'
371100990415     c                   z-add     603           comfev
371200990415     c                   movel     err(32)       arbder
371300990419     c                   exsr      wtrerr
371400990519     c                   endif
371500990415     c*
371600990415     c* importo in TAS = NO
371700990415     c* importo in ARB = SI
371800990415     c* varia   "R"    = NO
371900990415     c*                            errore no aggiunta varia "R"
372000991019     c                   when      tasias = 0 and arbias > 0
372100130325     c                   if        wvariar = ' '
372200130325     c***                movel     err(35)       arbder
372300130325     c***                exsr      wtrerr
372400120423     c                   exsr      ctr_vrb
372500991019     c                   endif
372600990415     c* messaggio all'Ufficio Assicurazione
372700990519     c                   if        tblrbl<>'R'
372800990415     c                   z-add     603           comfev
372900990415     c                   movel     err(31)       arbder
373000990419     c                   exsr      wtrerr
373100990519     c                   endif
373200990415     c*
373300990415     c* importo in TAS = SI
373400990415     c* importo in ARB = SI  ma diversa da tas
373500990415     c* varia   "R"    = SI
373600990415     c*                            errore no modificata varia "R"
373700991019     c                   when      tasias <> arbias
373800991019     c                   if        wvariar = 'S'
373900130325     c***                movel     err(36)       arbder
374000130325     c***                exsr      wtrerr
374100120423     c                   exsr      ctr_vrb
374200991019     c                   endif
374300990415     c* messaggio all'Ufficio Assicurazione
374400990519     c                   if        tblrbl<>'R'
374500990415     c                   z-add     603           comfev
374600990415     c                   movel     err(33)       arbder
374700990419     c                   exsr      wtrerr
374800990519     c                   endif
374900990415     c*
375000990519    2c                   endsl
375100990415     c*
375200990624     c* aggiorno in TITAS il valore dell'importo d'assicurare
375300990415     c                   z-add     arbias        tasias
375400990415     c                   movel     arbvas        tasvas
375500990824     c                   clear                   tasfcm
375600990415     c*
375700990624     c   02              update    titas000
375800990624     c   03              update    titas010
375900990624     c   04              update    titasp00
376000990419     c*
376100990419     c                   clear                   arbiap
376200990419     c                   clear                   arbvap
376300990419     c                   clear                   arbvar
376400990419     c**
376500990415     c* riaccendo il 29 e spengo il 36
376600990415     c                   seton                                        29
376700170110     c*****              setoff                                       36
376800991019     c* Se e' bolla normale, ma non compare nel tabulato per la
376900991019     c*  Consuldanni, stampo solo per l'ufficio assicurazione
377000990519     c                   other
377100990519     c                   if        tblrbl<>'R'
377200000410     c                   z-add     tasias        arbiap
377300000410     c                   movel     tasvas        arbvap
377400000410     c                   movel     'R'           arbvar
377500991019     c                   exsr      msgass
377600000410     c                   clear                   arbiap
377700000410     c                   clear                   arbvap
377800000410     c                   clear                   arbvar
377900990519     c                   endif
378000990519    1c                   ENDSL
378100990415     c*
378200990415     c                   endsr
378300990415     c*
378400991019     C**-------------------------------------------------------------
378500991019     C** messaggi per ufficio Assicurazione per tipo bolla non assic.
378600991019     C**-------------------------------------------------------------
378700991019     C     msgass        BEGSR
378800991019     C                   MOVEL     'TB'          COD
378900991019     c                   movel(P)  tastbl        key
379000991019     C     KTAB          CHAIN     TABEL                              32
379100991019     C  N32              MOVEL     TBLUNI        DSTB
379200991019     C   32              clear                   DSTB
379300991019     c* se bolla non assicurabile e aggiunto, tolto o modificato importo
379400991019     c* da assicurare devo stampare messaggio per ufficio Assicurazione
379500991019    1c     §tbfas        ifeq      '0'
379600991019    2c                   select
379700991019     c* tolto importo da assicurare
379800991019   w1c                   when      tasias > 0 and arbias = 0
379900991019     c                   z-add     603           comfev
380000991019     c                   movel     err(41)       arbder
380100991019     c                   exsr      wtrerr
380200991019     c* aggiunto importo da assicurare
380300991019   w2c                   when      tasias = 0 and arbias > 0
380400991019     c                   z-add     603           comfev
380500991019     c                   movel     err(40)       arbder
380600991019     c                   exsr      wtrerr
380700991019     c* modificato importo da assicurare
380800991019   w3c                   when      tasias <> 0
380900991019     c                   z-add     603           comfev
381000991019     c                   movel     err(42)       arbder
381100991019     c                   exsr      wtrerr
381200991019    2c                   endsl
381300991019    1c                   endif
381400991019     c                   endsr
381500990415     C**---------------------------------------------------------
381600990415     C* CONTROLLO SE ESISTE VARIA "R"
381700990415     C**---------------------------------------------------------
381800990415     C     ESVARR        BEGSR
381900990415     C                   CLEAR                   WVARIAR           1
382000990415     C*
382100990415     C                   SETON                                        30
382200990415    3C     TASSV1        IFNE      'R'
382300990415     C     TASSV2        ANDNE     'R'
382400990415     C     TASSV3        ANDNE     'R'
382500990415     C                   MOVEL     'R'           WSV1
382600170110     C     KBLPV         setll     TITA730C                               30
382700990415    3C                   ENDIF
382800990415     C*
382900990415     C     *IN30         IFEQ      *ON
383000990415     C                   MOVEL     'S'           WVARIAR
383100990415     C                   ENDIF
383200990415     C                   ENDSR
383300941130     C**---------------------------------------------------------
383400941130     C* ROUTINE INIZIALE
383500941130     C**---------------------------------------------------------
383600941130     C     *INZSR        BEGSR
383700021014
383800021014
383900941130     C                   Z-ADD     1             CODUT             1 0
384000021014
384100021014     c     *dtaara       define    §azute        azuteds
384200021014     c     *dtaara       define    §datiute      ddatiute
384300021014     C                   in(E)     *dtaara
384400021014     C                   IF        %Error  or  RSUT = *blanks
384500021014     C                   CLEAR                   TIBS34DS
384600021014     C                   call      'TIBS34R'
384700021014     C                   parm                    Tibs34Ds
384800021014     C                   in        *dtaara
384900021014     c                   ENDIF
385000021014
385100021014     C                   Z-ADD     DUTKCI        KCI               4 0
385200021014
385300941130     C*---------------------------------------------------------------*
385400991018     C     KBLP          KLIST
385500950201     C                   KFLD                    ARBAAS
385600941130     C                   KFLD                    ARBLNP
385700941130     C                   KFLD                    ARBNRS
385800941130     C                   KFLD                    ARBNSP
385900941130     C                   KFLD                    COMTBL            2
386000990629     C     KTITA         KLIST
386100990629     C                   KFLD                    TASAAS
386200990629     C                   KFLD                    TASLNP
386300990629     C                   KFLD                    TASNRS
386400990629     C                   KFLD                    TASNSP
386500990629     C                   KFLD                    §TRC
386600941130     C     KBLPV         KLIST
386700950201     C                   KFLD                    ARBAAS
386800941130     C                   KFLD                    ARBLNP
386900941130     C                   KFLD                    ARBNRS
387000941130     C                   KFLD                    ARBNSP
387100941130     C                   KFLD                    COMTBL            2
387200941130     C                   KFLD                    WSV1
387300941130     C     KANN          KLIST
387400950201     C                   KFLD                    ARBAAS
387500941130     C                   KFLD                    ARBLNP
387600941130     C                   KFLD                    ARBNRS
387700941130     C                   KFLD                    ARBNSP
387800950202     C     KARBU         KLIST
387900941130     C                   KFLD                    ARBAAS
388000941130     C                   KFLD                    ARBLNP
388100941130     C                   KFLD                    ARBNRS
388200941130     C                   KFLD                    ARBNSP
388300991015     C                   KFLD                    ARBTRC
388400941130     C                   KFLD                    ARBDTV
388500941130     C                   KFLD                    ARBORV
388600950202     C     KARBM         KLIST
388700950202     C                   KFLD                    ARBAAS
388800950202     C                   KFLD                    ARBLNP
388900950202     C                   KFLD                    ARBNRS
389000950202     C                   KFLD                    ARBNSP
389100950202     C                   KFLD                    ARBCVB
389200950202     C                   KFLD                    ARBDTV
389300950202     C                   KFLD                    ARBORV
389400941130     C     KARBD         KLIST
389500941130     C                   KFLD                    ARBAAS
389600941130     C                   KFLD                    ARBLNP
389700941130     C                   KFLD                    ARBNRS
389800941130     C                   KFLD                    ARBNSP
389900941130     C                   KFLD                    ARBCVB
390000941130     C                   KFLD                    ARBDTV
390100941130     C                   KFLD                    COMORV            6 0
390200941130     C     KTAB          KLIST
390300941130     C                   KFLD                    CODUT
390400941130     C                   KFLD                    COD               2
390500941130     C                   KFLD                    KEY               8
390600941130     C     KACO          KLIST
390700941130     C                   KFLD                    CODUT
390800941130     C                   KFLD                    KCI
390900941130     C                   KFLD                    TASKSC
391000950202     C     KACO2         KLIST
391100950202     C                   KFLD                    CODUT
391200950202     C                   KFLD                    KCI
391300950202     C                   KFLD                    ARBCCM
391400981118     C     Kppa          KLIST
391500981118     C                   KFLD                    wsoc
391600981118     C                   KFLD                    wctb
391700981118     C                   KFLD                    wkcc
391800981118     C                   KFLD                    wksc
391900981118     C                   KFLD                    wdft
392000981118     C                   KFLD                    wnft
392100981118     C                   KFLD                    wfiv
392200981118     C     Kycpa         KLIST
392300981118     C                   KFLD                    ppasys
392400981118     C                   KFLD                    ppanrasint
392500941130     C     KERR          KLIST
392600941130     C                   KFLD                    ARBAAS
392700941130     C                   KFLD                    ARBLNP
392800941130     C                   KFLD                    ARBNRS
392900941130     C                   KFLD                    ARBNSP
393000941130     C                   KFLD                    ARBDER
393100941130     C                   KFLD                    ARBDAT
393200991214     c                   kfld                    ARBDTV
393300991214     c                   kfld                    ARBORV
393400941130     C     KTAM          KLIST
393500941130     C                   KFLD                    TASKSC
393600941130     C                   KFLD                    TASCTR
393700941130     C     KTAM2         KLIST
393800941130     C                   KFLD                    TASKSC
393900941130     C                   KFLD                    TASCTR
394000941130     C                   KFLD                    PRG               3 0
394100950202     C                   KFLD                    WFTC
394200151119     C     Kar5          KLIST
394300151119     C                   KFLD                    ARBAAS
394400151119     C                   KFLD                    ARBLNP
394500151119     C                   KFLD                    ARBNRS
394600151119     C                   KFLD                    ARBNSP
394700151119     C                   KFLD                    kar5trd
394800121108
394900121108
395000121108
395100941130     C**
395200941130     C* DEFINIZIONE CAMPI
395300941130     C**
395400960625     C     *LIKE         DEFINE    TASTBL        SAVTBL
395500990719     C     *LIKE         DEFINE    tptftc        WFTC
395600950202     C     *LIKE         DEFINE    TASVLF        SAVVLF
395700950202     C     *LIKE         DEFINE    TASVLF        SAVVLB
395800950202     C     *LIKE         DEFINE    TASFVF        SAVFVF
395900950202     C     *LIKE         DEFINE    TASFVF        SAVFVB
396000990625     C     *LIKE         DEFINE    TAScts        SAVcts
396100941130     C     *LIKE         DEFINE    TASSV1        WSV1
396200941130     C     *LIKE         DEFINE    TASVA1        WVA1
396300941130     C     *LIKE         DEFINE    ARBDER        SAVDER
396400941130     C     *LIKE         DEFINE    CSBPAR        DESPAR
396500970916     C     *LIKE         DEFINE    CSBPAR        DESPRV
396600010126     C     *LIKE         DEFINE    arblnp        savlnp
396700010126     C     *like         define    tasftc        WTCP
396800010126     C     *like         define    tasftc        WsvTCP
396900941130     C*---------------------------------------------------------------*
397000941130     C*
397100941130     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
397200941130     C* ASSUMO IL PRIMO MEMBRO DEL FILE
397300170109     C*    MBRFIL        IFEQ      *BLANK
397400170109     C*                  MOVEL     'M888888888'  MBRFIL
397500170109     C*                  END
397600941130     C* MEMBRO LUNGO 3
397700170109     C*                  MOVEL     ALFFIL        MBR               3 0
397800941130     C* PRENDO ANNO IN CORSO
397900170110     C****               MOVE      UDATE         AAS               2 0
398000070119     c* Apro file bolle di filiale
398100070119     c                   clear                   tibs55ds
398200070119     c                   eval      i50apo=001
398300070119     c                   call      'TIBS55R'
398400070119     c                   parm                    tibs55ds
398500070119     c*
398600070119     c                   Clear                   commt
398700070119     c                   Movel(p)  cmdt(1)       commt
398800070119     c                   Eval      %Subst(commt:30:10) = o50ala
398900070119     c                   Eval      commt =
399000070119     c                             %trim(commt) + '/FNBLP01L)'
399100070119     c                   Call      'QCMDEXC'
399200070119     c                   Parm                    commt
399300070119     c                   Parm                    lenght
399400070119     c                   Open      FNBLP01L
399500070119     c
399600070119     c
399700071102      * Aggancio tabella VPO record DECO
399800120510     c**********         Clear                   Dvpodeco
399900120510     c***********        Clear                   Tibs02ds
400000120510     c***********        Eval      T02tla = 'L'
400100120510     c***********        Eval      T02Mod = 'C'
400200120510     c*************      Eval      T02Sif = knsif
400300120510     c*************      Eval      T02Cod = 'VPO'
400400120510     c***********        Movel(p)  'DECO'        T02Ke1
400500120510     c*******            Call      'TIBS02R'
400600120510     c*********          Parm                    Kpjba
400700120510     c*********          Parm                    Tibs02ds
400800120510     c*********          If        T02Err = *Blanks
400900120510     c*********          Movel     T02Uni        Dvpodeco
401000120510     c**********         EndIf
401100071102
401200941130     C                   ENDSR
401300170110     C**---------------------------------------------------------
401400170110     C* Impostazione campi date
401500170110     C**---------------------------------------------------------
401600170110     c     sr_date       begsr
401700170110     C                   TIME                    W0140            14 0
401800170110     C                   MOVEL     W0140         UTIME             6 0
401900170110     C                   MOVE      W0140         WDTGIO            8 0
402000170110     C* UDATE IN AAAAMMGG
402100170110     C                   Z-ADD     WDTGIO        G02DAT
402200170110     C                   MOVEL     *BLANK        G02ERR
402300170110     C                   CALL      'XSRDA8'
402400170110     C                   PARM                    WLBDAT
402500170110     C                   MOVEL     G02INV        DATEU             8 0
402600170110     C* SOTTRAGGO  1
402700170110     C     G02TGI        SUB       1             GIOTGI
402800170110     C                   CALL      'XSRGI8'
402900170110     C                   PARM                    WGIDAT
403000170110     C                   Z-ADD     GIOINV        DATAM1            8 0
403100170110     C* SOTTRAGGO  2
403200170110     C     G02TGI        SUB       2             GIOTGI
403300170110     C                   CALL      'XSRGI8'
403400170110     C                   PARM                    WGIDAT
403500170110     C                   Z-ADD     GIOINV        DATAM2            8 0
403600170110     C* SOTTRAGGO  3
403700170110     C     G02TGI        SUB       3             GIOTGI
403800170110     C                   CALL      'XSRGI8'
403900170110     C                   PARM                    WGIDAT
404000170110     C                   Z-ADD     GIOINV        DATAM3            8 0
404100170110     c                   endsr
404200930804     C*
404300981001     C**-------------------------------------------------------------
404400981001     C** AGGIORNO CON CODICI DI VARIAZIONE DM DD BOLLA SEDE MITT/DEST
404500981001     C**-------------------------------------------------------------
404600981001     C     AGTMD         BEGSR
404700981001     C* CONTROLLO CHE TIPO DI VARIAZIONE STO LEGGENDO
404800981001     C* DM CREAZIONE MITTENTE BOLLA SEDE
404900981001     C* DD CREAZIONE DESTINATARIO BOLLA SEDE
405000981001     C* I0 VARIAZIONE DESTINATARIO
405100981001     C* MI VARIAZIONE MITTENTE
405200981001     C*
405300981001    1C     ARBCVB        IFNE      'DM'
405400981001     C     ARBCVB        ANDNE     'DD'
405500981001     C     ARBCVB        ANDNE     'I0'
405600140407     C     ARBCVB        ANDNE     'I7'
405700140407     C     ARBCVB        ANDNE     'I8'
405800140407     C     ARBCVB        ANDNE     'I9'
405900981001     C     ARBCVB        ANDNE     'MI'
406000981001     C*
406100981001   X1C                   ELSE
406200981001     C* CERCO LA SPEDIZIONE NEL FILE TNTMD
406300981001     C     KANN          CHAIN     TNTMD01L                           48
406400981001     C* IMMISSIONE O CAMBIO MITTENTE
406500981001     C* CAUSALI DI VARIAZIONI
406600981001    2C     ARBCVB        IFEQ      'DM'
406700981001     C     ARBCVB        OREQ      'MI'
406800981001     C*
406900981001     C                   MOVEL     ARBRSM        TMDRSM
407000981001     C                   MOVEL     ARBINM        TMDINM
407100981001     C                   MOVEL     ARBCAM        TMDCAM
407200981001     C                   MOVEL     ARBLOM        TMDLOM
407300981001     C                   MOVEL     ARBPRM        TMDPRM
407400981001     C                   MOVEL     ARBNZM        TMDNZM
407500981001     C*
407600981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
407700981001    3C     *IN48         IFEQ      '1'
407800981005     C     ARBCVB        ANDEQ     'DM'
407900981001     C                   Z-ADD     ARBAAS        TMDAAS
408000981001     C                   Z-ADD     ARBLNP        TMDLNP
408100981001     C                   Z-ADD     ARBNRS        TMDNRS
408200981001     C                   Z-ADD     ARBNSP        TMDNSP
408300981001     C                   WRITE     TNTMD000
408400981001     C*
408500981005    3C                   ENDIF
408600981005     C* PER 48 SPENTO AGGIORNO IL RECORD
408700981005     C  N48              UPDATE    TNTMD000
408800981001     C*
408900981001    2C                   ENDIF
409000981001     C*
409100981001     C* IMMISSIONE O CAMBIO DESTINATARIO
409200981001     C* CAUSALI DI VARIAZIONI
409300981001    2C     ARBCVB        IFEQ      'DD'
409400981001     C     ARBCVB        OREQ      'I0'
409500140407     C     ARBCVB        OREQ      'I7'
409600140407     C     ARBCVB        OREQ      'I8'
409700140407     C     ARBCVB        OREQ      'I9'
409800981001     C*
409900981001     C                   MOVEL     ARBRSD        TMDRSD
410000981001     C                   MOVEL     ARBIND        TMDIND
410100981001     C                   MOVEL     ARBCAD        TMDCAD
410200981001     C                   MOVEL     ARBLOD        TMDLOD
410300981001     C                   MOVEL     ARBPRD        TMDPRD
410400981001     C                   MOVEL     ARBNZD        TMDNZD
410500981001     C*
410600981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
410700981001    3C     *IN48         IFEQ      '1'
410800981005     C     ARBCVB        ANDEQ     'DD'
410900981001     C                   Z-ADD     ARBAAS        TMDAAS
411000981001     C                   Z-ADD     ARBLNP        TMDLNP
411100981001     C                   Z-ADD     ARBNRS        TMDNRS
411200981001     C                   Z-ADD     ARBNSP        TMDNSP
411300981001     C                   WRITE     TNTMD000
411400981005    3C                   ENDIF
411500981005     C*
411600981001     C* PER 48 SPENTO AGGIORNO IL RECORD
411700981005     C  N48              UPDATE    TNTMD000
411800981001     C*
411900981001    2C                   ENDIF
412000981001     C*
412100981001     C* SE LA VARIAZIONE ARB E' DD O DM DELETO IL RECORD E NON PROSEGUO CON
412200981001     C* LA LETTURA
412300981001    2C     ARBCVB        IFEQ      'DM'
412400981001     C     ARBCVB        OREQ      'DD'
412500981001     C   11              DELETE    FNARBD00
412600981001     C   16              DELETE    FNARBM00
412700981001     C                   MOVE      'Y'           DMDD
412800981001    2C                   ENDIF
412900981001     C*
413000981001    1C                   ENDIF
413100981001     C*
413200981001     C                   ENDSR
413300120312     C**-------------------------------------------------------------
413400120515     C** RITASSAZIONE DELLA SPEDIZIONE CON I NUOVI DATI RICEVUTI
413500120312     C**-------------------------------------------------------------
413600120313     C     ctr_vrb       BEGSR
413700151008     c* Richiamo pgm memorizzazione variazioni post-fatturazione
413800151008     c                   clear                   fnlvv1ds
413900151008     c                   clear                   parvar
414000151008     c                   clear                   partitas
414100151008     c                   clear                   parcsb
414200151008     c                   eval      iv1cvb=comcvb
414300151008     c                   eval      iv1pgm=nompgm
414400151008     c                   select
414500151008     c                   when      %subst(iv1cvb:1:1)='I'
414600170109     c     nrri          chain     fnarbd2t      dsarbd
414700170109     c                   if        %found(fnarbd2t)
414800151008     c                   eval      parvar =dsarbd
414900151008     c                   endif
415000151008     c                   when      %subst(iv1cvb:1:1)='K'
415100170109     c     nrri          chain     fnarbk2t      dsarbk
415200170109     c                   if        %found(fnarbk2t)
415300151008     c                   eval      parvar =dsarbk
415400151008     c                   endif
415500151008     c                   if        wanncas='S'
415600151008     c                   eval      iv1can='S'
415700151008     c                   endif
415800151008     c                   eval      parcsb  = dscsb
415900151008     c                   when      %subst(iv1cvb:1:1)='T'
416000170109     c     nrri          chain     fiarbt2t      dsarbt
416100170109     c                   if        %found(fiarbt2t)
416200151008     c                   eval      parvar =dsarbt
416300151008     c                   endif
416400151008     c                   when      %subst(iv1cvb:1:1)='C'
416500170109     c     nrri          chain     fnarbg2t      dsarbg
416600170109     c                   if        %found(fnarbg2t)
416700151008     c                   eval      parvar =dsarbg
416800151008     c                   endif
416900151008     c                   endsl
417000151008     c                   eval      kpjbu=fnlvv1ds
417100151009     c                   eval      partitas=dstitas
417200151008     c                   call      'FNLVV1R'
417300151008     c                   parm                    kpjba
417400151008     c                   parm                    parvar
417500151008     c                   parm                    partitas
417600151008     c                   parm                    parcsb
417700120312     c                   endsr
417800121108      ****************************************************
417900121108      ** Memorizzazione tracciato della bolla di sede prima
418000121108      ** della variazione
418100121108      ****************************************************
418200121205     c     memo_dstitas  Begsr
418300121116     c                   eval      dstitas=titasds
418400121108     c                   endsr
418500121109      ****************************************************
418600121109      ** Memorizzazione dati del contrassegno prima
418700121109      ** della variazione
418800121109      ****************************************************
418900121109     c     memo_csb      Begsr
419000121109     c                   clear                   dscsb
419100121109     C     KANN          CHAIN(N)  TNCSB000
419200121109     c                   if        %found(tncsb03l)
419300121115     c                   eval      dscsb=csbds
419400121109     c                   endif
419500121109     c                   endsr
419600981001     C*
419700920113**
4198009201301 BOLLA  INESISTENTE                                                1
419900920130TIPO BOLLA INESISTENTE                                              2
420000920130CAUSALE VARIAZIONE INESISTENTE                                      3
420100960514NON ESISTE RECORD BOLLA IN TNAC200F                                 4
420200920130NON ESISTE ANAGRAFICA CONTRASSEGNI                                  5
420300920130NUOVO CODICE BOLLA INESISTENTE                                      6
4204009201302 BOLLA  INESISTENTE                                                7
420500920220SPEDIZIONE GIA' TASSATA                                             8
420600920212SPEDIZIONE GIA' CONTABILIZZATA                                      9
420700920210C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
420800950201MANCA RECORD FILE FNARBM0R                                          11
420900950201MANCA X CREAZ. ASSEGNATO FNARBD1R                                   12
421000930513X CREAR 2BOLLA MA NO VARIAZ CD.BOLL                                 15
421100920601GIA' CONTABIL.:NO TOLTA VARIA G                                     14
421200930513
421300920601GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 16
421400930513
421500920601GIA' CONTABIL.:NO AGGIUNTA VARIA G                                  18
421600930513
421700920601GIA' CONTABIL.:NO VARIATO L'A3                                      20
421800930513
421900920601GIA' CONTABIL.:NO MODIFIC. VARIA G                                  22
422000990414GIA' TASSATA  :NO TOLTA    VARIA G                                  23
422100920228GIA' TASSATA  :NO AGGIUNTA VARIA G                                  24
422200920228GIA' TASSATA  :NO MODIFIC. VARIA G                                  25
422300950203CAP E NAZIONE MITTENTE INESISTENTE                                  26
422400950203COD.MITTENTE INESISTENTE IN ANAGRAF                                 27
422500920302SPEDIZIONE ANNULLATA                                                28
422600960919COD.MITTENTE BLOCCATO                                               29
422700970814ERRORE IN DETERMINAZIONE COD.TASSAZ                                 30
422800990414AGGIUNTO   IMPORTO DA ASSICURARE                                    31
422900990414TOLTO      IMPORTO DA ASSICURARE                                    32
423000990414MODIFICATO IMPORTO DA ASSICURARE                                    33
423100990414GIA' CONTABIL.:NO TOLTA    VARIA R                                  34
423200990414GIA' CONTABIL.:NO AGGIUNTA VARIA R                                  35
423300990414GIA' CONTABIL.:NO MODIFIC. VARIA R                                  36
423400991022
423500991022
423600991022
423700991022AGGIUNTO IM.ASS. TP.BOLLA NON ASSIC                                 40
423800991022TOLTO    IM.ASS. TP.BOLLA NON ASSIC                                 41
423900991022MODIFIC. IM.ASS. TP.BOLLA NON ASSIC                                 42
424000070119** cmdt
424100070119OVRDBF FILE(FNBLP01L) TOFILE(
