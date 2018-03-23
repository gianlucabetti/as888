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
106600990625     C                   MOVEL     ARBFIN        TASFIN
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
108200990628     c* Se ho la seconda bolla contabilizzata 9999 non aggiornerò mai i
108300990628     c* i seguenti campi nemmeno sulla prima bolla.
108400990630     c* Dopodichè posso aggiornarli solo se la 1bolla non è contabilizzata
108500990628     c* o se non sono stati modificati inoltro cap e nazione
108600990628    1c     ksc42bo       ifne      9999
108700990628    2c     *in27         ifeq      *off
108800990628     c     flgmod        oreq      *blank
108900990625     C                   MOVEL(P)  ARBRSD        TASRSD
109000170123     c* aggiorno la seconda ragione sociale
109100170123     c                   exsr      sr_rd2
109200990625     C                   MOVEL(P)  ARBIND        TASIND
109300990625     C                   MOVEL(P)  ARBLOD        TASLOD
109400990625     C                   MOVEL(P)  ARBPRD        TASPRD
109500990701     c* Dalla filiale posso ricevere la partita iva del destinatario
109600990701     c* solo se la bolla è assegnato o solo in caso di doppia bolla
109700990701     c* In ogni caso vado SEMPRE in sostituzione di tascpd
109800990625     C                   MOVEL     ARBCPI        TASCPD
109900140219    2c                   endif
110000140219    1c                   endif
110100990625     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
110200130325     C***                MOVEL     ERR(9)        ARBDER
110300130325     c***                exsr      wtrerr
110400140219     c* La scrittura/aggiornamento del tivrb viene fatta sempre se la
110500140219     c* prima bolla è contabilizzata.
110600140219     c* Viene fatta a prescindere dal contenuto di FLGMOD perchè
110700140219     c* deve receprire anche eventuali variazioni che riportano
110800140219     c* i dati alla situazione originale ciè quella di titas
110900140219     c* Es: se prima vario inoltor da "P" a "I" e poi rimetto
111000140219     c* "P" devo receprie sul tivrb anche quest'ultima variazione
111100140219    1c                   if        *in27=*on
111200120321     c                   exsr      ctr_vrb
111300140219    1c                   END
111400140219   x1c*                  else
111500990628     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
111600130325     C***                MOVEL     ERR(9)        ARBDER
111700130325     c***                exsr      wtrerr
111800140219     c*                  exsr      ctr_vrb
111900140219    1c*                  END
112000990625     C* AGGIORNO TITAS
112100990625     C   02              UPDATE    TITAS000
112200990625     C   03              UPDATE    TITAS010
112300990625     C   04              UPDATE    TITASP00
112400990625     C*
112500990625     c                   endsr
112600170123     C**---------------------------------------------------------
112700170123     C** Aggiornamento seconda ragione sociale destinatario
112800170123     C**---------------------------------------------------------
112900170123     C     sr_rd2        BEGSR
113000170123     C* Aggiorna record Q di TITA4
113100170123     C                   movel     'D'           §trc
113200170123     c                   setoff                                       0506
113300170123     C     ktita         chain     tita430c                           32
113400170123     c* se non c'e' lo scrivo
113500170123    1c                   if        *in32
113600170123     c                   movel     arbaas        ta4aas
113700170123     c                   movel     arblnp        ta4lnp
113800170123     c                   movel     arbnrs        ta4nrs
113900170123     c                   movel     arbnsp        ta4nsp
114000170123     c                   movel     'D'           ta4trc
114100170123     c                   clear                   ta4not
114200170123    1c                   endif
114300170123     c
114400170123     c                   eval      ta4not = arbrd2
114500170123     c*
114600170123    1c                   if        not *in32
114700170123    2C                   IF        TA4NOT<>*BLANKS
114800170123     C   05              update    tita4000
114900170123     C   06              update    tita4p00
115000170123     C                   ELSE
115100170123     C*SE eliminata e non c'e' più nulla, cancello il record
115200170123     c   05              delete    tita4000
115300170123     c   06              delete    tita4p00
115400170123    2c                   endif
115500170123     c
115600170123   x1c                   else
115700170123    2c                   if        ta4not<>*blanks
115800170123     C  N04              write     tita4000
115900170123     C   04              write     tita4p00
116000170123    2c                   endif
116100170123     c*
116200170123    1c                   endif
116300170123     c                   endsr
116400120322     C**---------------------------------------------------------
116500120322     C** Richiamo tisi95 per determinazione codice tassazione
116600120322     C**---------------------------------------------------------
116700120322     C     C_tisi95      BEGSR
116800120322     C                   CLEAR                   DSSI95
116900120322     C                   MOVEL     ARBCAD        I95CAP
117000120322     C                   MOVEL     ARBNZD        I95NAR
117100120322     C                   MOVEL     ARBLOD        I95LOC
117200120322     C                   MOVEL     ARBPRD        I95PRV
117300121116     c                   movel     tasaas        i95dat
117400121116     c                   move      tasmgs        i95dat
117500121116     C****               MOVEL     TASDSP        I95DAT
117600120322     C                   MOVEL     '7'           I95TCN
117700120322      * Se network FedEx passo flag apposito per dire di prendere i dati
117800120322      * dalla tabella della nazione
117900120322     c     taslna        chain     azorg01l
118000120322     c                   if        %found(azorg01l) and orgfva = *blanks
118100120322     c                   eval      og143 = orgde3
118200120322     c     §ogntw        ifeq      'FED'
118300120322     c                   eval      i95fi1 = 'S'
118400120322     c                   endif
118500120322     c                   endif
118600120322
118700120322     C                   MOVEL     §3ATB1        I95TPO
118800120322    5C     §3AFCA        IFNE      0
118900120322     C                   MOVEL     'S'           I95FCA
119000120322    5C                   ENDIF
119100120322     C**
119200120322     C                   CALL      'TISI95R'
119300120322     C                   PARM                    DSSI95
119400120322     c                   endsr
119500950202     C**---------------------------------------------------------
119600950202     C** AGGIORNO CODICE VARIAZIONI MX
119700950202     C**---------------------------------------------------------
119800950202     C     AGARBM        BEGSR
119900950203     C* VEDO SE ESISTE CODICE
120000950202     C                   MOVE      ARBCCM        KSC4
120100950202     C*
120200950202     C* SE E' CODIFICATO CHAIN SU ANAGRAFICA
120300950202     C     KSC4          IFNE      8888
120400981001     C                   CLEAR                   DSBS69
120500981001     C                   MOVEL     KNSIF         I69SIF
120600981001     C                   MOVEL     KCI           I69KCC
120700981001     C                   MOVEL     ARBCCM        I69KAC
120800981001     C                   MOVEL     ARBCCM        I69KIN
120900981001     C                   CALL      'TIBS69R'
121000981001     C                   PARM                    DSBS69
121100981001     C                   PARM                    DSACO
121200981001     C                   PARM                    DSIND
121300981001     C                   PARM                    DSCLP
121400981001     C                   PARM                    DSCLS
121500950202     C*
121600950203     C* DO ERRORE SOLO SE SI TRATTA DI CLIENTE DI ALTRA FILIALE
121700981001     C     O69ERR        IFNE      ' '
121800170109     C*  ATTENDO L'ARRIVO DEL PDC
121900950203     C                   SETON                                        37
122000950202     C                   GOTO      ENDGBM
122100960919     C                   ELSE
122200960919     C* SE CLIENTE BLOCCATO CARICO LO STESSO MA SEGNALO
122300100303     C***  ACOABL        IFEQ      '8'
122400100303     C***                MOVEL     ERR(29)       ARBDER
122500100303     C***                EXSR      WTRERR
122600100303     C***                END
122700950202     C                   ENDIF
122800950202     C                   ENDIF
122900950203     C*
123000950203     C* VERIFICO ESISTENZA NAZIONE E PROVINCIA MITTENTE
123100970714     C                   CLEAR                   DSSI95
123200970714     C                   MOVEL     ARBCAM        I95CAP
123300121116     C                   MOVEL     ARBNZM        I95NAR
123400121116     c                   movel     tasaas        i95dat
123500121116     c                   move      tasmgs        i95dat
123600121116     C****               MOVEL     TASDSP        I95DAT
123700970714     C                   MOVEL     '3'           I95TCN
123800020311      * Se network FedEx passo flag apposito per dire di prendere i dati
123900020311      * dalla tabella della nazione
124000020311     c     arblnp        chain     azorg01l
124100020311     c                   if        %found(azorg01l) and orgfva = *blanks
124200020311     c                   eval      og143 = orgde3
124300020311     c     §ogntw        ifeq      'FED'
124400020311     c                   eval      i95fi1 = 'S'
124500020311     c                   endif
124600020311     c                   endif
124700020311
124800970714     C                   CALL      'TISI95R'
124900970714     C                   PARM                    DSSI95
125000950203     C*
125100970714     C     O95ERR        IFNE      *BLANKS
125200130326     C                   Z-ADD     102           COMFEV
125300950203     C                   MOVEL     ERR(26)       ARBDER
125400950203     C                   EXSR      WTRERR
125500950203     C                   GOTO      ENDGBM
125600950203     C                   ENDIF
125700950203     C*
125800950203     C* VERIFICO SE ESISTEN L'ANAGRAFICA DEI C/A E SE GIA' INCASSATO
125900950222    1C     §3AFCA        IFNE      0
126000950202     C                   EXSR      CONCSB
126100950222     C* INESISTENTE  --> NON AGGIORNO
126200950222     C   25              GOTO      ENDGBM
126300950203     C***
126400950203     C*** A G G I O R N O   C / A S S E G N O
126500950222     C* AGGIORNO SOLO SE NON E' INCASSATO
126600950222   1AC     *IN31         IFEQ      *OFF
126700950202     C     KANN          CHAIN     TNCSA000                           30
126800950202     C*
126900950202     C* NON E' 8888 :SE CREATA CANCELLO ANAGRAFICA C/A
127000950202IF  2C     KSC4          IFNE      8888
127100970916IF  2C     KSC4          ANDNE     9999
127200950202     C  N30              DELETE    TNCSA000
127300950202X   2C                   ELSE
127400950202     C                   CLEAR                   TNCSA000
127500950202     C*
127600950202     C                   MOVEL     CSBAAS        CSAAAS
127700950202     C                   MOVEL     CSBLNP        CSALNP
127800950202     C                   MOVEL     CSBNRS        CSANRS
127900950202     C                   MOVEL     CSBNSP        CSANSP
128000950202     C                   MOVEL     ARBRSM        CSARSD
128100950202     C                   MOVEL     ARBINM        CSAVID
128200950202     C                   MOVEL     ARBCAM        CSACAD
128300950202     C                   MOVEL     ARBLOM        CSACID
128400950202     C                   MOVEL     ARBPRM        CSAPRM
128500950202     C                   MOVEL     ARBNZM        CSANZM
128600950202     C*
128700950202     C  N30              UPDATE    TNCSA000
128800950202     C   30              WRITE     TNCSA000
128900950202E   2C                   ENDIF
129000950202     C*
129100950202     C                   MOVEL     ARBCCM        CSBCDI
129200950202     C*
129300950202     C* SE C/A SU 1 BOLLA IMPOSTO CODICE BOLLA AMCHE
129400950202    2C     §3AFCA        IFEQ      1
129500950202     C                   MOVEL     ARBCCM        CSBKSC
129600950202    2C                   ENDIF
129700950202     C*
129800950202     C                   UPDATE    TNCSB000
129900950222   1AC                   ENDIF
130000950222    1C                   ENDIF
130100950202     C*
130200950203     C* A G G I O R N O   L A   1  B O L L A
130300970714     C                   MOVEL     O95CTS        TASMCT
130400950202     C                   MOVEL     ARBCAM        TASCAM
130500950202     C                   MOVEL     ARBNZM        TASNZM
130600950202     C                   MOVEL     ARBFAP        TASFAP
130700950202     C                   MOVEL     ARBCCM        TASKSC
130800990628     C                   MOVEL     ARBCCM        TASccm
130900950202     C*
131000950202     C*
131100990628     c                   movel     'M'           §trc
131200990629     c     ktita         chain     titaa000                           30
131300950202IF  3C     KSC4          IFEQ      8888
131400990628     c   30              z-add     tasaas        taaaas
131500990628     c   30              z-add     taslnp        taalnp
131600990628     c   30              z-add     tasnrs        taanrs
131700990628     c   30              z-add     tasnsp        taansp
131800990628     c   30              movel     §trc          taatrc
131900990628     C                   MOVEL     ARBRSM        TAaRSc
132000990628     C                   MOVEL     ARBINM        TAaIND
132100990628     C                   MOVEL     ARBCAM        TAaCAP
132200990628     C                   MOVEL     ARBLOM        TAaLOc
132300990628     C                   MOVEL     ARBPRM        TAaPRV
132400990628     C                   MOVEL     ARBNZM        TAaNAZ
132500990628     C                   MOVEL     ARBCPI        TAacpi
132600990628     c  n30              update    titaa000
132700990628     c   30              write     titaa000
132800990628     c                   else
132900990628     c* se prima della variazione era un 8888 lo vado a cancellare da
133000990628     c* titaa00f
133100990628     c  n30              delete    titaa000
133200990628     c
133300950202E   3C                   ENDIF
133400950202     C*
133500990628     C   02              UPDATE    TiTAS000
133600071029     C***
133700950203     C*** A G G I O R N O   2   B O L L A
133800950202    3C     §3ATB2        IFNE      '  '
133900950202     C                   MOVEL     §3ATB2        COMTBL
134000950202     C                   SETOFF                                       020304
134100990628     C     KBLP          CHAIN     TiTAS30C                           39
134200950202     C*
134300950203     C     *IN39         IFEQ      *OFF
134400950202     C                   MOVEL     ARBCCM        TASCCM
134500950202     C*
134600950202     C                   MOVEL     ARBCAM        TASCAM
134700950202     C                   MOVEL     ARBNZM        TASNZM
134800950202     C                   MOVEL     ARBFAP        TASFAP
134900970714     C                   MOVEL     O95CTS        TASMCT
135000950202     C*
135100990628     C   02              UPDATE    TiTAS000
135200990628     C   03              UPDATE    TiTAS010
135300990628     C   04              UPDATE    TiTASP00
135400950202     C                   ENDIF
135500950202     C                   ENDIF
135600950202     C*
135700950202     C     ENDGBM        ENDSR
135800950202     C**---------------------------------------------------------
135900950202     C** AGGIORNO CODICE VARIAZIONI CX
136000950202     C**---------------------------------------------------------
136100950202     C     AGARBC        BEGSR
136200010126     C     arblnp        ifne      SAVLNP
136300010126     C     arblnp        chain     azorg01l                           32
136400010126     C  n32              movel     orgde3        og143
136500010126     C   32              clear                   og143
136600010126     C                   z-add     arblnp        savlnp
136700010126     C                   endif
136800010126     C* SE BOLLA POSTE ED AGGIUNTO O TOLTO IL FUORI MISURA BISOGNA
136900010126     C* AGGIORNARE tita4 TIPO RECORD "J"
137000010126     C     §ogpt         ifeq      'S'
137100010126     C                   clear                   wtcp
137200010126     C                   clear                   wsvtcp
137300010126    2C     arbtc1        ifeq      'F'
137400010126     C     arbtc2        oreq      'F'
137500010126     C                   movel     'F'           wtcp
137600010126    2C                   endif
137700010126    2C     tasftc        ifeq      'F'
137800010126     C     tastc2        oreq      'F'
137900010126     C                   movel     'F'           wsvtcp
138000010126    2C                   endif
138100010126    2C                   select
138200010126     C* SE NON E' STATA NE' AGGIUNTA NE' TOLTA LA "F" NON FACCIO NIENTE
138300010126     C     arbtc1        whenne    'F'
138400010126     C     arbtc2        andne     'F'
138500010126     C     tasftc        andne     'F'
138600010126     C     tastc2        andne     'F'
138700010126     C     wtcp          oreq      'F'
138800010126     C     wsvtcp        andeq     'F'
138900010126     C                   other
139000010126     C                   movel     'J'           §trc
139100010126     c                   setoff                                       0506
139200010126     C     ktita         chain     tita430c                           32
139300010126    3C     *in32         ifeq      *off
139400010126     C                   movel     ta4not        dsbl4j
139500010126     C* TOLTO LA 'F'
139600010126    4C     wtcp          ifeq      *blanks
139700010126     C     wsvtcp        andeq     'F'
139800010126     C                   clear                   §b4bai
139900010126   X4C                   else
140000010126     C* AGGIUNTA 'F'
140100010126    5C     wtcp          ifeq      'F'
140200010126     C     wsvtcp        andeq     *blanks
140300010126     C                   movel     'S'           §b4bai
140400010126    5C                   endif
140500010126    4C                   endif
140600010126     C                   movel     dsbl4j        ta4not
140700010126     C   05              update    tita4000
140800010126     C   06              update    tita4p00
140900010126   X3C                   else
141000010126     C* AGGIUNTA 'F' --> CREO RECORD "J"
141100010126    4C     wtcp          ifeq      'F'
141200010126     C     wsvtcp        andeq     *blanks
141300010126     C                   clear                   tita4000
141400010126     C                   z-add     tasaas        ta4AAS
141500010126     C                   z-add     taslnp        ta4LNP
141600010126     C                   z-add     tasnrs        ta4NRS
141700010126     C                   z-add     tasnsp        ta4NSP
141800010126     C                   movel     'J'           ta4TRC
141900010126     C                   clear                   dsbl4j
142000010126     C                   clear                   ta4not
142100010126     C                   movel     'S'           §B4BAI
142200010126     C                   movel     dsbl4j        ta4NOT
142300010126     C  N04              write     tita4000
142400010126     C   04              write     tita4p00
142500010126    4C                   endif
142600010126    3C                   endif
142700010126    2C                   endsl
142800010126     C                   endif
142900950202     C* AGGIORNO SEMPRE  LE CONSEGNE PARTICOLARI SOLO SU  1 BOLLA
143000991018     c* idem per gg chiusura e data consegna richiesta
143100950202     C                   MOVEL     ARBTC1        TASFTC
143200950202     C                   MOVEL     ARBTC2        TASTC2
143300991018     c                   movel     arbtcr        tastcr
143400991018     c                   z-add     arbdcr        tasdcr
143500991018     c                   z-add     arbhcr        tashcr
143600991018     C                   MOVEL     arbgC1        tasgc1
143700991018     C                   MOVEL     arbgC2        tasgc2
143800990628     C   02              UPDATE    TiTAS000
143900990628     C   03              UPDATE    TiTAS010
144000990628     C   04              UPDATE    TiTASP00
144100950202     C                   ENDSR
144200950202     C*
144300920113     C**---------------------------------------------------------
144400920113     C** AGGIORNO CODICE VARIAZIONI TX
144500920113     C**---------------------------------------------------------
144600920113     C     AGARBT        BEGSR
144700920113     C*
144800920226     C                   EXSR      CTR2
144900920226     C*
145000941130    1C  N29              DO
145100950202     C*
145200991102     c
145300990419     c* controllo l'importo da assicurare se la spedizione e' tassata
145400990419     c*  o con tot.imponibile
145500011029     c                   if        tasias > 0  and tasvas = *blanks
145600011029     c                   movel     'ITL'         tasvas
145700011029     c                   endif
145800990419     c                   if        arbias <> tasias or
145900990419     c                             arbvas <> tasvas
146000990419     c*
146100990419     c                   exsr      AGGIAS
146200990419     c                   endif
146300950202     C*
146400950202     C                   Z-ADD     ARBIAS        TASIAS
146500950202     C                   MOVEL     ARBVAS        TASVAS
146600990824     c                   clear                   TASFCM
146700920113     C*
146800941130    2C     §3ATB2        IFEQ      '  '
146900970514     C     COMCVB        OREQ      'TP'
147000950222     C* AGGIORNO KSC SOLO SE VARIAZIONE DA ARRIVO E NON DA PARTENZA
147100950222     C*  PERCHE' SI TRATTA DI VARIAZIONE DI MITTENTE E NON DI TASSAZION
147200950222     C     ARBCVB        IFNE      'TP'
147300920113     C                   Z-ADD     ARBKSC        TASKSC
147400950222     C                   ENDIF
147500051219     c* Non vado sopra al codice tariffa se bolla in assegnato tassata con
147600051219     c* tariffa mittente
147700051219     c     tasftm        ifeq      *blanks
147800051219     C     comcvb        oreq      'TP'
147900950222     C                   MOVEL     ARBCTR        TASCTR
148000051219     c                   endif
148100950202     C                   Z-ADD     ARBQFT        TASQFT
148200991015     c                   MOVEL     ARBDIV        TASDIV
148300920113     C                   Z-ADD     ARBPOR        TASPOR
148400920113     C                   MOVEL     ARBSV1        TASSV1
148500920113     C                   Z-ADD     ARBVA1        TASVA1
148600920113     C                   MOVEL     ARBSV2        TASSV2
148700941130     C                   Z-ADD     ARBVA2        TASVA2
148800950202     C                   MOVEL     ARBSV3        TASSV3
148900950202     C                   Z-ADD     ARBVA3        TASVA3
149000941130     C*
149100990628     C* SE ESITONO GIA' DELLE VARIE IN TITA700F LE CANCELLO
149200990628     C     KBLP          SETLL     TITA7000
149300990628     C     KBLP          READE     TITA7000                               30
149400941130     C*
149500941130    3C     *IN30         DOWEQ     *OFF
149600991015     C                   DELETE    TITA7000
149700991015     C     KBLP          READE     TITA7000                               30
149800941130    3C                   ENDDO
149900170109     C     KARBU         SETLL     FIARBU2l
150000170109     C     KARBU         READE     FIARBU2l                               30
150100950202     C*
150200950202     C     *IN30         DOWEQ     *OFF
150300990628     C                   CLEAR                   TiTA7000
150400950202     C                   Z-ADD     ARBAAS        TA7AAS
150500950202     C                   Z-ADD     ARBLNP        TA7LNP
150600950202     C                   Z-ADD     ARBNRS        TA7NRS
150700950202     C                   Z-ADD     ARBNSP        TA7NSP
150800950202     C                   MOVEL     TASTBL        TA7TBL
150900950202     C                   MOVEL     ARBSVN        TA7SVN
151000950202     C                   Z-ADD     ARBVAN        TA7VAN
151100990629     C                   WRITE     TITA7000
151200950202     C*
151300950202     C                   ADD       1             NUMRBU            7 0
151400170109     C                   DELETE    FIARBUtt
151500950202     C*
151600170110     C     KARBU         READE     FIARBU2L                               30
151700950202     C                   ENDDO
151800990628     c
151900941130     C*
152000920113     C                   Z-ADD     ARBDFT        TASDFT
152100920113     C                   Z-ADD     ARBNFT        TASNFT
152200920113     C                   Z-ADD     ARBFIV        TASFIV
152300941130     C                   MOVEL     ARBCEI        TASFEI
152400920113     C*
152500990624     C                   z-add     arbimv        TASimv
152600151120     c* Totale imponibile = 0 --> Cancello record FAT se presente e imposto fpf='R'
152700151120    3c                   if        tasimv=0
152800151120     c                   Eval      kar5trd='FAT'
152900151120     c     kar5          Chain     Fiar531c
153000151120     c                   if        %found(fiar531c)
153100151120     c   02              delete    fiar5000
153200151120     c                   endif
153300151118     c                   eval      tasfpf='R'
153400151120   x3c                   else
153500151120     c* Totale imponibile > 0 --> Scrivo record FAT se manca imposto in base a record FAT
153600151120     c*                           Imposto TASFPF in base al record FAT
153700151118     c                   clear                   dar5fat
153800151118     c                   Eval      kar5trd='FAT'
153900151118     c     kar5          Chain     Fiar531c
154000151120    4c                   If        %Found(Fiar531c)
154100151118     c                   Movel     ar5uni        dar5fat
154200151120   x4c                   else
154300151120     c                   Eval      Ar5Aas = ArbAas
154400151120     c                   Eval      Ar5Lnp = ArbLnp
154500151120     c                   Eval      Ar5Nrs = ArbNrs
154600151120     c                   Eval      Ar5Nsp = ArbNsp
154700151120     c                   Eval      Ar5Trd = 'FAT'
154800151120     c                   movel     tasvlf        §ar5vltas
154900151120     c                   movel     tasfvf        §ar5fvtas
155000151120     c                   movel     taspkf        §ar5pktas
155100151120     c                   movel     'R'           §ar5fptas
155200151120     c                   eval      §AR5DATAS=%dec(%date())
155300151120     c                   eval      §AR5HMTAS=%dec(%time())
155400151120     c                   eval      §AR5PRTAS=knmus
155500151120     c                   eval      §AR5NJTAS=knmtd
155600151120     c                   Movel(p)  dar5fat       Ar5uni
155700151120     c                   eval      ar5dac=%dec(%date())
155800151120     c                   eval      ar5orc=%dec(%time())
155900151120     c                   Eval      AR5FT1='T'
156000151120     c                   Eval      AR5DT1=99999999
156100151120     c                   Eval      AR5FT2='T'
156200151120     c                   Eval      AR5DT2=99999999
156300151120     c                   Eval      AR5FT3='T'
156400151120     c                   Eval      AR5DT3=99999999
156500151120     c   02              write     fiar5000
156600151120    4c                   endif
156700151120     c* Imposto TASFPF
156800151118     c                   select
156900151118     c                   when      §AR5FPTAS='R'
157000151118     c                   eval      tasfpf='R'
157100151118     c                   when      §AR5FPTAS='D'
157200151118     c                   eval      tasfpf='D'
157300151118     c                   other
157400151118     c                   eval      tasfpf='V'
157500151118     c                   endsl
157600151120    3c                   endif
157700941130    3C     ARBIFT        IFGT      0
157800941130     C                   MOVEL     '1'           TASFTS
157900920113     C                   ELSE
158000941130     C                   MOVEL     ' '           TASFTS
158100941130    3C                   END
158200941130    2C                   END
158300920113     C* AGGIORNO
158400990628     C   02              UPDATE    TiTAS000
158500941130    1C                   ENDDO
158600920113     C*
158700920113     C                   ENDSR
158800920113     C**---------------------------------------------------------
158900920113     C** AGGIORNO CODICE VARIAZIONI 2T
159000920113     C**---------------------------------------------------------
159100920113     C     AGAR2T        BEGSR
159200920113     C*
159300920226     C* SECONDA BOLLA
159400920113     C     §3ATB2        IFNE      '  '
159500920226     C                   EXSR      CTR2
159600920114     C*
159700920226     C  N29              DO
159800991015     C                   Z-ADD     ARBKSC        TASKSC
159900991015     C                   MOVEL     ARBCTR        TASCTR
160000991015     C                   movel     arbdiv        tasdiv
160100991015     C                   MOVEL     ARBSV1        TASSV1
160200991015     C                   Z-ADD     ARBVA1        TASVA1
160300991015     C                   MOVEL     ARBSV2        TASSV2
160400991015     C                   Z-ADD     ARBVA2        TASVA2
160500040114     c                   Movel     ArbSv3        TasSv3
160600040114     c                   Z-Add     ArbVa3        TasVa3
160700040114      * Se esistono già della varie su Tita7 le devo eliminare
160800040114     c     Kblp          Setll     Tita7000
160900040114     c                   Do        *Hival
161000040114     c     Kblp          Reade     Tita7000
161100040114     c                   If        %Eof
161200040114     c                   Leave
161300040114     c                   EndIf
161400040114     c                   Delete    Tita7000
161500040114     c                   EndDo
161600040114      * Scrivo il nuovo Tita7 e cancello il file di ricezione
161700170109     c     Karbu         Setll     Fiarbu2l
161800040114     c                   Do        *Hival
161900170109     c     Karbu         Reade     Fiarbu2l
162000170109     c                   If        %Eof(Fiarbu2l)
162100040114     c                   Leave
162200040114     c                   EndIf
162300040114     c                   Clear                   Tita7000
162400040114     c                   Z-add     ArbAas        Ta7Aas
162500040114     c                   Z-add     ArbLnp        Ta7Lnp
162600040114     c                   Z-add     ArbNrs        Ta7Nrs
162700040114     c                   Z-add     ArbNsp        Ta7Nsp
162800040114     c                   Movel     TasTbl        Ta7Tbl
162900040114     c                   Movel     ArbSvn        Ta7Svn
163000040114     c                   Z-add     ArbVan        Ta7Van
163100040114     c                   Write     Tita7000
163200040114     c                   Add       1             NumRbu
163300170109     c                   Delete    Fiarbutt
163400040114     c                   EndDo
163500040114
163600991015     C                   Z-ADD     ARBDFT        TASDFT
163700991015     C                   Z-ADD     ARBNFT        TASNFT
163800991015     C                   Z-ADD     ARBFIV        TASFIV
163900991015     C                   MOVEL     ARBCEI        TASFEI
164000040114     C**!!!              z-add     arbva1        TASimv
164100040114     c                   Z-add     ArbImv        TasImv
164200991015     C     ARBIFT        IFGT      0
164300941130     C                   MOVEL     '1'           TASFTS
164400920114     C                   ELSE
164500941130     C                   MOVEL     ' '           TASFTS
164600920114     C                   END
164700920113     C*
164800040114      * Aggiorno Titas
164900990628     C   02              UPDATE    TiTAS000
165000920113     C                   END
165100920130     C                   END
165200920113     C*
165300920113     C                   ENDSR
165400920127     C**---------------------------------------------------------
165500920127     C** AGGIORNO CODICE VARIAZIONI KX
165600920127     C**---------------------------------------------------------
165700920127     C     AGARBK        BEGSR
165800121109     c                   if        *in11='0' and *in16='0'
165900121109     c                   exsr      memo_csb
166000121109     c                   endif
166100121205     c                   clear                   wanncas
166200920212     C                   SETOFF                                       31
166300941130     C                   CLEAR                   TNCSB000
166400950202     C* NON AGGIORNO SE HO LETTO
166500950202    1C     *IN11         IFEQ      *OFF
166600950202     C     *IN16         ANDEQ     *OFF
166700920127     C* VARIA CONTRASSEGNO
166800941130    2C     ARBCBP        IFEQ      ARBCBN
166900920127     C* VEDO CHE TIPO BOLLA AGGANCIARE DAL FLAG C/ASSEGNO
167000920131     C* RIAGGANCIO SOLO LA SECONDA PERCHE' LA PRIMA L'HO GIA' AGGANCIAT
167100941130    3C     §3AFCA        IFEQ      2
167200920127     C                   MOVEL     §3ATB2        COMTBL
167300941130     C                   SETOFF                                       020304
167400990625     C     KBLP          CHAIN     TITAS30C                           39
167500121108     c                   exsr      memo_dstitas
167600941130    3C                   END
167700920228     C*
167800920228     C* CONTROLLO SPEDIZIONE SE OK
167900990414     C                   EXSR      CTR3
168000920210     C*
168100920212     C* CONTROLLO ARCHIVIO C/ASSEGNO SE ERA PREVISTO
168200941130    3C     §3AFCA        IFNE      0
168300920212     C                   EXSR      CONCSB
168400941130    3C                   END
168500920210     C* SE NO ERRORE
168600950324    3C  N31CSBDDC        IFEQ      0
168700950324     C     CSBBNA        ANDEQ     0
168800941130     C                   MOVEL     *BLANKS       ARBDER
168900941130     C*
169000920228     C*** AGGIORNO CONTRASSEGNO
169100920228     C*** PRIMA     ESISTEVA ORA     ESISTE: V A R I A Z I O N E
169200941130    4C     CSBCAS        IFNE      0
169300950202     C     ARBCAS        ANDNE     0
169400950202     C* SCRIVO SOLO SE IMPORTO DIVERSO
169500950202    5C     ARBCAS        IFNE      CSBCAS
169600991103    5C     ARBVCA        ORNE      WCSBVCA
169700000703     c*
169800000703     c     csbsta        oreq      9
169900920212     C* SCRIVO STORICO
170000920212     C                   MOVEL     'VACA'        CSVCAV
170100920212     C                   EXSR      CSVWTR
170200950202     C*
170300950202     C                   EXSR      AGGCSB
170400950202     C*
170500941130     C                   UPDATE    TNCSB000
170600920228     C*
170700120420     C* E R R O R E : NON MODIFICABILE IMPORTO VARIA G
170800130325     C***29              MOVEL     ERR(22)       ARBDER
170900120322     C   29              exsr      ctr_vrb
171000941130    5C                   END
171100941130    4C                   END
171200920228     C***
171300920228     C*** PRIMA     ESISTEVA ORA NON ESISTE: A N N U L L O
171400941130    4C     CSBCAS        IFNE      0
171500950202     C     ARBCAS        ANDEQ     0
171600920212     C                   EXSR      ANNCSB
171700920226     C*
171800920228     C* E R R O R E : NON ELIMINABILE IMPORTO VARIA G
171900920316     C                   MOVEL     *BLANKS       ARBDER
172000990505    5C   29              DO
172100941130     C* CONTROLLO SE ESISTE VARIA "G"
172200130326     C****               EXSR      ESVARG
172300130325    6C***  WSIVAR        IFEQ      'S'
172400130325     C***                MOVEL     ERR(14)       ARBDER
172500130325    6C***                END
172600121205     c                   eval      wanncas='S'
172700121203     c                   exsr      ctr_vrb
172800941130    5C                   END
172900941130    4C                   END
173000920228     C***
173100920228     C*** PRIMA NON ESISTEVA ORA     ESISTE: C R E O  RECORD C/ASSEGNO
173200941130    4C     CSBCAS        IFEQ      0
173300950202     C     ARBCAS        ANDNE     0
173400920212     C                   MOVEL     §3ATB1        TBLCA
173500941130     C                   MOVEL     §3ATB1        FLGTB1
173600941130     C*
173700941130     C                   MOVEL     TASLOD        DESPAR
173800990625     C                   MOVEL     TASPRD        DESPRV
173900950913     C*
174000920212     C                   EXSR      CRTCSB
174100920228     C* E R R O R E : NON POSSO AGGIUNGERE IMPORTO VARIA G
174200130325     C***29              MOVEL     ERR(18)       ARBDER
174300130325     c***29              setoff                                       36
174400120322     C   29              exsr      ctr_vrb
174500941130    4C                   END
174600920228     C***
174700920228     C*** AGGIORNO SPEDIZIONE
174800941130     C                   MOVEL     'S'           TASFCA
174900920228     C* SE GIA' TASSATA METTO A POSTO PER VARIA G
175000110203     c                   if        *in38
175100110203     c                   Z-ADD     arbcas        wcas
175200110203     C                   EXSR      VARIAG
175300110203     c
175400110203      * riorganizzo le varie se è necessario
175500110203     c                   if        flgriorg = '1'
175600110203     c                   exsr      sr_riorg
175700110203     c                   endif
175800110203     c                   endif
175900110203
176000950202     C* SCRIVO ERRORE SE GIA' CONTABILIZZATA/STORICIZZATA
176100941130    4C     ARBDER        IFNE      *BLANKS
176200920309     C                   EXSR      WTRERR
176300941130    4C                   END
176400920228     C*
176500990625     C   02              UPDATE    TITAS000
176600990625     C   03              UPDATE    TITAS010
176700990625     C   04              UPDATE    TITASP00
176800941130    3C                   END
176900920127     C*
177000941130   X2C                   ELSE
177100920228     C******
177200920228     C****** M O D I F I C A T O   T I P O   B O L L A
177300920228     C******
177400920212     C                   SETOFF                                       31
177500920127     C                   MOVEL     §3ATB1        SAVTB1            2
177600920127     C                   MOVEL     §3ATB2        SAVTB2            2
177700920227     C                   MOVEL     §3AFCA        SAVFCA            1 0
177800920227     C                   MOVEL     §3AFTD        SAVFTD            1 0
177900941130    3C     §3ABCM        IFNE      ' '
178000920324     C                   MOVEL     0             SAVFCA
178100941130    3C                   END
178200920127     C* NUOVO TIPO BOLLA
178300920127     C                   MOVEL     '3A'          COD
178400941130     C                   MOVEL(P)  ARBCBN        KEY
178500920212     C     KTAB          CHAIN     TABEL                              34
178600941130     C*
178700941130     C* COD BOLLA NUOVO NON TROVATO
178800941130    3C     *IN34         IFEQ      *ON
178900941130     C                   MOVEL     ERR(6)        ARBDER
179000920212     C*
179100941130     C                   SETON                                        37
179200941130     C                   Z-ADD     102           COMFEV
179300941130     C                   EXSR      WTRERR
179400941130     C**
179500941130   X3C                   ELSE
179600941130     C**
179700941130     C* COD BOLLA NUOVO TROVATO
179800920131     C                   MOVEL     TBLUNI        DS3A
179900941130    4C     §3ABCM        IFNE      ' '
180000920324     C                   Z-ADD     0             §3AFCA
180100941130    4C                   END
180200920131     C*
180300920212     C* ANNULLAMENTO CONTRASSEGNO
180400941130    4C     SAVFCA        IFNE      0
180500920212     C     §3AFCA        ANDEQ     0
180600920212     C                   EXSR      CONCSB
180700920212     C*
180800941209    5C     *IN31         IFEQ      *OFF
180900941209     C* IMPOSTO IL NUOVO TIPO BOLLA
181000941209    6C     SAVFCA        IFEQ      2
181100941209     C     §3ATB2        ANDNE     *BLANKS
181200941209     C                   MOVEL     §3ATB2        CSBTBL
181300941209     C                   ELSE
181400941209     C                   MOVEL     §3ATB1        CSBTBL
181500941209    6C                   ENDIF
181600941209     C*
181700941209     C                   EXSR      ANNCSB
181800941209    5C                   END
181900941209    4C                   END
182000950202     C***************
182100920212     C* SE C/A PASSA A  "F"  A  "A"  O VICEVERSA POSSO FARLO ANCHE SE
182200920212     C*  INCASSATO - BASTA CHE NON SIA MODIFICATO L'IMPORTO
182300941130    4C     SAVFCA        IFEQ      1
182400920212     C     §3AFCA        ANDEQ     2
182500920212     C                   EXSR      CONCSB
182600950630     C* Se da bolla singola a bolla doppia e C/A sulla 2 bolla non
182700950630     C* aggiorno adesso TNCSB ma solo dopo scrittura della 2 bolla su
182800950630     C* TNTAS
182900950630    5C     SAVTB2        IFEQ      '  '
183000950630     C     §3ATB2        ANDNE     '  '
183100950630     C                   UNLOCK    TNCSB03L                             99
183200950630     C                   ELSE
183300941130     C* ANAGRAFICA C/A TROVATA
183400950630    6C     *IN31         IFEQ      *OFF
183500941130     C* CREO STORICO SE MODIFICATO IMPORTO
183600950630    7C     CSBCAS        IFNE      ARBCAS
183700991103    7C     WCSBVCA       ORNE      ARBVCA
183800920212     C                   MOVEL     'VACA'        CSVCAV
183900920212     C                   EXSR      CSVWTR
184000950630    7C                   END
184100920212     C*
184200941130     C                   Z-ADD     9999          CSBKSC
184300941130     C                   MOVEL     TASLNA        CSBKSC
184400950202     C                   MOVEL     §3ATB2        CSBTBL
184500950202     C*
184600950202     C                   EXSR      AGGCSB
184700941130     C                   UPDATE    TNCSB000
184800950630    6C                   END
184900950630    5C                   END
185000941130    4C                   END
185100950202     C***************
185200941130    4C     SAVFCA        IFEQ      2
185300920212     C     §3AFCA        ANDEQ     1
185400920228     C                   EXSR      CHTB2
185500920212     C*
185600920228     C                   EXSR      CONCSB
185700941130     C*
185800941130    5C     *IN31         IFEQ      *OFF
185900920212     C* CREO STORICO SE MODIFICATO IMPORTO
186000950202    6C     ARBCAS        IFNE      CSBCAS
186100991102    6C     ARBVCA        ORNE      WCSBVCA
186200920212     C                   MOVEL     'VACA'        CSVCAV
186300920212     C                   EXSR      CSVWTR
186400941130    6C                   END
186500920212     C*
186600941130     C                   MOVEL     CSBCDI        CSBKSC
186700941130     C                   MOVEL     §3ATB1        CSBTBL
186800950202     C                   EXSR      AGGCSB
186900941130     C                   UPDATE    TNCSB000
187000941130    5C                   END
187100941130    4C                   END
187200950202     C***************
187300950202     C* MODIFICATO IMPORTO C/ASSEGNO
187400941130    4C     SAVFCA        IFEQ      1
187500920227     C     §3AFCA        ANDEQ     1
187600941130     C* CONTROLLO IL C/A
187700920227     C                   EXSR      CONCSB
187800941130     C*
187900950202    5C  N31CSBCAS        IFNE      ARBCAS
188000991103    5C     WCSBVCA       ORNE      ARBVCA
188100920227     C                   MOVEL     'VACA'        CSVCAV
188200920227     C                   EXSR      CSVWTR
188300941209    5C                   END
188400950202     C*
188500950202     C                   EXSR      AGGCSB
188600950202     C*
188700941209     C* IMPOSTO IL NUOVO TIPO BOLLA
188800941209     C                   MOVEL     §3ATB1        CSBTBL
188900941209     C  N31              UPDATE    TNCSB000
189000941130    4C                   END
189100950202     C***************
189200941130    4C     SAVFCA        IFEQ      2
189300920227     C     §3AFCA        ANDEQ     2
189400920228     C                   EXSR      CHTB2
189500920227     C*
189600920227     C                   EXSR      CONCSB
189700941130     C*
189800950202    5C  N31CSBCAS        IFNE      ARBCAS
189900991103    5C     WCSBVCA       ORNE      ARBVCA
190000920227     C                   MOVEL     'VACA'        CSVCAV
190100920227     C                   EXSR      CSVWTR
190200941209    5C                   END
190300950202     C*
190400950202     C                   EXSR      AGGCSB
190500950202     C*
190600941209     C* IMPOSTO IL NUOVO TIPO BOLLA
190700941209     C                   MOVEL     §3ATB2        CSBTBL
190800941209     C  N31              UPDATE    TNCSB000
190900941130    4C                   END
191000920212     C***
191100920227     C*** A  G  G  I  O  R  N  O       B  O  L  L  A
191200920212     C***
191300080408     c  N31              if        VariataSede=' '
191400941130     C                   CLEAR                   DESPAR
191500970916     C                   CLEAR                   DESPRV
191600920228     C***
191700920228     C*** DA BOLLA SINGOLA A BOLLA SINGOLA
191800941130    5C     SAVTB2        IFEQ      '  '
191900920227     C     §3ATB2        ANDEQ     '  '
192000110315     c
192100110315     c* Non lo faccio se devo aggiungere solo il C/Ass perchè la bolla
192200110315     c*  non la trovo col cod bolla vecchio ma col cod bolla nuovo
192300110315    6c                   if        SoloCAss=' '
192400110315     c
192500941130     C                   MOVEL     'N'           VARFCA            1
192600920228     C                   EXSR      AGGTB1
192700941130     C*
192800941130     C* IMPOSTO IL PAESE DI ARRIVO
192900941130     C                   MOVEL     TASLOD        DESPAR
193000990625     C                   MOVEL     TASPRD        DESPRV
193100110315    6C                   END
193200110315    5C                   END
193300920228     C***
193400920228     C*** DA BOLLA DOPPIA A BOLLA DOPPIA
193500941130    5C     SAVTB2        IFNE      '  '
193600920227     C     §3ATB2        ANDNE     '  '
193700920228     C* AGGIORNO 1 BOLLA
193800941130     C                   MOVEL     'N'           VARFCA            1
193900920228     C                   EXSR      AGGTB1
194000941130     C* AGGANCIO 2 BOLLA
194100941130     C                   EXSR      CHTB2
194200941130     C* IMPOSTO IL PAESE DI ARRIVO
194300941130     C                   MOVEL     TASLOD        DESPAR
194400990625     C                   MOVEL     TASPRD        DESPRV
194500920227     C*
194600941130     C   02
194700941130    6CANN29              DO
194800920227     C*
194900941130    7C   38SAVFCA        IFEQ      2
195000920228     C     §3AFCA        OREQ      2
195100990505     c* Se il C/A si trova sulla 2 bolla, li imposto, altrimenti lo azzero
195200990505     c*  per il controllo della varia G
195300990505     c                   if        §3afca=2
195400990505     c                   eval      wcas=arbcas
195500990505     c                   else
195600990505     c                   clear                   wcas
195700990505     c                   endif
195800990505     c
195900920228     C                   EXSR      VARIAG
196000941130    7C                   END
196100920227     C*
196200920227     C                   MOVEL     §3ATB2        TASTBL
196300990629     C                   MOVEL     arbcbn        TASCBO
196400011218
196500011218      * riorganizzo le varie se è necessario
196600011218     c                   if        flgriorg = '1'
196700011218     c                   exsr      sr_riorg
196800011218     c                   endif
196900011218
197000920227     C*
197100941201     C* ALTRIMENTI CAMBIO IL TIPO BOLAA ANCHE NEL TA7
197200990625     C     KBLP          SETLL     TiTA7000
197300990625     C     KBLP          READE     TiTA7000                               30
197400941201    8C     *IN30         DOWEQ     *OFF
197500941201     C                   MOVEL     §3ATB2        TA7TBL
197600990625     C                   UPDATE    TITA7000
197700990625     C     KBLP          READE     TITA7000                               30
197800941201    8C                   ENDDO
197900941130    6C                   END
198000920227     C*
198100990625     C   02              UPDATE    TITAS000
198200990625     C   03              UPDATE    TITAS010
198300990625     C   04              UPDATE    TITASP00
198400920227     C*
198500920227     C* ERRORI
198600941130     C  N02
198700941130     COR 02
198800941130    6CAN 29              DO
198900941130    7C     SAVTB2        IFNE      §3ATB2
199000130410     c* solo se tipo bolla nuovo non è uno storno
199100130410     c                   movel     'TB'          cod
199200130410     c                   movel(p)  §3atb2        key
199300130410     c                   clear                   dstb
199400130410     C     KTAB          CHAIN     TABEL
199500130410    8c                   if        %found(tabel00f)
199600130410     c                   movel     tbluni        dstb
199700130410    8c                   endif
199800130410    8c                   if        §tbfcb='1'
199900130326     c                   z-add     46            comfev
200000930513     C                   MOVEL     ERR(16)       ARBDER                          MODIF BOLLA
200100130410    8c                   endif
200200941130   X7C                   ELSE
200300920228     C*
200400130326     c                   z-add     46            comfev
200500930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF A3
200600941130    7C                   END
200700920228     C*
200800920227     C                   EXSR      WTRERR
200900941130    6C                   END
201000941130    5C                   END
201100920228     C***
201200920228     C*** DA BOLLA DOPPIA A BOLLA SINGOLA
201300941130    5C     SAVTB2        IFNE      '  '
201400920227     C     §3ATB2        ANDEQ     '  '
201500950630     C*
201600920228     C* AGGANCIO 2 BOLLA
201700920228     C                   EXSR      CHTB2
201800941130     C* IMPOSTO IL PAESE DI ARRIVO
201900941130     C                   MOVEL     TASLOD        DESPAR
202000990625     C                   MOVEL     TASPRD        DESPRV
202100920227     C*
202200941130     C   02
202300941130    6CANN29              DO
202400920227     C                   MOVEL     'AP'          TASTBL
202500991021     c                   movel     arbcbn        tascbo
202600941130     C*
202700941130     C* TOLGO FLAG C/A SOLO SE DALLA 2 BOLLA E' PASSATO ALLA 1
202800941130     C     SAVFCA        IFEQ      2
202900941130     C     §3AFCA        ANDEQ     1
203000941130     C                   MOVEL     ' '           TASFCA
203100941130     C                   ENDIF
203200941130     C*
203300990625     C                   UPDATE    TITAS000
203400941130    6C                   END
203500920227     C*
203600920227     C* ERRORI
203700990505    6C   29              DO
203800130326     c                   z-add     46            comfev
203900930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF BOLLA
204000920227     C                   EXSR      WTRERR
204100941130    6C                   END
204200920227     C**
204300920228     C** AGGIORNO 1 BOLLA
204400941130     C                   MOVEL     'S'           VARFCA
204500920228     C                   EXSR      AGGTB1
204600941130    5C                   END
204700920228     C***
204800920228     C*** DA BOLLA SINGOLA A BOLLA DOPPIA
204900941130    5C     SAVTB2        IFEQ      '  '
205000920227     C     §3ATB2        ANDNE     '  '
205100950630     C*
205200920228     C* AGGIORNO PRIMA BOLLA
205300941130     C                   MOVEL     'S'           VARFCA
205400920228     C                   EXSR      AGGTB1
205500920227     C*
205600000816     c* Prima di creare la seconda bolla cancello eventualmente bolla tipo
205700000816     c* "AP" che potrei avere in seguito ad un precedente cambio da bolla
205800000816     c* doppia a bolla singola
205900000816     c                   movel     'AP'          comtbl
206000000816     c                   setoff                                       020304
206100000816     c     kblp          chain     titas30c                           39
206200000816     c     *in39         ifeq      *off
206300000816     c     *in02         andeq     *on
206400000816     c     tasdft        ifeq      *zeros
206500000816     c     tasfic        oreq      ' '
206600000816     c                   delete    titas000
206700000816     c                   endif
206800000816     c                   endif
206900920227     C* CREO SECONDA BOLLA
207000920227     C*
207100950201     C* AGGANCIO FILE FNARBD1R
207200170109     c*  per sicurezza chaino bolla in FNBLP e aggiorno i dati
207300170109     c*  del destinatario da blp.
207400170109     C**   ARBORV        ADD       1             COMORV
207500170109     C**   KARBD         CHAIN     FNARBDRR                           32
207600170109     c
207700170109     C**N32              ADD       1             NUMRBD
207800920227     C* 32 ON - INESISTENTE: NON SCRIVO DESTINATARIO E SEGNALO ERRORE
207900170109    6C** 32              DO
208000170109     C**                 MOVEL     *BLANKS       ARBRSD
208100170109     C**                 MOVEL     *BLANKS       ARBRD2
208200170109     C**                 MOVEL     *BLANKS       ARBIND
208300170109     C**                 MOVEL     *BLANKS       ARBLOD
208400170109     C**                 MOVEL     *BLANKS       ARBCAD
208500170109     C**                 MOVEL     *BLANKS       ARBPRD
208600170109     C**                 MOVEL     *BLANKS       ARBNZD
208700920227     C*
208800170109     C**                 MOVEL     ERR(12)       ARBDER
208900170109     C**                 Z-ADD     102           COMFEV
209000170109     C**                 EXSR      WTRERR
209100170109    6C***                END
209200920227     C*
209300920227     C*
209400941130     C                   MOVEL     §3ATB2        TASTBL
209500990629     C                   MOVEL     ARBCBN        TASCBO
209600941130     C                   MOVE      TASKSC        KSC4
209700941130     C*
209800941130     C                   CLEAR                   TASRSD
209900941130     C                   CLEAR                   TASIND
210000990625     C                   CLEAR                   TASCAd
210100941130     C                   CLEAR                   TASLOD
210200990625     C                   CLEAR                   TASPRd
210300990625     C                   CLEAR                   TAScpd
210400170109     c
210500170109     c     kann          chain     fnblp01l
210600941130     C*
210700941130     C* MUOVO L'INIDIRIZZO COMPLETO DEL DESTINATARIO
210800170109     c                   if        %found(fnblp01l)
210900170109     C                   MOVEL     blpRSD        TASRSD
211000170109     C                   MOVEL     blpIND        TASIND
211100170109     C                   MOVEL     blpCAD        TASCAD
211200170109     C                   MOVEL     blpLOD        TASLOD
211300170109     C                   MOVEL     blpLOD        DESPAR
211400170109     C                   MOVEL     blpPRD        DESPRV
211500170109     C                   MOVEL     blpPRD        TASPRD
211600170109     C                   MOVEL     blpNZD        TASNZD
211700990701     c* Muovo comunque la p.iva in tascpd anche se in questo caso
211800990701     c* sarà sempre vuota perchè visto che prima della variazione
211900990701     c* la bolla era franco non poteva esserci in filiale la p.iva
212000990701     c* del destinatario memoziazzata su fnarb
212100170109     C*********          MOVEL     ARBCPI        TASCPD
212200170109     c                   endif
212300941130     C*
212400941130     C                   CLEAR                   TASPVL
212500941130     C                   CLEAR                   TASFTC
212600950202     C                   CLEAR                   TASTC2
212700941130     C                   CLEAR                   TASIAS
212800990824     C                   CLEAR                   TASFCM
212900950202     C                   CLEAR                   TASVAS
213000941130     C                   MOVEL     *ZEROS        TASCTR
213100930604     C                   MOVEL     *BLANKS       TASFTM
213200991018     C                   MOVEL     *BLANKS       TASDIV
213300920227     C                   MOVE      *ZERO         TASPOR
213400920227     C                   MOVE      *BLANK        TASSV1
213500920227     C                   MOVE      *ZERO         TASVA1
213600920227     C                   MOVE      *BLANK        TASSV2
213700920227     C                   MOVE      *ZERO         TASVA2
213800930212     C                   MOVE      *BLANK        TASSV3
213900930212     C                   MOVE      *ZERO         TASVA3
214000920227     C                   Z-ADD     0             TASDFT
214100920227     C                   Z-ADD     0             TASNFT
214200920227     C                   Z-ADD     0             TASFIV
214300930212     C                   MOVE      ' '           TASFA2
214400920227     C                   Z-ADD     9999          TASKSC
214500920227     C                   MOVEL     TASLNA        TASKSC
214600920227     C* CONTRASSEGNO A SECONDA DEL FLAG
214700941130    6C     §3AFCA        IFEQ      2
214800941130     C                   MOVEL     'S'           TASFCA
214900920227     C                   MOVEL     §3ATB2        TBLCA             2
215000920227     C                   ELSE
215100941130     C                   MOVEL     ' '           TASFCA
215200941130    6C                   END
215300920227     C*
215400990624     C                   MOVE      *zeros        TASimv
215500941130     C                   MOVEL     ' '           TASFEI
215600941130     C                   MOVEL     ' '           TASFTS
215700941130     C                   MOVEL     ' '           TASFIC
215800920227     C*
215900990625     C                   WRITE     TITAS000                             99
216000950630     C*
216100950630     C     SAVFCA        IFEQ      1
216200950630     C     §3AFCA        ANDEQ     2
216300950630     C     KANN          CHAIN     TNCSB000                           31
216400991103     c                   move      csbvca        wcsbvca
216500991103     c     csbcas        ifgt      0
216600991103     c     wcsbvca       andeq     *blanks
216700991103     c                   movel     'ITL'         wcsbvca
216800991103     c                   endif
216900950630     C     CSBCAS        IFNE      ARBCAS
217000991103     C     wcsbvca       ORNE      ARBVCA
217100950630     C                   MOVEL     'VACA'        CSVCAV
217200950630     C                   EXSR      CSVWTR
217300950630     C                   END
217400950630     C*
217500950630     C                   Z-ADD     9999          CSBKSC
217600950630     C                   MOVEL     TASLNA        CSBKSC
217700950630     C                   MOVEL     §3ATB2        CSBTBL
217800950630     C*
217900950630     C                   EXSR      AGGCSB
218000950630     C                   UPDATE    TNCSB000
218100950630     C                   END
218200920227     C*
218300941130     C* REIMPOSTO ORA DI VARIAZIONE DI ARBK: ORA DI ARBD - 1
218400170110     C* N32              SUB       1             ARBORV
218500170110     C* N32              DELETE    FNARBDRR
218600941130    5C                   END
218700920227     C*
218800920227     C* CREO SE NECESSARIO RECORD IN FILE CONTRASSEGNI
218900941130    5C     SAVFCA        IFEQ      0
219000941130    6C     §3AFCA        IFEQ      1
219100920227     C                   MOVEL     §3ATB1        TBLCA
219200941130     C* RICHAINO LA BOLLA
219300941130     C                   MOVEL     §3ATB1        COMTBL
219400941130     C                   SETOFF                                       020304
219500990625     C     KBLP          CHAIN     TITAS30C                           30
219600110315     c* Se solo C/Ass--> aggiungo il flag
219700110315     c                   if        SoloCAss='S'
219800110315     c                   eval      TASFCA='S'
219900110315     C                   MOVEL     TASLOD        DESPAR
220000110315     C                   MOVEL     TASPRD        DESPRV
220100110315     C   02              UPDATE    TITAS000
220200110315     C   03              UPDATE    TITAS010
220300110315     C   04              UPDATE    TITASP00
220400110315     c                   endif
220500950913     C*
220600920227     C                   EXSR      CRTCSB
220700941130     C*
220800920227     C                   ELSE
220900941130     C*
221000941130    7C     §3AFCA        IFEQ      2
221100920227     C                   MOVEL     §3ATB2        TBLCA
221200941130     C* RICHAINO LA BOLLA
221300941130     C                   MOVEL     §3ATB2        COMTBL
221400941130     C                   SETOFF                                       020304
221500990625     C     KBLP          CHAIN     TITAS30C                           30
221600920227     C                   EXSR      CRTCSB
221700941130    7C                   END
221800941130    6C                   END
221900941130    5C                   END
222000920227     C*
222100941130    4C                   END
222200941130    3C                   END
222300941130    2C                   END
222400930513     C*
222500930513     C* 11 ON - RECORD DESTINATARIO PER CREARE 2 BOLLA
222600941130   X1C                   ELSE
222700950202     C* NON CANCELLO IL RECORD : IN ARBM LO LASCIO LI'
222800950202     C                   SETON                                        37
222900950202     C*
223000950202     C     *IN11         IFEQ      *ON
223100930513     C                   MOVEL     102           COMFEV
223200930513     C                   MOVEL     ERR(15)       ARBDER
223300930513     C                   EXSR      WTRERR
223400950202     C                   ENDIF
223500950202    1C                   ENDIF
223600920227     C*
223700920227     C                   ENDSR
223800060317     C**---------------------------------------------------------
223900060317     C** AGGIORNO CODICE VARIAZIONE RM
224000060317     C**---------------------------------------------------------
224100060317     C     AGARBR        BEGSR
224200060317     c* rma in tita4 record "A"
224300060317     c                   setoff                                       0506
224400060317     C                   movel     'A'           §trc
224500060317     C     ktita         chain     tita430c                           32
224600060317    3C     *in32         ifeq      *off
224700060317     C                   movel     ta4not        dta4a
224800060317     c                   movel     arbrsd        §TA4ARMA
224900160121     C                   MOVEL     ARBrd2        §TA4anas
225000060317     C                   movel     dta4a         ta4not
225100060317     C   05              update    tita4000
225200060317     C   06              update    tita4p00
225300060317     c                   endif
225400060317     c*
225500160412     C****               MOVEL     ARBrd2        TASnas
225600160412     C** 02              UPDATE    TiTAS000
225700160412     C** 03              UPDATE    TiTAS010
225800160412     C** 04              UPDATE    TiTASP00
225900160412     c**
226000160412     c                   unlock    titas30c
226100060317     c*
226200060317     C                   ENDSR
226300920228     C**---------------------------------------------------------
226400920228     C** AGGIORNO PRIMA BOLLA
226500920228     C**---------------------------------------------------------
226600920228     C     AGGTB1        BEGSR
226700920228     C* AGGANCIO 1 BOLLA
226800920228     C                   EXSR      CHTB1
226900920228     C*
227000941201     C* SE E' DA AGGIUNGERE LO AGGIUNGO
227100920228     C     §3AFCA        IFEQ      1
227200941130     C                   MOVEL     'S'           TASFCA
227300941201     C*
227400920228     C                   ELSE
227500941201     C* LO TOLGO SOLO SE FALG IMPOSTATO = "S"
227600941201     C*
227700941201     C     VARFCA        IFEQ      'S'
227800941130     C                   MOVEL     ' '           TASFCA
227900920228     C                   END
228000941130     C                   END
228100920228     C*
228200941130     C   02
228300941130     CANN29              DO
228400920228     C   38SAVFCA        IFEQ      1
228500920228     C     §3AFCA        OREQ      1
228600990505     c* Devo impostare l'importo C/A nella routine varia G :
228700990505     c*  se il C/A e' stato tolto dalla 1 bolla, richiamo con importo=0
228800990505     c                   if        §3afca=1
228900990505     c                   eval      wcas=arbcas
229000990505     c                   else
229100990505     c                   clear                   wcas
229200990505     c                   endif
229300990505     c
229400920228     C                   EXSR      VARIAG
229500920228     C                   END
229600920228     C*
229700920228     C                   MOVEL     §3ATB1        TASTBL
229800990629     C                   MOVEL     ARBCBN        TASCBO
229900011219
230000011219      * riorganizzo le varie se è necessario
230100011219     c                   if        flgriorg = '1'
230200011219     c                   exsr      sr_riorg
230300011219     c                   endif
230400011219
230500000209     C* AGGIORNO TITA7000 SE ESISTE
230600000209     C     KBLP          SETLL     TITA7000
230700000209     C     KBLP          READE     TITA7000                               30
230800000209     C*
230900000209     C     *IN30         DOWEQ     *OFF
231000000209     C                   MOVEL     §3ATB1        TA7TBL
231100000209     C                   UPDATE    TITA7000
231200000209     C     KBLP          READE     TITA7000                               30
231300000209     C                   ENDDO
231400920228     C                   END
231500941130     C*
231600920228     C*
231700990625     C   02              UPDATE    TITAS000
231800990625     C   03              UPDATE    TITAS010
231900990625     C   04              UPDATE    TITASP00
232000920228     C*
232100990505     C* spedizione gia' contabilizzata
232200990505     C   29              DO
232300920228     C*
232400920312     C                   MOVEL     *BLANKS       ARBDER
232500920228     C     §3ATB1        IFNE      SAVTB1
232600130410     c* solo se tipo bolla nuovo non è uno storno
232700130410     c                   movel     'TB'          cod
232800130410     c                   movel(p)  §3atb1        key
232900130410     c                   clear                   dstb
233000130410     C     KTAB          CHAIN     TABEL
233100130410    3c                   if        %found(tabel00f)
233200130410     c                   movel     tbluni        dstb
233300130410    3c                   endif
233400130410    3c                   if        §tbfcb='1'
233500130326     c                   z-add     46            comfev
233600930513     C                   MOVEL     ERR(16)       ARBDER                         MODIF BOLLA
233700130410    3c                   endif
233800920228     C                   ELSE
233900920228     C*
234000920228     C     SAVFCA        IFNE      1
234100920228     C     §3AFCA        ANDEQ     1
234200130325     C***                MOVEL     ERR(18)       ARBDER                         AGGIUN VARIA
234300130325     c***                setoff                                       36
234400120423     c                   exsr      ctr_vrb
234500920228     C                   END
234600920228     C*
234700920228     C     SAVFCA        IFEQ      1
234800920228     C     §3AFCA        IFNE      1
234900941130     C*
235000941130     C* CONTROLLO SE ESISTE VARIA "G"
235100130326     C***                EXSR      ESVARG
235200130325     C***  WSIVAR        IFEQ      'S'
235300130325     C***                MOVEL     ERR(14)       ARBDER                         TOLTA VARIA
235400130325     c***                setoff                                       36
235500130325     C***                END
235600121205     c                   eval      wanncas='S'
235700121203     c                   exsr      ctr_vrb
235800941130     C*
235900920228     C                   ELSE
236000130325     C****               MOVEL     ERR(22)       ARBDER                         MODIF VARIA
236100130325     c****               setoff                                       36
236200120423     c                   exsr      ctr_vrb
236300920228     C                   END
236400920228     C                   END
236500920228     C                   END
236600920228     C*
236700920312     C     ARBDER        IFNE      *BLANKS
236800920228     C                   EXSR      WTRERR
236900920228     C                   END
237000920312     C                   END
237100920228     C                   ENDSR
237200920227     C**---------------------------------------------------------
237300920227     C** CALCOLO O ANNULLO VARIA  G
237400120322     C**---------------------------------------------------------
237500920227     C     VARIAG        BEGSR
237600920228     C                   MOVE      TASKSC        KSC4
237700990505     c                   clear                   ARBDER
237800011218
237900011218     c                   clear                   flgriorg
238000941130     C*
238100990414     c* Se data fattura a zero e cliente codificato vado a togliere la varia G
238200990505     C     TASDFT        IFEQ      *ZEROS
238300990414    1C     KSC4          ANDNE     9999
238400920303     C     KSC4          ANDNE     8888
238500920228     C*
238600990505     c* C/A tolto
238700990505     c                   if        wcas=0
238800941130     C                   SELECT
238900941130     C     TASSV1        WHENEQ    'G'
239000011218     c                   eval      flgriorg = '1'
239100991019     c     tasimv        ifeq      tasva1
239200991019     c                   clear                   tasdiv
239300991019     c                   endif
239400920227     C                   MOVEL     ' '           TASSV1
239500920227     C                   Z-ADD     0             TASVA1
239600990624     C                   MOVE      *zeros        TASimv
239700941130     C     TASSV2        WHENEQ    'G'
239800011218     c                   eval      flgriorg = '1'
239900991019     c     tasimv        ifeq      tasva2
240000991019     c                   clear                   tasdiv
240100991019     c                   endif
240200920227     C                   MOVEL     ' '           TASSV2
240300920227     C                   Z-ADD     0             TASVA2
240400990624     C                   MOVE      *zeros        TASimv
240500941130     C     TASSV3        WHENEQ    'G'
240600011218     c                   eval      flgriorg = '1'
240700991019     c     tasimv        ifeq      tasva3
240800991019     c                   clear                   tasdiv
240900991019     c                   endif
241000930212     C                   MOVEL     ' '           TASSV3
241100930212     C                   Z-ADD     0             TASVA3
241200990624     C                   MOVE      *zeros        TASimv
241300941130     C*
241400941130     C                   OTHER
241500941130     C*
241600941130     C                   MOVEL     'G'           WSV1
241700990625     C     KBLPV         DELETE    TITA7000                           30
241800990624     C  n30              MOVE      *zeros        TASimv
241900941130     C                   ENDSL
242000990419     c                   else
242100990419     c* C/A aggiunto o midificato
242200990419     c                   EXSR      ESVARG
242300990419     c* Se la varia non c'e' tolto totale imponibile cosi' la calcola
242400990419     c                   if        wsivar=' '
242500990624     c                   clear                   TASimv
242600990419     c                   else
242700990419     c* Se gia' esiste stampo errore --> no modificata varia G
242800130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
242900130326     C***                EXSR      WTRERR
243000130326     c***                eval      wcntab=' '
243100130326     c***                exsr      ctr_vrb
243200990419     c                   endif
243300990419     c                   endif
243400920228     C*
243500941130   X1C                   ELSE
243600990414     C* CLIENTE NON CODIFICATO O CON DATA FATTURA VALORIZZATA
243700991019     C* COMUNQUE SE ANNULLATO C/A ELIMINO VARIA G  E ADEGUO TOT.IMPONIBILE
243800990414     C* Se c'è importo del contrassegno e la data fattura è uguale a zero posso
243900990414     c* togliere la varia "G"
244000990505    2C     WCAS          IFEQ      0
244100990505     C     TASDFT        ANDEQ     0
244200990414     c*
244300941130    3C                   SELECT
244400941130     C     TASSV1        WHENEQ    'G'
244500011218     c                   eval      flgriorg = '1'
244600991019     c                   sub       tasva1        tasimv
244700991019     c     tasimv        ifeq      0
244800991019     c                   clear                   tasdiv
244900991019     c                   endif
245000920228     C                   MOVEL     ' '           TASSV1
245100920228     C                   Z-ADD     0             TASVA1
245200941130     C     TASSV2        WHENEQ    'G'
245300011218     c                   eval      flgriorg = '1'
245400991019     c                   sub       tasva2        tasimv
245500991019     c     tasimv        ifeq      0
245600991019     c                   clear                   tasdiv
245700991019     c                   endif
245800920228     C                   MOVEL     ' '           TASSV2
245900920228     C                   Z-ADD     0             TASVA2
246000941130     C     TASSV3        WHENEQ    'G'
246100011218     c                   eval      flgriorg = '1'
246200991019     c                   sub       tasva3        tasimv
246300991019     c     tasimv        ifeq      0
246400991019     c                   clear                   tasdiv
246500991019     c                   endif
246600930212     C                   MOVEL     ' '           TASSV3
246700930212     C                   Z-ADD     0             TASVA3
246800941130     C                   OTHER
246900941130     C                   MOVEL     'G'           WSV1
247000991019     C     KBLPV         chain     TITA7000                           30
247100991019     c  n30              sub       ta7van        tasimv
247200991019     c  n30              delete    tita7000
247300941130    3C                   ENDSL
247400920228     C*
247500941130   X2C                   ELSE
247600990414     c*
247700990414     c* Se è già stato fatturato oppure arbcas > 0
247800920228     C*
247900941130     C* VEDO SE LA VARIA "G" C'ERA
248000130326     C*****              EXSR      ESVARG
248100941130     C*
248200991019     c* se c'è  l'importo del contrassegno per non codificati
248300991019     c*                                      o data fattura maggiore di zero
248400130326    3c***  wcas          ifgt      *zeros
248500990414     c*
248600130326    4C***  WSIVAR        IFEQ      ' '
248700130326     C***                MOVEL     ERR(24)       ARBDER                         AGGIUN VARIA
248800130326     C***                EXSR      WTRERR
248900130326     c***                eval      wcntab=' '
249000130326     c***                exsr      ctr_vrb
249100130326     C***                ELSE
249200130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
249300130326     C***                EXSR      WTRERR
249400130326     c***                eval      wcntab=' '
249500130326     c***                exsr      ctr_vrb
249600130326    4C***                END
249700130326    3c****               endif
249800990505     c*
249900990414     c* se l'importo è uguale a zero e prima c'era la varia "G"
250000130326    3c***  wcas          ifeq      *zeros
250100130326     c***  wsivar        andne     ' '
250200130326     C***                MOVEL     ERR(23)       ARBDER                          no tolta varia
250300130326     C***                EXSR      WTRERR
250400130326    3C***                END
250500941130    2C                   END
250600941130    1C                   END
250700920227     C*
250800920227     C                   ENDSR
250900011218      *----------------------------------------------------------
251000011218      * Riorganizzo le varie sul titas
251100011218      *----------------------------------------------------------
251200011218     c     sr_riorg      begsr
251300011218
251400011218     c                   clear                   sksv
251500011218     c                   clear                   skva
251600011218     c                   eval      xx = 1
251700011218
251800011219      * carico la sk delle varie dal titas
251900011218     c                   if        tassv1 <> *blanks
252000011218     c                   eval      sksv(xx) = tassv1
252100011218     c                   eval      skva(xx) = tasva1
252200020107     c                   clear                   tassv1
252300020107     c                   clear                   tasva1
252400011218     c                   add       1             xx
252500011218     c                   endif
252600011218     c                   if        tassv2 <> *blanks
252700011218     c                   eval      sksv(xx) = tassv2
252800011218     c                   eval      skva(xx) = tasva2
252900020107     c                   clear                   tassv2
253000020107     c                   clear                   tasva2
253100011218     c                   add       1             xx
253200011218     c                   endif
253300011218     c                   if        tassv3 <> *blanks
253400011218     c                   eval      sksv(xx) = tassv3
253500011218     c                   eval      skva(xx) = tasva3
253600020107     c                   clear                   tassv3
253700020107     c                   clear                   tasva3
253800011218     c                   add       1             xx
253900011218     c                   endif
254000011218
254100011219      * carico la sk delle varie dal tita7
254200011219     c     kblp          setll     tita7000
254300011218     c                   do        *hival
254400011219     c     kblp          reade     tita7000
254500011219     c                   if        %eof
254600011218     c                   leave
254700011219     c                   endif
254800011218     c                   eval      skva(xx) = ta7van
254900011218     c                   eval      sksv(xx) = ta7svn
255000011218     c                   add       1             xx
255100011219     c                   delete    tita7000
255200011218     c                   enddo
255300011219
255400011219      * scrivo le varie su titas
255500011219     c                   if        sksv(1) <> *blanks
255600011219     c                   eval      tassv1 = sksv(1)
255700011219     c                   eval      tasva1 = skva(1)
255800011219     c                   endif
255900011219     c                   if        sksv(2) <> *blanks
256000011219     c                   eval      tassv2 = sksv(2)
256100011219     c                   eval      tasva2 = skva(2)
256200011219     c                   endif
256300011219     c                   if        sksv(3) <> *blanks
256400011219     c                   eval      tassv3 = sksv(3)
256500011219     c                   eval      tasva3 = skva(3)
256600011219     c                   endif
256700011219
256800011219      * scrivo le varie su tita7
256900011219     c     4             do        20            xx
257000011219     c                   if        sksv(xx) <> *blanks
257100011219     c                   eval      ta7aas = arbaas
257200011219     c                   eval      ta7lnp = arblnp
257300011219     c                   eval      ta7nrs = arbnrs
257400011219     c                   eval      ta7nsp = arbnsp
257500011219     c                   eval      ta7tbl = comtbl
257600011219     c                   eval      ta7svn = sksv(xx)
257700011219     c                   eval      ta7van = skva(xx)
257800011219     c                   write     tita7000
257900011219     c                   endif
258000011219     c                   enddo
258100011218
258200011218     c                   endsr
258300011218
258400920210     C**---------------------------------------------------------
258500920210     C** CREO RECORD CONTRASSEGNO
258600920210     C**---------------------------------------------------------
258700920210     C     CRTCSB        BEGSR
258800941130     C                   MOVEL     ARBDER        SAVDER
258900920303     C*
259000920303     C* SE ESISTE ANNULLATO LO DELETO E RISCRIVO
259100941130     C     KANN          CHAIN     TNCSB000                           41
259200941130    1C     *IN41         IFEQ      *ON
259300941130     C                   CLEAR                   TNCSB000
259400941130     C                   ELSE
259500941130     C     KANN          CHAIN     TNCSA000                           39
259600941130     C  N39              DELETE    TNCSA000
259700941130    1C                   ENDIF
259800920303     C*
259900950201     C* AGGANCIO FILE FNARBM0R
260000170109     C***  KARBM         CHAIN     FNARBMRR                           35
260100170109     C**N35              ADD       1             NUMRBM            7 0
260200170109     c** non utilizzo più il mittente da trasmissione in FNARBM
260300170109     c*  ma aggancio direttamente l'anagrafica mittente in TITAA30C
260400170109     c
260500920212     C* INESISTENTE: STAMPO MA CREO UGUALMENTE ARCHIVIO (DA COMPLETARE)
260600170109    1C** 35              DO
260700170109     C**                 MOVEL     ERR(11)       ARBDER
260800170109     C**                 Z-ADD     102           COMFEV
260900170109     C**                 EXSR      WTRERR
261000920212     C* AZZERO CAMPI MITTENTE
261100170109     C**                 MOVEL     *BLANKS       ARBRSM
261200170109     C**                 MOVEL     *BLANKS       ARBINM
261300170109     C**                 MOVEL     *BLANKS       ARBCAM
261400170109     C**                 MOVEL     *BLANKS       ARBLOM
261500170109     C**                 MOVEL     *BLANKS       ARBPRD
261600170109     C**                 MOVEL     *ZEROS        ARBCCM
261700170109    1C**                 END
261800920212     C*
261900920210     C                   MOVEL     TASLNP        CSALNP
262000920210     C                   MOVEL     TASNRS        CSANRS
262100920210     C                   MOVEL     TASAAS        CSAAAS
262200920210     C                   MOVEL     TASNSP        CSANSP
262300920210     C*
262400920210     C                   MOVEL     TBLCA         CSBTBL
262500920210     C                   Z-ADD     TASLNP        CSBLNP
262600920210     C                   Z-ADD     TASAAS        CSBAAS
262700920210     C                   Z-ADD     TASNRS        CSBNRS
262800920210     C                   Z-ADD     TASNSP        CSBNSP
262900941130     C                   Z-ADD     TASMGS        CSBMGS
263000990629     C                   Z-ADD     TASTFP        CSBFLE
263100941130     C                   MOVEL     DESPAR        CSBPAR                         PAESE ARRIVO
263200970916     C                   MOVEL     DESPRV        CSBPRO                         PROV.DEST.
263300030310     C                   MOVE      *ZERO         CSBSTA
263400950202     C                   EXSR      AGGCSB
263500920304     C*
263600941130     C                   MOVEL     TBLCA         FLGTB1
263700920304     C*
263800920304     C* SE BLANK E CODIFICATO DESTINATARIO CHIAN
263900941130    2C     TASRSD        IFEQ      *BLANKS
264000990625     c* N.B.la call al tibs69 sarà da togliere quando tutte le bolla avranno
264100990625     c* il destinatario pieno
264200990625     c     flgtb1        ifeq      'A'
264300981001     C                   CLEAR                   DSBS69
264400981001     C                   MOVEL     KCI           I69KCC
264500981001     C                   MOVEL     KNSIF         I69SIF
264600981001     C                   MOVEL     TASKSC        I69KAC
264700981001     C                   CALL      'TIBS69R'
264800981001     C                   PARM                    DSBS69
264900981001     C                   PARM                    DSACO
265000981001     C                   PARM                    DSIND
265100981001     C                   PARM                    DSCLP
265200981001     C                   PARM                    DSCLS
265300981001     C**
265400981001     C     O69ERR        IFEQ      ' '
265500981001     C                   MOVEL     ACORAG        CSBRSD
265600981001     C                   ELSE
265700981001     C                   MOVEL     *BLANKS       CSBRSD
265800981001     C                   ENDIF
265900990625     C                   ENDIF
266000981001     C                   ELSE
266100981001     C                   MOVEL     TASRSD        CSBRSD
266200941130    2C                   END
266300920304     C*
266400920304     C*
266500920210     C                   Z-ADD     TASRMN        CSBRMN
266600990625     c* prendo il riferimento mittente alfabetico da estensione (tita4)
266700990625     c                   movel     'A'           §trc
266800010126     c     ktita         chain(N)  tita430c                           39
266900990625     c  n39              movel     ta4not        csbrma
267000990625     c   39              clear                   csbrma
267100941207     C                   Z-ADD     TASLNA        CSBLNA
267200040721     C**                   MOVEL     TASFIN        CSBFIN
267300920210     C                   Z-ADD     TASKSC        CSBKSC
267400920210     C                   MOVE      CSBKSC        CSBCDI
267500931108     C   41              MOVEL     'R'           CSBFTR
267600931108     C   41              MOVE      DATEU         CSBDTR
267700920303     C*
267800941130    1C     FLGTB1        IFEQ      'A'
267900941130     C                   Z-ADD     99999         CSBKSC
268000941130     C                   MOVEL     TASLNA        CSBKSC
268100941206     C                   MOVE      CSBKSC        CSBCDI                         WB/LB
268200941206    1C                   END                                                    WB/LB
268300941130     C*
268400170109     C* VEDO SE tasccm   e'GENERICo
268500170109     C                   MOVE      tasccm        W0040             4 0
268600170111     c                   if        w0040 <>8888 and w0040 <>9999
268700941130     C                   MOVEL     TASCCM        CSBCDI
268800170109     c                   endif
268900170109     c
269000941206    1C*************        END                             WB/LB
269100941130     C*
269200920210     C** RAGGRUPPAMENTO C/ASSEGNI
269300920210     C     TASLNA        CHAIN     AZORG                              51
269400920210     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
269500920210     C   51              Z-ADD     TASLNA        CSBRGF
269600941130     C*
269700941130     C* VEDO SE CSBCDI E' INTESTAZIONE GENERICA
269800941130     C                   MOVE      CSBCDI        W0040             4 0
269900920210     C*
270000920210     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
270100941130     C     W0040         IFEQ      8888
270200941130     C     W0040         OREQ      9999
270300170109     C**                 MOVEL     ARBRSM        CSARSD                         MITTENTE
270400170109     C**                 MOVEL     ARBINM        CSAVID                            '
270500170109     C**                 MOVEL     ARBCAM        CSACAD                            '
270600170109     C**                 MOVEL     ARBLOM        CSACID                            '
270700170109     C**                 MOVEL     ARBPRM        CSAPRM
270800170109     C**                 MOVEL     ARBNZM        CSANZM
270900170109
271000170109     c                   movel     'M'           §trc
271100170109     c     ktita         chain(N)  titaa30c
271200170109     c                   if        %found(titaa30c)
271300170109     C                   MOVEL     taarsc        CSARSD                         MITTENTE
271400170109     C                   MOVEL     taaind        CSAVID                            '
271500170109     C                   MOVEL     taacap        CSACAD                            '
271600170109     C                   MOVEL     taaloc        CSACID                            '
271700170109     C                   MOVEL     taaprv        CSAPRM
271800170109     C                   MOVEL     taanaz        CSANZM
271900170109     c                   else
272000170109     c                   clear                   csarsd
272100170109     c                   clear                   csavid
272200170109     c                   clear                   csacid
272300170109     c                   clear                   csaprm
272400170109     c                   clear                   csanzm
272500170109     c                   endif
272600920212     C*
272700941201     C                   WRITE     TNCSA000                             99
272800920212     C                   END
272900920212     C**
273000941201     C   41              WRITE     TNCSB000                             99
273100941130     C  N41              UPDATE    TNCSB000
273200950914     C*
273300950914     C* CREO RECORD DI INSERIMENTO C/ASSEGNO NELLE VARIAZIONI
273400950914     C                   MOVEL     'IMCA'        CSVCAV
273500950914     C                   EXSR      CSVWTR
273600941130     C*
273700941130     C** SE ERA IMPOSTATO LO RIMETTO COME ERA PRIMA
273800941130     C                   MOVEL     SAVDER        ARBDER
273900920210     C                   ENDSR
274000920210     C**---------------------------------------------------------
274100920210     C** CONTROLLO ESISTENZA ARCHIVIO CONTRASSEGNI
274200920210     C**---------------------------------------------------------
274300920210     C     CONCSB        BEGSR
274400930513     C                   SETOFF                                           42    25
274500920212     C* VARIO  CONTRASSEGNO SOLO SE NON PAGATO-INCASSATO
274600941130     C     KANN          CHAIN     TNCSB000                           31
274700930303     C*
274800941130     C   31KANN          SETLL     FNLBL01L                               42
274900920212     C*
275000930513     C* PER CASUALE VARIAZIONE "K" SEMPRE ERRORE
275100930303     C   31FLGCVB        IFEQ      'K'
275200930513     C                   SETON                                        25
275300930303     C                   MOVEL     ERR(5)        ARBDER
275400930303     C                   Z-ADD     102           COMFEV
275500930303     C                   EXSR      WTRERR
275600930513     C*
275700930303     C                   ELSE
275800930513     C*
275900930513     C* ALTRIMENTI SOLO SE NON C'E' IL LEGAME BOLLA
276000930513     C     *IN42         IFEQ      '0'
276100930513     C                   SETON                                        25
276200930513     C                   MOVEL     ERR(5)        ARBDER
276300930513     C                   Z-ADD     102           COMFEV
276400930513     C                   EXSR      WTRERR
276500930303     C                   END
276600930513     C                   END
276700991103     c*
276800991103     c     *in31         ifeq      *off
276900991103     c                   movel     csbvca        wcsbvca
277000991103     c     csbcas        ifgt      0
277100991103     c     csbvca        andeq     *blanks
277200991103     c                   movel     'ITL'         wcsbvca
277300991103     c                   endif
277400991103     c                   endif
277500930513     C*
277600920302     C* GIA' INCASSATO O PAGATO
277700950202    1C  N31CSBDDC        IFNE      0
277800920210     C     CSBBNA        ORNE      0
277900920212     C*
278000950324     C* SE NON MODIFICATO L'IMPORTO DEL C/A OK, SE VARIAZIONE "K"
278100950324     C     FLGCVB        IFEQ      'K'
278200950202    3C     ARBCAS        IFNE      CSBCAS
278300991102     C     ARBVCA        ORNE      WCSBVCA
278400920212     C                   SETON                                        31
278500110214     C                   Z-ADD     102           COMFEV
278600950202    2C                   END
278700110214     c
278800950324     C* ALTRIMENTI SEMPRE ERRORE
278900950324     C                   ELSE
279000950324     C                   SETON                                        31
279100110214     C                   Z-ADD     102           COMFEV
279200950324    2C                   END
279300920601     C*
279400950202    2C   31FLGCVB        IFNE      'I'
279500950222    2C     FLGCVB        ANDNE     'M'
279600170110     C***                SETON                                        36
279700920601     C                   MOVEL     ERR(10)       ARBDER
279800920601     C                   EXSR      WTRERR
279900950324     C                   ELSE
280000950324     C                   CLEAR                   COMFEV
280100950202    2C                   END
280200950202    1C                   END
280300920210     C                   ENDSR
280400950202     C*
280500950202     C**---------------------------------------------------------
280600950202     C** AGGIORNO RECORD CONTRASSEGNO
280700950202     C**---------------------------------------------------------
280800950202     C     AGGCSB        BEGSR
280900950202     C                   Z-ADD     ARBCAS        CSBCAS
281000950202     C                   MOVEL     ARBVCA        CSBVCA
281100950202     C                   MOVEL     ARBGCA        CSBGCA
281200950203     C                   Z-ADD     ARBCMB        CSBCMB
281300030310     c                   if        csbsta <> 3
281400000703     c                   movel     0             csbsta
281500030310     c                   end
281600950202     C* TIPO ASSEGNO: MITTENTE / BARTOLINI
281700950202     C                   MOVEL     '1A'          COD
281800950202     C                   MOVEL(P)  ARBTIC        KEY
281900950202     C     KTAB          CHAIN     TABEL                              30
282000950202     C  N30              MOVEL     TBLUNI        DS1A
282100950202     C  N30              MOVEL     §1AFMB        CSBFUS
282200950202     C   30              CLEAR                   CSBFUS
282300950202     C                   ENDSR
282400920212     C**---------------------------------------------------------
282500920212     C** ANNULLO RECORD CONTRASSEGNO
282600920212     C**---------------------------------------------------------
282700920212     C     ANNCSB        BEGSR
282800941209     C* CREO STORICO VARIAZIONE
282900920602     C     CSBSTA        IFNE      9
283000920212     C                   MOVEL     'ANCA'        CSVCAV
283100920212     C                   EXSR      CSVWTR
283200920212     C* ANNULLO
283300920212     C                   MOVEL     9             CSBSTA
283400941130     C                   UPDATE    TNCSB000
283500920212     C                   END
283600920212     C                   ENDSR
283700920212     C**---------------------------------------------------------
283800920212     C** ESEGUE WRITE BLCSV00F                                  -
283900920212     C**---------------------------------------------------------
284000920212     C     CSVWTR        BEGSR
284100950202     C* COME DATA E ORA VARIAZIONE METTO QUELLA DELLA FILIALE
284200941130     C                   Z-ADD     ARBDTV        CSVDTV
284300941130     C                   Z-ADD     ARBORV        CSVORV
284400020604     c*
284500021112     c                   clear                   tibs36ds
284600021112     c                   movel     arbpru        i36ute
284700140407     c                   eval      %subst(i36ute:10:1)=' '
284800021112     C                   CALL      'TIBS36R'
284900021112     C                   PARM                    tibs36ds
285000020604     c* non tovato profilo su azute
285100070420   1 c     o36err        ifne      *blanks
285200020604     c* richiamo fnlv55r per sapere se lna è gestita da altro p.o. e da chi
285300020604     c                   clear                   dslv55
285400020604     c                   movel     '6'           d55tpt
285500020604     c                   z-add     taslna        d55lin
285600020604     c                   z-add     csvdtv        d55drf
285700020604     c                   call      'FNLV55R'
285800020604     c                   parm                    dslv55
285900020604     C                   MOVEL     d55tfa        CSVFEV
286000070420   x1c                   else
286100020604     c* trovato profilo su azute
286200021112     c                   movel     o36pou        csvfev
286300070420    1c                   endif
286400020604     c*
286500920212     C                   MOVEL     CSBDS         CSVDS
286600941130     C* COME DATA TRSMISSIONE QUANDO HO CARICATO
286700941130     C                   Z-ADD     DATEU         CSVDTR
286800941130     C                   WRITE     TNCSV000
286900920212     C                   ENDSR
287000920130     C**---------------------------------------------------------
287100920130     C** CONTROLLO ESISTENZA BOLLA
287200920130     C**---------------------------------------------------------
287300990628     C     CTRSPE        BEGSR
287400920302     C                   SETOFF                                       332940
287500941130     C                   SETOFF                                       4217
287600941121     C                   SETOFF                                       020304
287700930514     C                   SETON                                        23
287800080408     c                   clear                   VariataSede       1
287900110315     c                   clear                   SoloCASS          1
288000941121     C*
288100991015     C     KBLP          CHAIN     TITAS30C                           29
288200920220     C*
288300941121     C* 29 ON  - S P E D I Z I O N E   I N E S I S T E N T E
288400941121    1C     *IN29         IFEQ      *ON
288500941121     C*
288600941121     C     KBLP          CHAIN     TNBLA000                           40
288700941121     C   40KANN          CHAIN     TNBLA000                           40
288800920602     C*
288900930804     C* 40 OFF - S P E D I Z I O N E   A N N U L L A T A
289000991015     C  N40              MOVEL     ERR(28)       ARBDER
289100070119     c
289200070119     c* Verifico se lnp estera, se record bolla chiuso in partenza
289300070119     c*  come merce mai affidata
289400070119    2c                   if        *in40
289500070119     c
289600070119     c     ArbLnp        Chain     Azorg01l
289700070119    3c                   If        %Found(Azorg01l)
289800070119     c                   Eval      Og143 = OrgDe3
289900070119    4c                   If        §OgNtw = 'DPD' or §ogntw='EEX' or
290000070119     c                             §ogntw='FED'
290100070119     c* controllo bolla se presente in fnblp
290200070119     c     kann          chain     fnblp01l
290300070119    5c                   if        %found(fnblp01l) and blpft1='N'
290400070119     c                             and blpcca='7'
290500070119     c                   setoff                                       23
290600070119    5c                   endif
290700070119    4c                   endif
290800070119    3c                   endif
290900070119    2c                   endif
291000920302     C*
291100920602     C* 40 ON - INESISTENTE: VEDO SE GIA' TRASFORMATO COD BOLLA
291200070119     c   23
291300070119    2Can 40FLGCVB        IFEQ      'K'
291400941121    3C     COMTBL        IFEQ      §3ATB1
291500930108     C                   MOVEL     ERR(1)        ARBDER
291600930108     C                   ELSE
291700930108     C                   MOVEL     ERR(7)        ARBDER
291800941121    3C                   END
291900930108     C*
292000920602     C                   MOVEL     §3ATB1        SAVTB1            2
292100920602     C                   MOVEL     §3ATB2        SAVTB2            2
292200080408     c                   movel     ds3a          savds3a
292300920602     C*
292400920602     C                   MOVEL     '3A'          COD
292500070119     c                   MOVEL(P)  ARBCBN        KEY
292600920602     C     KTAB          CHAIN     TABEL                              32
292700920602     C  N32              MOVEL     TBLUNI        DS3A
292800080408     c   32              clear                   ds3a
292900080408     c                   movel     §3afca        newfca            1 0
293000920602     C*
293100941121     C* 1BOLLA MODIFICATA
293200941121    3C  N32SAVTB1        IFNE      §3ATB1
293300920602     C                   MOVEL     §3ATB1        COMTBL
293400990624     C     KBLP          SETLL     TITAS30C                               42
293500920602     C  N42              MOVEL     ERR(1)        ARBDER
293600941121    3C                   END
293700920602     C*
293800941121     C* 2 BOLLA MODIFICATA
293900920602     C* SOLO SE ESISTEVA CONTROLLO
294000941121    3C  N32SAVTB2        IFNE      §3ATB2
294100920602     C     SAVTB2        ANDNE     '  '
294200920602     C*
294300941121    4C     §3ATB2        IFEQ      '  '
294400920602     C                   MOVEL     'AP'          COMTBL
294500941121     C                   ELSE
294600920602     C                   MOVEL     §3ATB2        COMTBL
294700941121    4C                   END
294800990624     C     KBLP          SETLL     TITAS30C                               42
294900070119     C  N42              MOVEL     ERR(7)        ARBDER
295000941121    3C                   END
295100080408     C***                MOVEL     SAVTB1        §3ATB1
295200080408     C***                MOVEL     SAVTB2        §3ATB2
295300080408     c                   movel     savds3a       ds3a
295400070119     c
295500070119     c* Se codice bolla vecchio = codice bolla nuovo, anche se
295600070119     c*  inesistente è inutile ristamparlo: già fatto precedentemente
295700080408    3c  n42              if        arbcbp=arbcbn and §3afca=0
295800070119     c                   setoff                                       23
295900080408    3c                   endif
296000080408     c* Se si tratta di tipo bolla con C/ASS prima e senza dopo
296100080408     c*  o una KB che toglie solo in C/ASS,
296200080408     c*  se bolla modificata in C/S --> vado avanti per il C/ASS
296300090504    3c***                if        not *in42 and *in23  and §3atb2=*blanks
296400090504    3c                   if        *in23  and §3atb2=*blanks
296500080408    4c                   if        (newfca=0 and §3afca=1) or
296600080408     c                             (arbcbp=arbcbn and §3afca=1 and arbcas=0)
296700080408     C                   MOVEL     'F7'          COMTBL
296800080408     C     KBLP          chain     TITAS30C
296900080408    5c                   if        %found(titas30c)
297000080408     c                   setoff                                       2923
297100080408     c                   eval      VariataSede='S'
297200160714   x5c                   else
297300160714
297400160714     C                   MOVEL     'A2'          COMTBL
297500160714     C     KBLP          chain     TITAS30C
297600160714    6c                   if        %found(titas30c)
297700160714    7c                   if        tascca='2' or tascca='6'
297800160714     c                   setoff                                       2923
297900160714     c                   eval      VariataSede='S'
298000160714    7c                   endif
298100160714   x6c                   else
298200160714
298300160714     C                   MOVEL     'F1'          COMTBL
298400160714     C     KBLP          chain     TITAS30C
298500160714    7c                   if        %found(titas30c)
298600160714    8c                   if        tascca='2' or tascca='6'
298700160714     c                   setoff                                       2923
298800160714     c                   eval      VariataSede='S'
298900160714    8c                   endif
299000170424   x7c                   else
299100170424     C                   MOVEL     'AS'          COMTBL
299200170424     C     KBLP          chain     TITAS30C
299300170424    8c                   if        %found(titas30c)
299400170424    9c                   if        tascca='2' or tascca='6'
299500170424     c                   setoff                                       2923
299600170424     c                   eval      VariataSede='S'
299700170424    9c                   endif
299800170424    8c                   endif
299900170424     c
300000160714    7c                   endif
300100160714    6c                   endif
300200080408    5c                   endif
300300080408    4c                   endif
300400080408    3c                   endif
300500110315     c
300600110315     c* se ho trovato la bolla con cod bolla nuovo ma era stato annullato il C/Ass  o
300700110315     c*   non c'era e ora c'e' --> lo aggiungo o riabilito
300800110315    3c                   if        *in23  and *in42 and §3atb2=*blanks  and
300900110315    4c                              newfca=1 and §3afca=0
301000110315     c                   setoff                                       2923
301100110315     c                   eval       soloCAss='S'
301200110315     c                   endif
301300920602     C*
301400941121   X2C                   ELSE
301500920602     C*
301600941130     C* CONTROLLO SE LA SPEDIZIONE E' STORNATA
301700930514     C                   EXSR      STORNO
301800991019     c* se causale variazione 'Cx' e la spedizione e' stornata
301900991019     c* spengo in29 per caricare ugualmente la variazione
302000991021    3c     flgcvb        ifeq      'C'
302100060317    3c     flgcvb        oreq      'R'
302200991021    4c     *in23         ifeq      *off
302300991021     c                   setoff                                       29
302400991021   x4c                   else
302500991021     c* altrimenti cerco la spedizione
302600991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
302700991021     c* (cioè il franco) faccio setgt e readpe invece che chain
302800991021     c     kann          setgt     titas30c
302900991021     c     kann          readpe    titas30c                               23
303000991019     c  n23              setoff                                       29
303100991021    4c                   endif
303200991021    3c                   endif
303300080408     c
303400080408     c* per "I0" e codice bolla "Z" cerco la bolla come A2 e poi
303500080408     c*  aggiorno secondo i modi già previsti in AGARBI
303600140407    3c                   if        (comcvb='I0' or
303700140407     c                              comcvb='I7' or
303800140407     c                              comcvb='I8' or
303900140407     c                              comcvb='I9' )
304000140407    3c                              and arbcbo='Z'
304100080408     C                   MOVEL     'A2'          COMTBL
304200080408     C     KBLP          chain     TITAS30C
304300080408    4c                   if        %found(titas30c)
304400080408     c                   setoff                                       2923
304500080408    4c                   endif
304600080408    3c                   endif
304700110214     c* per "I0" e codice bolla "A" cerco la bolla come F1 e poi
304800110214     c*  aggiorno secondo i modi già previsti in AGARBI
304900140407    3c***                if        comcvb='I0' and (arbcbo='A' or arbcbo='Q')
305000140407    3c                   if        (comcvb='I0' or
305100140407     c                              comcvb='I7' or
305200140407     c                              comcvb='I8' or
305300170424     c                              comcvb='I9' or
305400170424     c                              comcvb='TP' )
305500140407     c                             and (arbcbo='A' or arbcbo='Q')
305600110214     C                   MOVEL     'F1'          COMTBL
305700110214     C     KBLP          chain     TITAS30C
305800110214    4c                   if        %found(titas30c)
305900110214     c                   setoff                                       2923
306000110214    4c                   endif
306100110214    3c                   endif
306200160714     c
306300160714     c* se si tratta di causale variazione TB non stampo: la bolla è in arrivo
306400160714     c*  solo per tipo bolla AR
306500160714     c                   if        comcvb='TB' and §3atb1='AR'
306600160714     c                   setoff                                       23
306700160714     c                   seton                                        37
306800160714     c                   endif
306900930514     C*
307000941121    3C   23COMTBL        IFEQ      §3ATB1
307100991015     C                   MOVEL     ERR(1)        ARBDER
307200920130     C                   ELSE
307300991015     C                   MOVEL     ERR(7)        ARBDER
307400941121    3C                   END
307500941121    2C                   END
307600941130     C**
307700941130     C* 23 ON --> STAMPO ERRORE
307800941130    2C     *IN23         IFEQ      *ON
307900920602     C  N42
308000920602     CAN 40              SETON                                        37
308100920212     C*        PER C.E.D.
308200920603     C  N42
308300920603     CAN 40
308400920603     CORN40              Z-ADD     102           COMFEV
308500920602     C  N42
308600920602     CAN 40
308700920602     CORN40              EXSR      WTRERR
308800941121    2C                   END
308900941121     C*
309000991019    1C                   Endif
309100941121     C*
309200991019    1c     *in29         ifeq      *off
309300941121     C* 29 OFF - S P E D I Z I O N E       E S I S T E N T E
309400121108     c* Salvo contenuto del tracciato record di titas in ds apposita:
309500121108     c* serve per la scrittura dell'immagine di titas prima della prima variazione
309600121108     c* nel file TIVRB00F
309700121108     c                   exsr      memo_dstitas
309800941121     C*
309900941121     C* 02 OFF             --> GIA' CONTABILIZZATA IN BLTAS10 O BLTASP
310000941130     C* 02 ON  MA TASFIC=1 --> GIA' CONTABILIZZATA IN BLTAS00
310100930512     C*
310200941130    2C     *IN02         IFEQ      *OFF
310300941121     C     *IN02         OREQ      *ON
310400941130     C     TASFIC        ANDNE     ' '
310500941130     C**
310600950202     C* VARIAZIONE "Ix" --> indirizzo/ peso/ volume
310700941130     C**
310800941130    3C     FLGCVB        IFEQ      'I'
310900941130     C*
311000950202     C* SE VARIO PESO/VOLUME      E NON E' LA 2 BOLLA --> ERRORE
311100960416    4C     COMCVB        IFNE      'I0'
311200140407    4C     COMCVB        andne     'I7'
311300140407    4C     COMCVB        andne     'I8'
311400140407    4C     COMCVB        andne     'I9'
311500941130     C*
311600991021    5C     §3ATB2        IFNE      tastbl
311700930513     C                   SETON                                        29
311800930804     C*
311900140219    6c***  TASPKB        IFNE      ARBPKF
312000130325     C***                MOVEL     ERR(9)        ARBDER
312100130325     C***                EXSR      WTRERR
312200120313     c                   exsr      ctr_vrb
312300140219   X6C***                ELSE
312400930603     C*
312500140219    7C***  TASFVF        IFNE      'T'
312600140219     C***  TASVLF        ANDNE     ARBVLF
312700140219    8C***  TASFVF        IFEQ      'Z'
312800140219     C***  TASVLF        ANDLT     ARBVLF
312900140219     C***  TASFVF        ORNE      'Z'
313000130325     C***                MOVEL     ERR(9)        ARBDER
313100130325     C***                EXSR      WTRERR
313200140219     c***                eval      wcntab='S'
313300140219     c***                exsr      ctr_vrb
313400140219    8C***                END
313500140219    7C***                END
313600930603     C*
313700140219    6C***                END
313800930603     C*
313900941130    5C                   ENDIF
314000941130     C*
314100930512   X4C                   ELSE
314200930512     C*
314300950308RM*  C*§SE VARIATO FERMO DEPOSITO E NON E' LA 2 BOLLA --> ERRORE
314400950308RM* 5C*§         TASFFD    IFNE ARBFFD
314500950308RM*  C*§         §3ATB2    ANDNECOMTBL
314600950308RM*  C*§                   MOVELERR,9     ARBDER
314700950308RM*  C*§                   EXSR WTRERR
314800950308RM* 5C*§                   END
314900990625     C**
315000990625     C* MEMORIZZO ULTIME 4 CIFRE CODICE FATTURAZIONE 2 BOLLA
315100991021    4C     §3ATB2        IFEQ      tastbl
315200990625     C                   MOVE      TASKSC        KSC42BO           4 0
315300930512    4C                   END
315400990625     C*
315500990625    4C                   END
315600930512    3C                   END
315700950202     C**
315800950202     C* VARIAZIONE "Cx" --> consegne particolari
315900950202     C**
316000950202    3C     FLGCVB        IFEQ      'C'
316100950202     C* no errore se 2 bolla
316200991021    4C     §3ATB2        IFNE      tastbl
316300991019     c
316400991019     C                   MOVEL     'TB'          COD
316500991021     C                   MOVEL(P)  tastbl        KEY
316600991019     C     KTAB          CHAIN     TABEL                              32
316700991019     C  N32              MOVEL     TBLUNI        DSTB
316800991019     C   32              MOVEL     *ALL'1'       DSTB
316900991019   4aC     §TBFCB        IFEQ      '1'
317000950202     C*
317100950202     C* STAMPO SOLO SE UNO DEI TRE VARIATO
317200950202    5C     TASFTC        IFEQ      ARBTC1
317300950202     C     TASTC2        ANDEQ     ARBTC2
317400950202     C     TASFTC        OREQ      ARBTC2
317500950202     C     TASTC2        ANDEQ     ARBTC1
317600950202     C*
317700950202   X5C                   ELSE
317800950202     C* CONTROLLO SE LA TARIFFA TASSA LE CONSEGNE PARTICOLARI
317900140219     C***                EXSR      CONPAR
318000950202     C*
318100140219    6C**   *IN17         IFEQ      *ON
318200130325     C****               MOVEL     ERR(9)        ARBDER
318300130325     C****               EXSR      WTRERR
318400120320     c                   exsr      ctr_vrb
318500140219    6C**                ENDIF
318600950202    5C                   ENDIF
318700950202     C*
318800991019   4ac                   ENDIF
318900950202    4C                   ENDIF
319000950202    3C                   ENDIF
319100941130     C**
319200941130     C* VARIAZIONE "Tx" --> tassazione 1 bolla
319300941130     C**
319400941130    3C     FLGCVB        IFEQ      'T'
319500950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
319600130326     C***  ARBFIV        IFGT      0
319700130326     C***  TASFIV        ANDNE     ARBFIV
319800950324     C                   Z-ADD     46            COMFEV
319900130326     C***                ENDIF
320000950324     C*
320100930513     C                   MOVEL     ERR(9)        ARBDER
320200990505     c                   eval      wdelarbu='S'
320300920220     C                   EXSR      WTRERR
320400920226     C                   SETON                                        29
320500011029     c                   if        tasias > 0  and tasvas = *blanks
320600011029     c                   movel     'ITL'         tasvas
320700011029     c                   endif
320800990419     c* Controllo se variato l'importo da assicurare se contabilizzata     da assicurare
320900990419     c                   if        *in13 and (arbias <> tasias or
321000990419     c                             arbvas <> tasvas)
321100990419     c*
321200990419     c                   exsr      AGGIAS
321300990419     c*
321400990419     c                   endif
321500941130    3C                   END
321600941130     C**
321700941130     C* VARIAZIONE "2x" --> tassazione 2 bolla
321800941130     C**
321900941130    3C     FLGCVB        IFEQ      '2'
322000991021     C     tastbl        ANDEQ     §3ATB2
322100950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
322200130326     C**   ARBFIV        IFGT      0
322300130326     C**   TASFIV        ANDNE     ARBFIV
322400950324     C                   Z-ADD     46            COMFEV
322500130326     C**                 ENDIF
322600950324     C*
322700950203     C                   MOVEL     §3ATB2        TBLERR
322800930513     C                   MOVEL     ERR(9)        ARBDER
322900920226     C                   EXSR      WTRERR
323000920226     C                   SETON                                        29
323100941130    3C                   END
323200950202     C**
323300950202     C* VARIAZIONE "Mx" --> mittente
323400990628     c*                     la variazione del mittente dipende dalla conab
323500990628     c*                     della prima bolla, a prescindere da come è messa
323600990628     c*                     la seconda: se aggiono la  aggiorno anche la 2
323700950202     C**
323800950202    3C     FLGCVB        IFEQ      'M'
323900991021    5C     §3ATB2        ANDNE     tastbl
324000130326     C                   Z-ADD     46            COMFEV
324100950202     C                   MOVEL     ERR(9)        ARBDER
324200950202     C                   EXSR      WTRERR
324300950202     C                   SETON                                        29
324400950202    3C                   END
324500941130    2C                   END
324600930512    1C                   END
324700941130     C*
324800920226     C                   ENDSR
324900920228     C**---------------------------------------------------------
325000920228     C** CHAIN SU PRIMA BOLLA
325100920228     C**---------------------------------------------------------
325200920228     C     CHTB1         BEGSR
325300920228     C                   MOVEL     SAVTB1        COMTBL
325400941130     C                   SETOFF                                       020304
325500990625     C     KBLP          CHAIN     TITAS30C                           39
325600121108     c                   exsr      memo_dstitas
325700920228     C*
325800990414     C                   EXSR      CTR3
325900920228     C                   ENDSR
326000920228     C**---------------------------------------------------------
326100920228     C** CHAIN SU SECONDA BOLLA
326200920228     C**---------------------------------------------------------
326300920228     C     CHTB2         BEGSR
326400920228     C                   MOVEL     SAVTB2        COMTBL
326500941130     C                   SETOFF                                       020304
326600990625     C     KBLP          CHAIN     TITAS30C                           39
326700121108     c                   exsr      memo_dstitas
326800920228     C*
326900990414     C                   EXSR      CTR3
327000920228     C                   ENDSR
327100920226     C**---------------------------------------------------------
327200920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
327300920226     C**---------------------------------------------------------
327400920226     C     CTR2          BEGSR
327500920302     C                   MOVEL     COMTBL        FLGTB1            1
327600950202     C* SPEDIZIONE 1) GIA' IMPONIBILE     SE NO FATTURA E NO ASSEGNATO
327700950202     C*               SE VARIAZOINE DA ARRIVO
327800950202     C*            2) GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
327900950202     C*            --> NON POSSO VARIARE
328000950202    1C     TASDFT        IFGT      0
328100950202    2C     TASFIC        IFNE      ' '
328200920226     C                   SETON                                        29
328300130326     c                   z-add     46            comfev
328400991015     C                   MOVEL     ERR(9)        ARBDER
328500920226     C                   EXSR      WTRERR
328600950202    2C                   END
328700920226     C*
328800950202   X1C                   ELSE
328900920226     C*
329000990624    2C     TASimv        IFNE      *zeros
329100950202     C     COMCVB        ANDNE     'TP'
329200950202    3C     FLGTB1        ANDNE     'A'
329300920302     C                   SETON                                        29
329400920226     C*
329500991015     C
329600991015     C   29              MOVEL     ERR(8)        ARBDER
329700130326     c   29              z-add     46            comfev
329800920226     C   29              EXSR      WTRERR
329900950202    2C                   END
330000920302     C*
330100950202    1C                   END
330200920226     C                   ENDSR
330300920226     C**---------------------------------------------------------
330400920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
330500941130     C** PER LE VAIRAZIONI C/ASSEGNO O TIPI BOLLA
330600920226     C**---------------------------------------------------------
330700920226     C     CTR3          BEGSR
330800920303     C                   SETOFF                                       2938
330900920226     C* SPEDIZIONE GIA' IMPONIBILE     SE NO FATTURA
331000920226     C*            GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
331100920226     C*            NON POSSO VARIARE
331200920226     C*
331300170110     C**N02              SETON                                        2936
331400170110     C  N02              SETON                                        29
331500941202     C   02TASDFT        IFGT      0
331600990414     C     TASFIC        ANDNE     ' '
331700170110     C***                SETON                                        2936
331800170110     C                   SETON                                        29
331900920226     C*
332000920226     C                   ELSE
332100920226     C*
332200990624     C     TASimv        IFNE      *zeros
332300920226     C                   SETON                                        38
332400920226     C                   END
332500920226     C*
332600920226     C                   END
332700920226     C                   ENDSR
332800920211     C**---------------------------------------------------------
332900920211     C** SCRIVO FILE ERRORI
333000920211     C**---------------------------------------------------------
333100920211     C     WTRERR        BEGSR
333200110208     c                   clear                   arbdat
333300920211     C*
333400970916     C* CREO IL RECORD DI ERRORE SOLO SE LA DATA VARIAZIONE E' INFERIOR
333500970916     C* DI 3 GIORNI A OGGI PER LE STAMPA DI SEDE
333600000703    0C     COMFEV        IFEQ      102
333700110208     C     ARBDTV        ANDLE     DATAM3
333800000703     c     *in37         andeq     *on
333900100225
334000110208     c* Se si tratta di codice TP con importo da assicurare, stampo subito
334100100225    0C     COMFEV        oreq      102
334200100225     c     *in13         andeq     *on
334300100225     c     arbias        andgt     0
334400970916     C*é*
334500970916     C     COMFEV        ORNE      102
334600000703     C*é*
334700000703     C     *in37         ORNE      *on
334800970916     C**
334900991015     C     KERR          SETLL     FIARBE2C                               01
335000930702     C* SE NON TROVATO ERRORE
335100930706    1C     *IN01         IFEQ      *OFF
335200110208     c
335300110208     c* Se l'errore è bolla inesistente, aspetto 3 gg prima di ricrearlo
335400110208    2c                   if        arbder=err(1)
335500110208     c                   eval      arbdat=dateu
335600110208     C     KERR          SETLL     FIARBE2C                               01
335700110208    3c                   if        not  *in01
335800110208     c                   eval      arbdat=datam1
335900110208     C     KERR          SETLL     FIARBE2C                               01
336000110208    4c                   if        not  *in01
336100110208     c                   eval      arbdat=datam2
336200110208     C     KERR          SETLL     FIARBE2C                               01
336300110208    4c                   endif
336400110208    3c                   endif
336500110208    2c                   endif
336600110208    1c                   endif
336700110208     c
336800110208    1C     *IN01         IFEQ      *OFF
336900110209     c                   clear                   arbdat
337000930702     C* RISCRIVO ERRORE
337100950203     C* SE NON L'HO GIA' IMPOSTATO CERCO FILIALE A CUI INVIARE STAMPA
337200930706    2C     COMFEV        IFEQ      0
337300920212     C*
337400950203    3C     TBLERR        IFEQ      'F'
337500991015     C                   MOVEL     ARBLNP        ARBFEV
337600930706   X3C                   ELSE
337700991015     C                   MOVEL     TASLNA        ARBFEV
337800930706    3C                   END
337900971215     C* PRENDO TERMINAL DI PARTENZA
338000971215     C                   CLEAR                   DSLV55
338100971215     C                   MOVEL     'P'           D55TPT
338200991015     C                   MOVE      ARBFEV        D55LIN
338300971215     C                   Z-ADD     DATEU         D55DRF
338400971215     C                   CALL      'FNLV55R'
338500971215     C                   PARM                    DSLV55
338600971215     C     D55ERR        IFEQ      *BLANKS
338700991015     C                   MOVE      D55TFP        ARBFEV
338800971215     C                   END
338900920212     C*
339000930706   X2C                   ELSE
339100920212     C*
339200991015     C                   Z-ADD     COMFEV        ARBFEV
339300930706    2C                   END
339400991015     C     ARBFEV        IFEQ      102
339500070420     C*********          MOVE      MBR           ARBLNA
339600950202     C                   ELSE
339700950202     C                   MOVEL     TASLNA        ARBLNA
339800950202     C                   ENDIF
339900920211     C*
340000950201     C   11              WRITE     FNARBDEE
340100920212     C*
340200950201     C   12              WRITE     FNARBKEE
340300950201     C* SCRIVO ANCHE FNARBM0E SOLO SE RICHIESTO
340400170109    2C** 12*IN36         IFEQ      '1'
340500170109     C**   KARBM         CHAIN     FNARBMRR                           32
340600170109     C**N32              WRITE     FNARBMEE
340700170109     C**                 SETOFF                                       36
340800170109    2C**                 END
340900920212     C*
341000950203     C     *IN13         IFEQ      *ON
341100991015     C                   WRITE     FIARBTEE
341200950203     C*
341300991015     c* se devo cancellare fiarbu, lo faccio
341400991018     c     arbtrc        ifeq      '1'
341500170110     C     KARBU         SETLL     FIARBU2l
341600170110     C     KARBU         READE     FIARBU2l                               30
341700950203     C*
341800950203     C     *IN30         DOWEQ     *OFF
341900991015     C                   WRITE     FIARBUEE                             30
342000950203     C* SE POSSO LI DELETO
342100990505     c  n37              if        wdelarbu='S'
342200990505     C                   ADD       1             NUMRBU
342300170109     C                   DELETE    FIARBUtt
342400990505     c                   endif
342500950203     C*
342600170110     C     KARBU         READE     FIARBU2l                               30
342700950203     C                   ENDDO
342800991018     c                   endif
342900950203     C                   ENDIF
343000950203     C*
343100950202     C   15              WRITE     FNARBGEE
343200950202     C   16              WRITE     FNARBMEE
343300991214   X1c                   else
343400991214     c* se devo cancellare fiarbu, lo faccio
343500991214    2c     *in37         ifeq      *off
343600991214     c     wdelarbu      andeq     'S'
343700991214     c     arbtrc        andeq     '1'
343800170110     C     KARBU         SETLL     FIARBU2l
343900170110     C     KARBU         READE     FIARBU2l                               30
344000991214     C*
344100991214    3C     *IN30         DOWEQ     *OFF
344200991214     C                   ADD       1             NUMRBU
344300170109     C                   DELETE    FIARBUtt
344400170110     C     KARBU         READE     FIARBU2l                               30
344500991214    3C                   ENDDO
344600991214    2c                   endif
344700930706    1C                   ENDIF
344800970916    0C                   ENDIF
344900920212     C*
345000920212     C                   Z-ADD     0             COMFEV            3 0
345100950203     C                   MOVEL     §3ATB1        TBLERR
345200920211     C                   ENDSR
345300930514     C**---------------------------------------------------------
345400930514     C** CONTROLLO SE LA SPEDIZIONE INESISTENTE E' STORNATA
345500930514     C**---------------------------------------------------------
345600930514     C     STORNO        BEGSR
345700930514     C*
345800930514     C* SE E' SINGOLA BOLLA
345900930514     C     §3ATB2        IFEQ      '  '
346000930514     C*
346100991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
346200991021     c* (cioè il franco) faccio setgt e readpe invece che chain
346300991021     C     KANN          setgt     TITAS30C
346400991021     C     KANN          readpe    TITAS30C                               23
346500931119     C*
346600941130     C     *IN23         IFEQ      *OFF
346700930514     C                   MOVEL     'TB'          COD
346800941130     C                   MOVEL(P)  TASTBL        KEY
346900930514     C     KTAB          CHAIN     TABEL                              32
347000930514     C  N32              MOVEL     TBLUNI        DSTB
347100930514     C   32              MOVEL     *ALL'1'       DSTB
347200930514     C*
347300930514     C* SE NON E' STORNO STAMPO ERRORE
347400930514     C     §TBFCB        IFEQ      '1'
347500930514     C                   SETON                                        23
347600930514     C                   ELSE
347700930514     C                   MOVEL     '1'           ST1               1
347800930514     C                   END
347900930514     C                   END
348000930514     C*
348100930514     C                   END
348200930514     C                   ENDSR
348300930804     C*
348400950202     C**---------------------------------------------------------
348500950202     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE
348600950202     C**---------------------------------------------------------
348700930804     C     CONPAR        BEGSR
348800930804     C                   SETOFF                                       17
348900930804     C*
349000930804     C                   MOVE      TASKSC        KSC4              4 0
349100930804     C*
349200930804     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
349300930804    2C     KSC4          IFEQ      9999
349400930804     C     KSC4          OREQ      8888
349500930804     C                   SETON                                        17
349600930804     C*
349700930804   X2C                   ELSE
349800930804     C*
349900930804     C                   Z-ADD     999           TARPRG
350000930804     C                   Z-ADD     999           PREPRG
350100991021     C                   MOVEL     tastbl        UNOTB1
350200930804     C*
350300930805     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
350400930805     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
350500930804    3C     UNOTB1        IFEQ      'F'
350600930805     C                   MOVEL     'A'           TFP               1
350700930804     C                   ELSE
350800930805     C                   MOVEL     'F'           TFP
350900930804    3C                   END
351000930804     C*
351100930804     C* SE C'E' A ZERO NON DO' L'ERRORE
351200941130     C     KTAM          SETLL     TNTAM000
351300941130     C     KTAM          READE     TNTAM000                               35
351400930804     C*
351500930804    3C     *IN35         DOWEQ     *OFF
351600930804     C*
351700930804     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
351800930804    4C     TAMATB        IFNE      'A'
351900930804     C*
352000941205    5C     TAMBAP        IFNE      'B'
352100941205     C     TAMBAP        ANDNE     TFP
352200930804     C*
352300930804     C                   MOVE      TAMPRG        TARPRG            3 0
352400930804     C*
352500930804     C* VEDO SE E' TARIFFA VALIDA
352600121116     c                   movel     tasaas        tasdsp
352700121116     c                   move      tasmgs        tasdsp
352800120430    6C     TASDSP        IFGE      TAMDDT
352900120430     C     TASDSP        ANDLE     TAMDST
353000120430     C                   SETON                                        35
353100120430    6C                   END
353200930804     C*
353300930804   X5C                   ELSE
353400930804     C                   MOVE      TAMPRG        PREPRG            3 0
353500930804    5C                   END
353600930804    4C                   END
353700930804     C*
353800941130     C  N35KTAM          READE     TNTAM000                               35
353900930804    3C                   END
354000930804     C*
354100930804     C* NON TROVATA TARIFFA: STAMPO ERRORE
354200930804    3C     TARPRG        IFEQ      999
354300930804     C     PREPRG        ANDEQ     999
354400930804     C                   SETON                                        17
354500930804   X3C                   ELSE
354600930804     C*
354700930804    4C     TARPRG        IFNE      999
354800930804     C                   MOVEL     TARPRG        PRG
354900930804   X4C                   ELSE
355000930804     C                   MOVEL     PREPRG        PRG
355100930804    4C                   ENDIF
355200930804     C*
355300930804     C* VEDO SE TARIFFA PARTICOLARE A 0
355400950202     C     ARBTC1        IFNE      ' '
355500990719     C                   MOVEL(P)  ARBTC1        WFTC
355600950202     C                   EXSR      PARTIC
355700950202     C                   ENDIF
355800950202     C  N17ARBTC2        IFNE      ' '
355900990719     C                   MOVEL(P)  ARBTC2        WFTC
356000950202     C                   EXSR      PARTIC
356100950202     C                   ENDIF
356200950202     C*
356300930804    3C                   END
356400930804    2C                   END
356500930804     C*
356600930804     C                   ENDSR
356700950202     C**---------------------------------------------------------
356800950202     C* CONTROLLO IN TPT
356900950202     C**---------------------------------------------------------
357000950202     C     PARTIC        BEGSR
357100950202     C*
357200990719     C     KTAM2         CHAIN     TITPT000                           17
357300950202     C* 17 ON - NON ESISTE TARIFFA
357400950202    4C     *IN17         IFEQ      *OFF
357500950202     C*         NON ESISTE PERCHE' ANNULLATA
357600950202    5C     TPTATB        IFEQ      'A'
357700950202     C                   SETON                                        17
357800950202   X5C                   ELSE
357900950202     C* VEDO SE ESISTE DETTAGLIO TARIFFA PARTICOLARE
358000990719     C     KTAM2         SETLL     TITPD000
358100990719     C     KTAM2         READE     TITPD000                               35
358200950202     C*
358300950202    6C     *IN35         DOWEQ     *OFF
358400950202     C* NO ANNULLATO
358500950202    7C     TPDATB        IFEQ      ' '
358600950202     C* SE > 0 ERRORE
358700950202    8C     TPDITR        IFNE      0
358800950202     C     TPDMIN        ORNE      0
358900950202     C     TPDMAX        ORNE      0
359000950202     C                   SETON                                        1735
359100950202    8C                   END
359200950202    7C                   END
359300950202     C*
359400990719     C  N35KTAM2         READE     TITPD000                               35
359500950202    6C                   END
359600950202    5C                   END
359700950202    4C                   END
359800950202     C                   ENDSR
359900941130     C**---------------------------------------------------------
360000941130     C* CONTROLLO SE ESISTE VARIA "G"
360100941130     C**---------------------------------------------------------
360200941130     C     ESVARG        BEGSR
360300941130     C                   CLEAR                   WSIVAR
360400941130     C*
360500941130     C                   SETON                                        30
360600941130    3C     TASSV1        IFNE      'G'
360700941130     C     TASSV2        ANDNE     'G'
360800941130     C     TASSV3        ANDNE     'G'
360900941130     C                   MOVEL     'G'           WSV1
361000990625     C     KBLPV         SETLL     TITA730C                               30
361100941130    3C                   ENDIF
361200941130     C*
361300941130     C     *IN30         IFEQ      *ON
361400941130     C                   MOVEL     'S'           WSIVAR            1
361500941130     C                   ENDIF
361600941130     C                   ENDSR
361700941202     C**---------------------------------------------------------
361800941202     C* CONTROLLO SE HO MODIFICATO IL DESTINATARIO -------------------*
361900941202     C**---------------------------------------------------------
362000941202     C     CTRDES        BEGSR
362100941202     C*
362200950202     C                   MOVEL     CSBRSD        ALF35            35
362300941202     C* CONTROLLO LA 1 POSIZIONE <> ' '
362400950202     C     ' '           CHECKR    ALF35         C                 3 0    30
362500950202     C                   MOVE      ALF35         W001A             1
362600941202     C*
362700941202     C* CONFRONTO CON IL CAMPO CHE HO RICEVUTO
362800941202     C     *IN30         IFEQ      *ON
362900941215     C     W001A         ANDEQ     ' '
363000950202     C                   MOVEA     ARBRSD        K35
363100941202     C                   ADD       1             C
363200950202     C                   MOVEA     *BLANKS       K35(C)
363300950202     C                   MOVEA     K35           W035A            35
363400941202     C                   ELSE
363500950202     C                   MOVEL     ARBRSD        W035A
363600941202     C                   ENDIF
363700941202     C*
363800950202     C     W035A         IFEQ      ALF35
363900941202     C                   SETOFF                                       31
364000941202     C                   END
364100941202     C*
364200941202     C                   ENDSR
364300990415     C**------------------------------------------------------------------------------
364400990415     C** VARIAZIONE IMPORTO D'ASSICURARE PER SPEDIZIONE GIA' TASSATA / CONTABILIZZATA
364500990415     C**------------------------------------------------------------------------------
364600990415     c*
364700990419     c     AGGIAS        begsr
364800990415     c*
364900990415     c* controllo se la spedizione è già stata fatturata / contabilizzata oppure
365000990415     c* se è stato calcolato l'imponibile
365100990415     c*
365200990415     c                   exsr      ctr3
365300990415     c* se è acceso il  38 vuol dire che non è stata contabilizzata ma è stato calcolato
365400991018     c* fino all'imponibile e quindi devo andare a controllare se è già stata
365500990415     c* calcolata la varia "R"
365600990519     c                   select
365700990519    1c                   when      *in38
365800990421     c*
365900990421     c                   z-add     tasias        arbiap
366000990421     c                   movel     tasvas        arbvap
366100990421     c                   movel     'R'           arbvar
366200991019     c*
366300991019     c* verifico se devo registrare messagio per ufficio assicurazione
366400991019     C* relativo a spedizione avente tipo bolla non assicurabile per la
366500991019     c* quale è stato tolto, modificato o aggiunto l'importo da assicurare
366600991019     c* Escludo le bolle di recupero
366700991019     c     tblrbl        ifne      'R'
366800991019     c                   exsr      msgass
366900991019     c                   endif
367000131129
367100131129     c                   clear                   arbiap
367200131129     c                   clear                   arbvap
367300131129     c                   clear                   arbvar
367400990415     c*
367500990824     c* Non faccio niente se spedizione:
367600990824     c* - contabilizzata
367700990824     c* - tasias> 0
367800990824     c* - tasfcm="F" (tasias immesso dalla sede (dalla fatturazione))
367900990824     c* - arbias= 0
368000990824     c                   when      *in29 and tasias > 0 and tasfcm = 'F'
368100990824     c                                   and arbias = 0
368200990824     c*
368300990519     c* se è acceso il 29  vuol dire che è stata CONTABILIZZATA
368400990519    1c                   when      *in29
368500990505     c                   eval      wdelarbu='S'
368600990415     c
368700990415     c* verifico l'esistenza della varia "R"
368800990415     c                   exsr      esvarr
368900990419     c*
369000990419     c                   z-add     tasias        arbiap
369100990419     c                   movel     tasvas        arbvap
369200990419     c                   movel     'R'           arbvar
369300990415     c*
369400990519    2c                   SELECT
369500990415     c*
369600990415     c* importo in TAS = SI
369700990415     c* importo in ARB = NO
369800990415     c* varia   "R"    = SI
369900990415     c*                            errore no tolta varia "R"
370000991019     c                   when      tasias > 0 and arbias = 0
370100130325     c***                if        Wvariar = 'S'
370200130325     c***                movel     err(34)       arbder
370300130325     c***                exsr      WTRERR
370400130325     c***                endif
370500121203     c                   exsr      ctr_vrb
370600990415     c* messaggio all'Ufficio Assicurazione
370700990519     c* escludo le bolle di recupero
370800990519     c                   if        tblrbl<>'R'
370900990415     c                   z-add     603           comfev
371000990415     c                   movel     err(32)       arbder
371100990419     c                   exsr      wtrerr
371200990519     c                   endif
371300990415     c*
371400990415     c* importo in TAS = NO
371500990415     c* importo in ARB = SI
371600990415     c* varia   "R"    = NO
371700990415     c*                            errore no aggiunta varia "R"
371800991019     c                   when      tasias = 0 and arbias > 0
371900130325     c                   if        wvariar = ' '
372000130325     c***                movel     err(35)       arbder
372100130325     c***                exsr      wtrerr
372200120423     c                   exsr      ctr_vrb
372300991019     c                   endif
372400990415     c* messaggio all'Ufficio Assicurazione
372500990519     c                   if        tblrbl<>'R'
372600990415     c                   z-add     603           comfev
372700990415     c                   movel     err(31)       arbder
372800990419     c                   exsr      wtrerr
372900990519     c                   endif
373000990415     c*
373100990415     c* importo in TAS = SI
373200990415     c* importo in ARB = SI  ma diversa da tas
373300990415     c* varia   "R"    = SI
373400990415     c*                            errore no modificata varia "R"
373500991019     c                   when      tasias <> arbias
373600991019     c                   if        wvariar = 'S'
373700130325     c***                movel     err(36)       arbder
373800130325     c***                exsr      wtrerr
373900120423     c                   exsr      ctr_vrb
374000991019     c                   endif
374100990415     c* messaggio all'Ufficio Assicurazione
374200990519     c                   if        tblrbl<>'R'
374300990415     c                   z-add     603           comfev
374400990415     c                   movel     err(33)       arbder
374500990419     c                   exsr      wtrerr
374600990519     c                   endif
374700990415     c*
374800990519    2c                   endsl
374900990415     c*
375000990624     c* aggiorno in TITAS il valore dell'importo d'assicurare
375100990415     c                   z-add     arbias        tasias
375200990415     c                   movel     arbvas        tasvas
375300990824     c                   clear                   tasfcm
375400990415     c*
375500990624     c   02              update    titas000
375600990624     c   03              update    titas010
375700990624     c   04              update    titasp00
375800990419     c*
375900990419     c                   clear                   arbiap
376000990419     c                   clear                   arbvap
376100990419     c                   clear                   arbvar
376200990419     c**
376300990415     c* riaccendo il 29 e spengo il 36
376400990415     c                   seton                                        29
376500170110     c*****              setoff                                       36
376600991019     c* Se e' bolla normale, ma non compare nel tabulato per la
376700991019     c*  Consuldanni, stampo solo per l'ufficio assicurazione
376800990519     c                   other
376900990519     c                   if        tblrbl<>'R'
377000000410     c                   z-add     tasias        arbiap
377100000410     c                   movel     tasvas        arbvap
377200000410     c                   movel     'R'           arbvar
377300991019     c                   exsr      msgass
377400000410     c                   clear                   arbiap
377500000410     c                   clear                   arbvap
377600000410     c                   clear                   arbvar
377700990519     c                   endif
377800990519    1c                   ENDSL
377900990415     c*
378000990415     c                   endsr
378100990415     c*
378200991019     C**-------------------------------------------------------------
378300991019     C** messaggi per ufficio Assicurazione per tipo bolla non assic.
378400991019     C**-------------------------------------------------------------
378500991019     C     msgass        BEGSR
378600991019     C                   MOVEL     'TB'          COD
378700991019     c                   movel(P)  tastbl        key
378800991019     C     KTAB          CHAIN     TABEL                              32
378900991019     C  N32              MOVEL     TBLUNI        DSTB
379000991019     C   32              clear                   DSTB
379100991019     c* se bolla non assicurabile e aggiunto, tolto o modificato importo
379200991019     c* da assicurare devo stampare messaggio per ufficio Assicurazione
379300991019    1c     §tbfas        ifeq      '0'
379400991019    2c                   select
379500991019     c* tolto importo da assicurare
379600991019   w1c                   when      tasias > 0 and arbias = 0
379700991019     c                   z-add     603           comfev
379800991019     c                   movel     err(41)       arbder
379900991019     c                   exsr      wtrerr
380000991019     c* aggiunto importo da assicurare
380100991019   w2c                   when      tasias = 0 and arbias > 0
380200991019     c                   z-add     603           comfev
380300991019     c                   movel     err(40)       arbder
380400991019     c                   exsr      wtrerr
380500991019     c* modificato importo da assicurare
380600991019   w3c                   when      tasias <> 0
380700991019     c                   z-add     603           comfev
380800991019     c                   movel     err(42)       arbder
380900991019     c                   exsr      wtrerr
381000991019    2c                   endsl
381100991019    1c                   endif
381200991019     c                   endsr
381300990415     C**---------------------------------------------------------
381400990415     C* CONTROLLO SE ESISTE VARIA "R"
381500990415     C**---------------------------------------------------------
381600990415     C     ESVARR        BEGSR
381700990415     C                   CLEAR                   WVARIAR           1
381800990415     C*
381900990415     C                   SETON                                        30
382000990415    3C     TASSV1        IFNE      'R'
382100990415     C     TASSV2        ANDNE     'R'
382200990415     C     TASSV3        ANDNE     'R'
382300990415     C                   MOVEL     'R'           WSV1
382400170110     C     KBLPV         setll     TITA730C                               30
382500990415    3C                   ENDIF
382600990415     C*
382700990415     C     *IN30         IFEQ      *ON
382800990415     C                   MOVEL     'S'           WVARIAR
382900990415     C                   ENDIF
383000990415     C                   ENDSR
383100941130     C**---------------------------------------------------------
383200941130     C* ROUTINE INIZIALE
383300941130     C**---------------------------------------------------------
383400941130     C     *INZSR        BEGSR
383500021014
383600021014
383700941130     C                   Z-ADD     1             CODUT             1 0
383800021014
383900021014     c     *dtaara       define    §azute        azuteds
384000021014     c     *dtaara       define    §datiute      ddatiute
384100021014     C                   in(E)     *dtaara
384200021014     C                   IF        %Error  or  RSUT = *blanks
384300021014     C                   CLEAR                   TIBS34DS
384400021014     C                   call      'TIBS34R'
384500021014     C                   parm                    Tibs34Ds
384600021014     C                   in        *dtaara
384700021014     c                   ENDIF
384800021014
384900021014     C                   Z-ADD     DUTKCI        KCI               4 0
385000021014
385100941130     C*---------------------------------------------------------------*
385200991018     C     KBLP          KLIST
385300950201     C                   KFLD                    ARBAAS
385400941130     C                   KFLD                    ARBLNP
385500941130     C                   KFLD                    ARBNRS
385600941130     C                   KFLD                    ARBNSP
385700941130     C                   KFLD                    COMTBL            2
385800990629     C     KTITA         KLIST
385900990629     C                   KFLD                    TASAAS
386000990629     C                   KFLD                    TASLNP
386100990629     C                   KFLD                    TASNRS
386200990629     C                   KFLD                    TASNSP
386300990629     C                   KFLD                    §TRC
386400941130     C     KBLPV         KLIST
386500950201     C                   KFLD                    ARBAAS
386600941130     C                   KFLD                    ARBLNP
386700941130     C                   KFLD                    ARBNRS
386800941130     C                   KFLD                    ARBNSP
386900941130     C                   KFLD                    COMTBL            2
387000941130     C                   KFLD                    WSV1
387100941130     C     KANN          KLIST
387200950201     C                   KFLD                    ARBAAS
387300941130     C                   KFLD                    ARBLNP
387400941130     C                   KFLD                    ARBNRS
387500941130     C                   KFLD                    ARBNSP
387600950202     C     KARBU         KLIST
387700941130     C                   KFLD                    ARBAAS
387800941130     C                   KFLD                    ARBLNP
387900941130     C                   KFLD                    ARBNRS
388000941130     C                   KFLD                    ARBNSP
388100991015     C                   KFLD                    ARBTRC
388200941130     C                   KFLD                    ARBDTV
388300941130     C                   KFLD                    ARBORV
388400950202     C     KARBM         KLIST
388500950202     C                   KFLD                    ARBAAS
388600950202     C                   KFLD                    ARBLNP
388700950202     C                   KFLD                    ARBNRS
388800950202     C                   KFLD                    ARBNSP
388900950202     C                   KFLD                    ARBCVB
389000950202     C                   KFLD                    ARBDTV
389100950202     C                   KFLD                    ARBORV
389200941130     C     KARBD         KLIST
389300941130     C                   KFLD                    ARBAAS
389400941130     C                   KFLD                    ARBLNP
389500941130     C                   KFLD                    ARBNRS
389600941130     C                   KFLD                    ARBNSP
389700941130     C                   KFLD                    ARBCVB
389800941130     C                   KFLD                    ARBDTV
389900941130     C                   KFLD                    COMORV            6 0
390000941130     C     KTAB          KLIST
390100941130     C                   KFLD                    CODUT
390200941130     C                   KFLD                    COD               2
390300941130     C                   KFLD                    KEY               8
390400941130     C     KACO          KLIST
390500941130     C                   KFLD                    CODUT
390600941130     C                   KFLD                    KCI
390700941130     C                   KFLD                    TASKSC
390800950202     C     KACO2         KLIST
390900950202     C                   KFLD                    CODUT
391000950202     C                   KFLD                    KCI
391100950202     C                   KFLD                    ARBCCM
391200981118     C     Kppa          KLIST
391300981118     C                   KFLD                    wsoc
391400981118     C                   KFLD                    wctb
391500981118     C                   KFLD                    wkcc
391600981118     C                   KFLD                    wksc
391700981118     C                   KFLD                    wdft
391800981118     C                   KFLD                    wnft
391900981118     C                   KFLD                    wfiv
392000981118     C     Kycpa         KLIST
392100981118     C                   KFLD                    ppasys
392200981118     C                   KFLD                    ppanrasint
392300941130     C     KERR          KLIST
392400941130     C                   KFLD                    ARBAAS
392500941130     C                   KFLD                    ARBLNP
392600941130     C                   KFLD                    ARBNRS
392700941130     C                   KFLD                    ARBNSP
392800941130     C                   KFLD                    ARBDER
392900941130     C                   KFLD                    ARBDAT
393000991214     c                   kfld                    ARBDTV
393100991214     c                   kfld                    ARBORV
393200941130     C     KTAM          KLIST
393300941130     C                   KFLD                    TASKSC
393400941130     C                   KFLD                    TASCTR
393500941130     C     KTAM2         KLIST
393600941130     C                   KFLD                    TASKSC
393700941130     C                   KFLD                    TASCTR
393800941130     C                   KFLD                    PRG               3 0
393900950202     C                   KFLD                    WFTC
394000151119     C     Kar5          KLIST
394100151119     C                   KFLD                    ARBAAS
394200151119     C                   KFLD                    ARBLNP
394300151119     C                   KFLD                    ARBNRS
394400151119     C                   KFLD                    ARBNSP
394500151119     C                   KFLD                    kar5trd
394600121108
394700121108
394800121108
394900941130     C**
395000941130     C* DEFINIZIONE CAMPI
395100941130     C**
395200960625     C     *LIKE         DEFINE    TASTBL        SAVTBL
395300990719     C     *LIKE         DEFINE    tptftc        WFTC
395400950202     C     *LIKE         DEFINE    TASVLF        SAVVLF
395500950202     C     *LIKE         DEFINE    TASVLF        SAVVLB
395600950202     C     *LIKE         DEFINE    TASFVF        SAVFVF
395700950202     C     *LIKE         DEFINE    TASFVF        SAVFVB
395800990625     C     *LIKE         DEFINE    TAScts        SAVcts
395900941130     C     *LIKE         DEFINE    TASSV1        WSV1
396000941130     C     *LIKE         DEFINE    TASVA1        WVA1
396100941130     C     *LIKE         DEFINE    ARBDER        SAVDER
396200941130     C     *LIKE         DEFINE    CSBPAR        DESPAR
396300970916     C     *LIKE         DEFINE    CSBPAR        DESPRV
396400010126     C     *LIKE         DEFINE    arblnp        savlnp
396500010126     C     *like         define    tasftc        WTCP
396600010126     C     *like         define    tasftc        WsvTCP
396700941130     C*---------------------------------------------------------------*
396800941130     C*
396900941130     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
397000941130     C* ASSUMO IL PRIMO MEMBRO DEL FILE
397100170109     C*    MBRFIL        IFEQ      *BLANK
397200170109     C*                  MOVEL     'M888888888'  MBRFIL
397300170109     C*                  END
397400941130     C* MEMBRO LUNGO 3
397500170109     C*                  MOVEL     ALFFIL        MBR               3 0
397600941130     C* PRENDO ANNO IN CORSO
397700170110     C****               MOVE      UDATE         AAS               2 0
397800070119     c* Apro file bolle di filiale
397900070119     c                   clear                   tibs55ds
398000070119     c                   eval      i50apo=001
398100070119     c                   call      'TIBS55R'
398200070119     c                   parm                    tibs55ds
398300070119     c*
398400070119     c                   Clear                   commt
398500070119     c                   Movel(p)  cmdt(1)       commt
398600070119     c                   Eval      %Subst(commt:30:10) = o50ala
398700070119     c                   Eval      commt =
398800070119     c                             %trim(commt) + '/FNBLP01L)'
398900070119     c                   Call      'QCMDEXC'
399000070119     c                   Parm                    commt
399100070119     c                   Parm                    lenght
399200070119     c                   Open      FNBLP01L
399300070119     c
399400070119     c
399500071102      * Aggancio tabella VPO record DECO
399600120510     c**********         Clear                   Dvpodeco
399700120510     c***********        Clear                   Tibs02ds
399800120510     c***********        Eval      T02tla = 'L'
399900120510     c***********        Eval      T02Mod = 'C'
400000120510     c*************      Eval      T02Sif = knsif
400100120510     c*************      Eval      T02Cod = 'VPO'
400200120510     c***********        Movel(p)  'DECO'        T02Ke1
400300120510     c*******            Call      'TIBS02R'
400400120510     c*********          Parm                    Kpjba
400500120510     c*********          Parm                    Tibs02ds
400600120510     c*********          If        T02Err = *Blanks
400700120510     c*********          Movel     T02Uni        Dvpodeco
400800120510     c**********         EndIf
400900071102
401000941130     C                   ENDSR
401100170110     C**---------------------------------------------------------
401200170110     C* Impostazione campi date
401300170110     C**---------------------------------------------------------
401400170110     c     sr_date       begsr
401500170110     C                   TIME                    W0140            14 0
401600170110     C                   MOVEL     W0140         UTIME             6 0
401700170110     C                   MOVE      W0140         WDTGIO            8 0
401800170110     C* UDATE IN AAAAMMGG
401900170110     C                   Z-ADD     WDTGIO        G02DAT
402000170110     C                   MOVEL     *BLANK        G02ERR
402100170110     C                   CALL      'XSRDA8'
402200170110     C                   PARM                    WLBDAT
402300170110     C                   MOVEL     G02INV        DATEU             8 0
402400170110     C* SOTTRAGGO  1
402500170110     C     G02TGI        SUB       1             GIOTGI
402600170110     C                   CALL      'XSRGI8'
402700170110     C                   PARM                    WGIDAT
402800170110     C                   Z-ADD     GIOINV        DATAM1            8 0
402900170110     C* SOTTRAGGO  2
403000170110     C     G02TGI        SUB       2             GIOTGI
403100170110     C                   CALL      'XSRGI8'
403200170110     C                   PARM                    WGIDAT
403300170110     C                   Z-ADD     GIOINV        DATAM2            8 0
403400170110     C* SOTTRAGGO  3
403500170110     C     G02TGI        SUB       3             GIOTGI
403600170110     C                   CALL      'XSRGI8'
403700170110     C                   PARM                    WGIDAT
403800170110     C                   Z-ADD     GIOINV        DATAM3            8 0
403900170110     c                   endsr
404000930804     C*
404100981001     C**-------------------------------------------------------------
404200981001     C** AGGIORNO CON CODICI DI VARIAZIONE DM DD BOLLA SEDE MITT/DEST
404300981001     C**-------------------------------------------------------------
404400981001     C     AGTMD         BEGSR
404500981001     C* CONTROLLO CHE TIPO DI VARIAZIONE STO LEGGENDO
404600981001     C* DM CREAZIONE MITTENTE BOLLA SEDE
404700981001     C* DD CREAZIONE DESTINATARIO BOLLA SEDE
404800981001     C* I0 VARIAZIONE DESTINATARIO
404900981001     C* MI VARIAZIONE MITTENTE
405000981001     C*
405100981001    1C     ARBCVB        IFNE      'DM'
405200981001     C     ARBCVB        ANDNE     'DD'
405300981001     C     ARBCVB        ANDNE     'I0'
405400140407     C     ARBCVB        ANDNE     'I7'
405500140407     C     ARBCVB        ANDNE     'I8'
405600140407     C     ARBCVB        ANDNE     'I9'
405700981001     C     ARBCVB        ANDNE     'MI'
405800981001     C*
405900981001   X1C                   ELSE
406000981001     C* CERCO LA SPEDIZIONE NEL FILE TNTMD
406100981001     C     KANN          CHAIN     TNTMD01L                           48
406200981001     C* IMMISSIONE O CAMBIO MITTENTE
406300981001     C* CAUSALI DI VARIAZIONI
406400981001    2C     ARBCVB        IFEQ      'DM'
406500981001     C     ARBCVB        OREQ      'MI'
406600981001     C*
406700981001     C                   MOVEL     ARBRSM        TMDRSM
406800981001     C                   MOVEL     ARBINM        TMDINM
406900981001     C                   MOVEL     ARBCAM        TMDCAM
407000981001     C                   MOVEL     ARBLOM        TMDLOM
407100981001     C                   MOVEL     ARBPRM        TMDPRM
407200981001     C                   MOVEL     ARBNZM        TMDNZM
407300981001     C*
407400981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
407500981001    3C     *IN48         IFEQ      '1'
407600981005     C     ARBCVB        ANDEQ     'DM'
407700981001     C                   Z-ADD     ARBAAS        TMDAAS
407800981001     C                   Z-ADD     ARBLNP        TMDLNP
407900981001     C                   Z-ADD     ARBNRS        TMDNRS
408000981001     C                   Z-ADD     ARBNSP        TMDNSP
408100981001     C                   WRITE     TNTMD000
408200981001     C*
408300981005    3C                   ENDIF
408400981005     C* PER 48 SPENTO AGGIORNO IL RECORD
408500981005     C  N48              UPDATE    TNTMD000
408600981001     C*
408700981001    2C                   ENDIF
408800981001     C*
408900981001     C* IMMISSIONE O CAMBIO DESTINATARIO
409000981001     C* CAUSALI DI VARIAZIONI
409100981001    2C     ARBCVB        IFEQ      'DD'
409200981001     C     ARBCVB        OREQ      'I0'
409300140407     C     ARBCVB        OREQ      'I7'
409400140407     C     ARBCVB        OREQ      'I8'
409500140407     C     ARBCVB        OREQ      'I9'
409600981001     C*
409700981001     C                   MOVEL     ARBRSD        TMDRSD
409800981001     C                   MOVEL     ARBIND        TMDIND
409900981001     C                   MOVEL     ARBCAD        TMDCAD
410000981001     C                   MOVEL     ARBLOD        TMDLOD
410100981001     C                   MOVEL     ARBPRD        TMDPRD
410200981001     C                   MOVEL     ARBNZD        TMDNZD
410300981001     C*
410400981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
410500981001    3C     *IN48         IFEQ      '1'
410600981005     C     ARBCVB        ANDEQ     'DD'
410700981001     C                   Z-ADD     ARBAAS        TMDAAS
410800981001     C                   Z-ADD     ARBLNP        TMDLNP
410900981001     C                   Z-ADD     ARBNRS        TMDNRS
411000981001     C                   Z-ADD     ARBNSP        TMDNSP
411100981001     C                   WRITE     TNTMD000
411200981005    3C                   ENDIF
411300981005     C*
411400981001     C* PER 48 SPENTO AGGIORNO IL RECORD
411500981005     C  N48              UPDATE    TNTMD000
411600981001     C*
411700981001    2C                   ENDIF
411800981001     C*
411900981001     C* SE LA VARIAZIONE ARB E' DD O DM DELETO IL RECORD E NON PROSEGUO CON
412000981001     C* LA LETTURA
412100981001    2C     ARBCVB        IFEQ      'DM'
412200981001     C     ARBCVB        OREQ      'DD'
412300981001     C   11              DELETE    FNARBD00
412400981001     C   16              DELETE    FNARBM00
412500981001     C                   MOVE      'Y'           DMDD
412600981001    2C                   ENDIF
412700981001     C*
412800981001    1C                   ENDIF
412900981001     C*
413000981001     C                   ENDSR
413100120312     C**-------------------------------------------------------------
413200120515     C** RITASSAZIONE DELLA SPEDIZIONE CON I NUOVI DATI RICEVUTI
413300120312     C**-------------------------------------------------------------
413400120313     C     ctr_vrb       BEGSR
413500151008     c* Richiamo pgm memorizzazione variazioni post-fatturazione
413600151008     c                   clear                   fnlvv1ds
413700151008     c                   clear                   parvar
413800151008     c                   clear                   partitas
413900151008     c                   clear                   parcsb
414000151008     c                   eval      iv1cvb=comcvb
414100151008     c                   eval      iv1pgm=nompgm
414200151008     c                   select
414300151008     c                   when      %subst(iv1cvb:1:1)='I'
414400170109     c     nrri          chain     fnarbd2t      dsarbd
414500170109     c                   if        %found(fnarbd2t)
414600151008     c                   eval      parvar =dsarbd
414700151008     c                   endif
414800151008     c                   when      %subst(iv1cvb:1:1)='K'
414900170109     c     nrri          chain     fnarbk2t      dsarbk
415000170109     c                   if        %found(fnarbk2t)
415100151008     c                   eval      parvar =dsarbk
415200151008     c                   endif
415300151008     c                   if        wanncas='S'
415400151008     c                   eval      iv1can='S'
415500151008     c                   endif
415600151008     c                   eval      parcsb  = dscsb
415700151008     c                   when      %subst(iv1cvb:1:1)='T'
415800170109     c     nrri          chain     fiarbt2t      dsarbt
415900170109     c                   if        %found(fiarbt2t)
416000151008     c                   eval      parvar =dsarbt
416100151008     c                   endif
416200151008     c                   when      %subst(iv1cvb:1:1)='C'
416300170109     c     nrri          chain     fnarbg2t      dsarbg
416400170109     c                   if        %found(fnarbg2t)
416500151008     c                   eval      parvar =dsarbg
416600151008     c                   endif
416700151008     c                   endsl
416800151008     c                   eval      kpjbu=fnlvv1ds
416900151009     c                   eval      partitas=dstitas
417000151008     c                   call      'FNLVV1R'
417100151008     c                   parm                    kpjba
417200151008     c                   parm                    parvar
417300151008     c                   parm                    partitas
417400151008     c                   parm                    parcsb
417500120312     c                   endsr
417600121108      ****************************************************
417700121108      ** Memorizzazione tracciato della bolla di sede prima
417800121108      ** della variazione
417900121108      ****************************************************
418000121205     c     memo_dstitas  Begsr
418100121116     c                   eval      dstitas=titasds
418200121108     c                   endsr
418300121109      ****************************************************
418400121109      ** Memorizzazione dati del contrassegno prima
418500121109      ** della variazione
418600121109      ****************************************************
418700121109     c     memo_csb      Begsr
418800121109     c                   clear                   dscsb
418900121109     C     KANN          CHAIN(N)  TNCSB000
419000121109     c                   if        %found(tncsb03l)
419100121115     c                   eval      dscsb=csbds
419200121109     c                   endif
419300121109     c                   endsr
419400981001     C*
419500920113**
4196009201301 BOLLA  INESISTENTE                                                1
419700920130TIPO BOLLA INESISTENTE                                              2
419800920130CAUSALE VARIAZIONE INESISTENTE                                      3
419900960514NON ESISTE RECORD BOLLA IN TNAC200F                                 4
420000920130NON ESISTE ANAGRAFICA CONTRASSEGNI                                  5
420100920130NUOVO CODICE BOLLA INESISTENTE                                      6
4202009201302 BOLLA  INESISTENTE                                                7
420300920220SPEDIZIONE GIA' TASSATA                                             8
420400920212SPEDIZIONE GIA' CONTABILIZZATA                                      9
420500920210C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
420600950201MANCA RECORD FILE FNARBM0R                                          11
420700950201MANCA X CREAZ. ASSEGNATO FNARBD1R                                   12
420800930513X CREAR 2BOLLA MA NO VARIAZ CD.BOLL                                 15
420900920601GIA' CONTABIL.:NO TOLTA VARIA G                                     14
421000930513
421100920601GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 16
421200930513
421300920601GIA' CONTABIL.:NO AGGIUNTA VARIA G                                  18
421400930513
421500920601GIA' CONTABIL.:NO VARIATO L'A3                                      20
421600930513
421700920601GIA' CONTABIL.:NO MODIFIC. VARIA G                                  22
421800990414GIA' TASSATA  :NO TOLTA    VARIA G                                  23
421900920228GIA' TASSATA  :NO AGGIUNTA VARIA G                                  24
422000920228GIA' TASSATA  :NO MODIFIC. VARIA G                                  25
422100950203CAP E NAZIONE MITTENTE INESISTENTE                                  26
422200950203COD.MITTENTE INESISTENTE IN ANAGRAF                                 27
422300920302SPEDIZIONE ANNULLATA                                                28
422400960919COD.MITTENTE BLOCCATO                                               29
422500970814ERRORE IN DETERMINAZIONE COD.TASSAZ                                 30
422600990414AGGIUNTO   IMPORTO DA ASSICURARE                                    31
422700990414TOLTO      IMPORTO DA ASSICURARE                                    32
422800990414MODIFICATO IMPORTO DA ASSICURARE                                    33
422900990414GIA' CONTABIL.:NO TOLTA    VARIA R                                  34
423000990414GIA' CONTABIL.:NO AGGIUNTA VARIA R                                  35
423100990414GIA' CONTABIL.:NO MODIFIC. VARIA R                                  36
423200991022
423300991022
423400991022
423500991022AGGIUNTO IM.ASS. TP.BOLLA NON ASSIC                                 40
423600991022TOLTO    IM.ASS. TP.BOLLA NON ASSIC                                 41
423700991022MODIFIC. IM.ASS. TP.BOLLA NON ASSIC                                 42
423800070119** cmdt
423900070119OVRDBF FILE(FNBLP01L) TOFILE(
