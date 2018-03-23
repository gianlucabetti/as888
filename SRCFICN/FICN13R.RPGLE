000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219     H* ficn13R *----------------------------------------------------*
000300090430     H*         - DUPLICAZIONE TARIFFE COOP       -  BATCH           *
000400000000     H*--------------------------------------------------------------*
000500011030     Ffifgt01L  UF A E           K DISK
000600011030     Ffifpt01L  UF A E           K DISK
000700011030     Ffifpd01L  UF A E           K DISK
000701130617     Ftabel00f  iF   E           K DISK
000800930823     D PARAM           DS
000900931013     D  PDROLD                 1      7  0
001000931013     D  SMLOLD                 8      8
001100931013     D  TSROLD                 9      9
001200931013     D  CTROLD                10     12  0
001300931013     D  PRGOLD                13     15  0
001400931013     D  PDRNEW                16     22  0
001500931013     D  SMLNEW                23     23
001600931013     D  TSRNEW                24     24
001700931013     D  CTRNEW                25     27  0
001800011126     D  divold                28     30
001900931013     D*
002000900924     D WLBDAT          DS
002100940922     D  G02DAT                 1      8  0
002200940922     D  G02INV                 9     16  0
002300940922     D  G02ERR                17     17
002400940922     D  G02TGI                18     22  0
002500900924     D WGIDAT          DS
002600940922     D  GIODAT                 1      8  0
002700940922     D  GIOINV                 9     16  0
002800940922     D  GIOTGI                17     21  0
002900930823     D                 DS
003000930921     D  COMPDR                 1      7  0
003100931013     D  COMSML                 8      8
003200931013     D  COMTSR                 9      9
003300931013     D  COMCTR                10     12  0
003400931013     D  COMNEW                 1     12
003500930823     D                 DS
003600930921     D  FGTPDR                 1      7  0
003700931013     D  FGTSML                 8      8
003800931013     D  FGTTSR                 9      9
003900931013     D  FGTCTR                10     12  0
004000931013     D  COMFGT                 1     12
004100930823     D KPJBA         E DS
004101130617     d ds8a          e ds
004200930823     C*****************************************************************
004300930823     C* RIEPILOGO INDICATORI
004400930823     C*****************************************************************
004500930824     C* 30/33 - LETTURE
004600930824     C* 31/34 - DI COMODO
004700930823     C*****************************************************************
004800930823     C     *ENTRY        PLIST
004900930823     C                   PARM                    KPJBA
005000930823     C                   MOVEL     *ZEROS        PARAM
005100930823     C                   MOVEL     KPJBU         PARAM
005200930823     C*---------------------------------------------------------------*
005201130617     C* ACCESSO tabel
005202130617     C     Ktab          KLIST
005203130617     C                   Kfld                    tblkut
005204130617     C                   KFLD                    tblcod
005205130617     C                   KFLD                    tblkey
005206130617     C                   z-add     1             tblkut
005207130617     C                   move      '8A'          tblcod
005300011030     C* ACCESSO fifgt
005400930824     C     KFGTN         KLIST
005500930921     C                   KFLD                    PDRNEW
005600931013     C                   KFLD                    SMLNEW
005700931013     C                   KFLD                    TSRNEW
005800930824     C                   KFLD                    CTRNEW
005900930824     C     KFGTN2        KLIST
006000930921     C                   KFLD                    PDRNEW
006100931013     C                   KFLD                    SMLNEW
006200931013     C                   KFLD                    TSRNEW
006300930824     C                   KFLD                    CTRNEW
006400930824     C                   KFLD                    PRGNEW
006500011030     C* ACCESSO fifgt E fifpt
006600930823     C     KFGTP         KLIST
006700930921     C                   KFLD                    PDROLD
006800931013     C                   KFLD                    SMLOLD
006900931013     C                   KFLD                    TSROLD
007000930823     C                   KFLD                    CTROLD
007100930823     C                   KFLD                    PRGOLD
007200011030     C* ACCESSO fifpd
007300930824     C     KFPDN         KLIST
007400930921     C                   KFLD                    PDRNEW
007500931013     C                   KFLD                    SMLNEW
007600931013     C                   KFLD                    TSRNEW
007700930823     C                   KFLD                    CTRNEW
007800930823     C                   KFLD                    PRGNEW
007900011030     C                   KFLD                    FPDctd
008000930823     C                   KFLD                    FPDCAP
008100940923     C                   KFLD                    FPDSGL
008200930824     C     KFPDP         KLIST
008300930921     C                   KFLD                    PDROLD
008400931013     C                   KFLD                    SMLOLD
008500931013     C                   KFLD                    TSROLD
008600930824     C                   KFLD                    CTROLD
008700930824     C                   KFLD                    PRGOLD
008800011030     C                   KFLD                    FPTctd
008900930823     C     KFPDP2        KLIST
009000930921     C                   KFLD                    PDROLD
009100931013     C                   KFLD                    SMLOLD
009200931013     C                   KFLD                    TSROLD
009300930823     C                   KFLD                    CTROLD
009400930823     C                   KFLD                    PRGOLD
009500011030     C                   KFLD                    FPDctd
009600930823     C                   KFLD                    FPDCAP
009700940923     C                   KFLD                    FPDSGL
009800011030     C* ACCESSO fifpt
009900930823     C     KFPTN         KLIST
010000930921     C                   KFLD                    PDRNEW
010100931013     C                   KFLD                    SMLNEW
010200931013     C                   KFLD                    TSRNEW
010300930823     C                   KFLD                    CTRNEW
010400930823     C                   KFLD                    PRGNEW
010500011030     C                   KFLD                    FPTctd
010600930823     C     KFPTP         KLIST
010700930921     C                   KFLD                    PDROLD
010800931013     C                   KFLD                    SMLOLD
010900931013     C                   KFLD                    TSROLD
011000930823     C                   KFLD                    CTROLD
011100930823     C                   KFLD                    PRGOLD
011200011030     C                   KFLD                    FPTctd
011300940922     C*
011400940922     C* DEFINIZIONE CAMPI
011500940923     C     *LIKE         DEFINE    FGTDDT        WUDA                           *AAAA/MM/GG
011600940923     C     *LIKE         DEFINE    FGTPRG        PRGNEW
011700940923     C     *LIKE         DEFINE    FGTDST        DSTOLD
011800940922     C*
011900930823     C*---------------------------------------------------------------*
012000940922     C***
012100940922     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
012200940922     C***
012300940922     C                   TIME                    W0120            12 0
012400940922     C                   MOVE      W0120         WDAT              6 0
012500940922     C*
012600940922     C                   Z-ADD     WDAT          G02DAT
012700940922     C                   MOVEL     *BLANK        G02ERR
012800940922     C                   CALL      'XSRDA8'
012900940922     C                   PARM                    WLBDAT
013000940922     C* UDATE A 8 IN AAAA/MM/GG
013100940922     C                   Z-ADD     G02INV        WUDA
013200930921     C*
013300940923     C*
013400090116     C*****    D U P L I C A    D I    F I F G T    *****
013500930823     C* CERCO L'ULTIMO PRG DELLA TARIFFA IN CUI COPIARE
013600011030     C     KFGTN         SETGT     fifgt000
013700011030     C                   READP     fifgt000                               30
013800930823     C*
013900930823     C     *IN30         DOWEQ     *OFF
014000930823     C* ESCLUDO ANNULLATI
014100930823     C     FGTATB        IFEQ      ' '
014200931013     C                   Z-ADD     PDRNEW        COMPDR
014300931013     C                   MOVEL     SMLNEW        COMSML
014400931013     C                   MOVEL     TSRNEW        COMTSR
014500931013     C                   Z-ADD     CTRNEW        COMCTR
014600930823     C*
014700930823     C     COMFGT        IFNE      COMNEW
014800940923     C                   Z-ADD     0             PRGNEW
014900930823     C                   ELSE
015000930823     C     FGTPRG        ADD       1             PRGNEW
015100940923     C                   Z-ADD     FGTDST        DSTOLD
015200930823     C                   ENDIF
015300930823     C*
015400930823     C                   SETON                                        30
015500930823     C                   ENDIF
015600930823     C*
015700011030     C  N30              READP     fifgt000                               30
015800930823     C                   ENDDO
015900930823     C*
016000940923     C* TARIFFA NUOVA NON DEVE ESISTERE: SE ESISTE ANNULLATA --> DELETE
016100011030     C     KFGTN2        CHAIN     fifgt000                           31
016200011030     C  N31              DELETE    fifgt000
016300930823     C*
016400011030     C     KFGTP         CHAIN     fifgt000                           32
016500930823     C   32              GOTO      FINE
016600930823     C*
016700930823     C                   MOVEL     ' '           FGTATB
016800930921     C                   Z-ADD     PDRNEW        FGTPDR
016900931013     C                   MOVEL     SMLNEW        FGTSML
017000931013     C                   MOVEL     TSRNEW        FGTTSR
017100930823     C                   Z-ADD     CTRNEW        FGTCTR
017200930823     C                   Z-ADD     PRGNEW        FGTPRG
017300930823     C*
017400900924     C     PRGNEW        IFNE      0
017500940923     C                   Z-ADD     DSTOLD        G02INV
017600900924     C                   MOVEL     '3'           G02ERR
017700940923     C                   CALL      'XSRDA8'
017800900924     C                   PARM                    WLBDAT
017900900924     C* AGGIUNGO 1 AI GIORNI
018000900924     C     G02TGI        ADD       1             GIOTGI
018100900924     C*
018200940923     C                   CALL      'XSRGI8'
018300900924     C                   PARM                    WGIDAT
018400930823     C* DATA DECORRENZA = DATA SCADENZA PRG PRECEDENTE + 1
018500930823     C                   Z-ADD     GIOINV        FGTDDT
018600930823     C* SE DATA DECORRENZA > DATA SCADENZA : METTO LA STESSA
018700030304     C                   Z-ADD     20391231      FGTDST
018800030304     C     FGTDDT        IFGT      FGTDST
018900030304     C                   z-add     FGTDDT        FGTDST
019000030304     c                   move      1231          fgtdst
019100030304     C                   ENDIF
019200090116     c* se non esiste metto data decorrenza oggi e data fine 31/12/2039
019300090116     c                   else
019400090116     c                   z-add     wuda          fgtddt
019500090116     c                   z-add     20391231      fgtdst
019600930823     C                   ENDIF
019700930823     C*
019800011126     c                   move      divold        fgtdiv
019900940922     C                   Z-ADD     WUDA          FGTDUV
020000091210     C******             MOVEL     'T'           FGTFTR
020100091210     C******             z-add     *all'9'       FGTDTR
020200020315     C                   z-add     0             FGTDTs
020300020315     C                   z-add     0             FGTDcn
020400011030     C                   WRITE     fifgt000
020500930823     C*
020600930823     C*
020700940922     C*****    D U P L I C A    D I    T N F P T / T N F P D    *****
020800011030     C     KFGTP         SETLL     fifpt000
020900011030     C     KFGTP         READE     fifpt000                               30
021000930823     C*
021100930823     C     *IN30         DOWEQ     *OFF
021200930823     C* ESCLUDO ANNULLATI
021300930823     C     FPTATB        IFEQ      ' '
021301130617     c                   clear                   ds8a
021302130617     c                   movel(p)  fptctd        tblkey
021303130617     c     ktab          chain     tabel00f
021304130617     c                   if        %found(tabel00f)
021305130617     c                   movel     tbluni        ds8a
021306130617     c                   endif
021307130617     c                   if        §8Aesccar = *blank
021400930823     C*
021500011030     C     KFPDP         SETLL     fifpd000
021600011030     C     KFPDP         READE     fifpd000                               33
021700930823     C*
021800930823     C     *IN33         DOWEQ     *OFF
021900930823     C* ESCLUDO ANNULLATI
022000930823     C     FPDATB        IFEQ      ' '
022100930823     C*
022200930921     C* TARIFFA NUOVA NON DEVE ESISTERE: SE ESISTE ANNULLATA DELETE
022300011030     C     KFPDN         SETLL     fifpd000                               31
022400011030     C   31KFPDN         CHAIN     fifpd000                           32
022500940922     C   31
022600011030     CANN32              DELETE    fifpd000
022700930921     C*
022800011030     C     KFPDP2        CHAIN     fifpd000                           32
022900930823     C*
023000930921     C                   Z-ADD     PDRNEW        FPDPDR
023100931013     C                   MOVEL     SMLNEW        FPDSML
023200931013     C                   MOVEL     TSRNEW        FPDTSR
023300930823     C                   Z-ADD     CTRNEW        FPDCTR
023400930823     C                   Z-ADD     PRGNEW        FPDPRG
023500091210     C******             MOVEL     'T'           FPDFTR
023600091210     C******             MOVEL     *ALL'9'       FPDDTR
023700930823     C*
023800011030     C                   WRITE     fifpd000
023900930823     C                   ENDIF
024000930823     C*
024100011030     C     KFPDP         READE     fifpd000                               33
024200930823     C                   ENDDO
024300930823     C*
024400930921     C* TARIFFA NUOVA NON DEVE ESISTERE: SE ESISTE ANNULLATA DELETE
024500011030     C     KFPTN         SETLL     fifpt000                               34
024600011030     C   34KFPTN         CHAIN     fifpt000                           32
024700940922     C   34
024800011030     CANN32              DELETE    fifpt000
024900930921     C*
025000011030     C     KFPTP         CHAIN     fifpt000                           32
025100930823     C*
025200930921     C                   Z-ADD     PDRNEW        FPTPDR
025300931013     C                   MOVEL     SMLNEW        FPTSML
025400931013     C                   MOVEL     TSRNEW        FPTTSR
025500930823     C                   Z-ADD     CTRNEW        FPTCTR
025600930823     C                   Z-ADD     PRGNEW        FPTPRG
025700091210     C******             MOVEL     'T'           FPTFTR
025800091210     C******             MOVEL     *ALL'9'       FPTDTR
025900930823     C*
026000011030     C                   WRITE     fifpt000
026001130617     c                   endif
026100930823     C                   ENDIF
026200930823     C*
026300011030     C     KFGTP         READE     fifpt000                               30
026400930823     C                   ENDDO
026500930823     C*
026600930823     C     FINE          TAG
026700930823     C                   SETON                                        LR
