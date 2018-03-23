000100090112     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020219     H* ficn13R *----------------------------------------------------*
000300930823     H*         - DUPLICAZIONE TARIFFE PADRONCINI -  BATCH           *
000400000000     H*--------------------------------------------------------------*
000500090112     Ffitgt01L  iF   E           K DISK
000600090112     Ffifgt01L  UF A E           K DISK    commit
000700090112     Ffifpt01L  UF A E           K DISK    commit
000800090112     Ffifpd01L  UF A E           K DISK    commit
000900090112     d ficn06ds      e ds
001000931013     D*
001100900924     D WLBDAT          DS
001200940922     D  G02DAT                 1      8  0
001300940922     D  G02INV                 9     16  0
001400940922     D  G02ERR                17     17
001500940922     D  G02TGI                18     22  0
001600900924     D WGIDAT          DS
001700940922     D  GIODAT                 1      8  0
001800940922     D  GIOINV                 9     16  0
001900940922     D  GIOTGI                17     21  0
002000930823     D KPJBA         E DS
002100930823     C*****************************************************************
002200930823     C* RIEPILOGO INDICATORI
002300930823     C*****************************************************************
002400930824     C* 30/33 - LETTURE
002500930824     C* 31/34 - DI COMODO
002600090112     C     Ktgtn         CHAIN     fitgt000                           32
002700090112     C   32              GOTO      FINE
002800930823     C*
002900090112     C     KFGTP         CHAIN     fifgt000                           32
003000930823     C   32              GOTO      FINE
003100930823     C*
003200930823     C                   MOVEL     ' '           FGTATB
003300090112     C                   Z-ADD     comNEW        FGTPDR
003400931013     C                   MOVEL     SMLNEW        FGTSML
003500090112     C                   MOVEL     TSRold        FGTTSR
003600090112     C                   Z-ADD     CTRold        FGTCTR
003700930823     C                   Z-ADD     PRGNEW        FGTPRG
003800090112     C* DATi da supertestata
003900090112     C                   Z-ADD     tgtddt        FGTDDT
004000090112     C                   Z-ADD     tgtdst        FGTDST
004100090112     c                   movel     tgtdiv        fgtdiv
004200940922     C                   Z-ADD     WUDA          FGTDUV
004300091210     C********           MOVEL     'T'           FGTFTR
004400091210     C********           z-add     *all'9'       FGTDTR
004500020315     C                   z-add     0             FGTDTs
004600020315     C                   z-add     0             FGTDcn
004700011030     C                   WRITE     fifgt000
004800930823     C*
004900090112     C*****    D U P L I C A    D I    F I F P T / F I F P D    *****
005000011030     C     KFGTP         SETLL     fifpt000
005100011030     C     KFGTP         READE     fifpt000                               30
005200930823     C*
005300930823     C     *IN30         DOWEQ     *OFF
005400930823     C* ESCLUDO ANNULLATI
005500930823     C     FPTATB        IFEQ      ' '
005600930823     C*
005700011030     C     KFPDP         SETLL     fifpd000
005800011030     C     KFPDP         READE     fifpd000                               33
005900930823     C*
006000930823     C     *IN33         DOWEQ     *OFF
006100930823     C* ESCLUDO ANNULLATI
006200930823     C     FPDATB        IFEQ      ' '
006300930823     C*
006400930921     C* TARIFFA NUOVA NON DEVE ESISTERE: SE ESISTE ANNULLATA DELETE
006500011030     C     KFPDN         SETLL     fifpd000                               31
006600011030     C   31KFPDN         CHAIN     fifpd000                           32
006700940922     C   31
006800011030     CANN32              DELETE    fifpd000
006900930921     C*
007000011030     C     KFPDP2        CHAIN     fifpd000                           32
007100930823     C*
007200090112     C                   Z-ADD     comNEW        FPDPDR
007300931013     C                   MOVEL     SMLNEW        FPDSML
007400090112     C                   MOVEL     TSRold        FPDTSR
007500090112     C                   Z-ADD     CTRold        FPDCTR
007600930823     C                   Z-ADD     PRGNEW        FPDPRG
007700091210     C********           MOVEL     'T'           FPDFTR
007800091210     C********           MOVEL     *ALL'9'       FPDDTR
007900930823     C*
008000011030     C                   WRITE     fifpd000
008100930823     C                   ENDIF
008200930823     C*
008300011030     C     KFPDP         READE     fifpd000                               33
008400930823     C                   ENDDO
008500930823     C*
008600930921     C* TARIFFA NUOVA NON DEVE ESISTERE: SE ESISTE ANNULLATA DELETE
008700011030     C     KFPTN         SETLL     fifpt000                               34
008800011030     C   34KFPTN         CHAIN     fifpt000                           32
008900940922     C   34
009000011030     CANN32              DELETE    fifpt000
009100930921     C*
009200011030     C     KFPTP         CHAIN     fifpt000                           32
009300930823     C*
009400090112     C                   Z-ADD     comNEW        FPTPDR
009500931013     C                   MOVEL     SMLNEW        FPTSML
009600090112     C                   MOVEL     TSRold        FPTTSR
009700090112     C                   Z-ADD     CTRold        FPTCTR
009800930823     C                   Z-ADD     PRGNEW        FPTPRG
009900091210     C********           MOVEL     'T'           FPTFTR
010000091210     C********           MOVEL     *ALL'9'       FPTDTR
010100930823     C*
010200011030     C                   WRITE     fifpt000
010300930823     C                   ENDIF
010400930823     C*
010500011030     C     KFGTP         READE     fifpt000                               30
010600930823     C                   ENDDO
010700930823     C*
010800930823     C     FINE          TAG
010900930823     C                   SETON                                        LR
011000090112     C*****************************************************************
011100090112     c     *inzsr        begsr
011200090112     C*****************************************************************
011300090112     C     *ENTRY        PLIST
011400090112     C                   PARM                    KPJBA
011500090112     C                   MOVEL     KPJBU         ficn06ds
011600090112     C                   MOVEL     FILNEW        COMNEW            7 0
011700090112     C                   MOVE      PDRNEW        COMNEW
011800090112     C                   MOVEL     FILOLD        COMOLD            7 0
011900090112     C                   MOVE      PDROLD        COMOLD
012000090112     c
012100090112     C*---------------------------------------------------------------*
012200090112     C* ACCESSO fitgt
012300090112     C     KtGTN         KLIST
012400090112     C                   KFLD                    comNEW
012500090112     C                   KFLD                    SMLNEW
012600090112     C                   KFLD                    prgNEW
012700090112     C* ACCESSO fifgt E fifpt
012800090112     C     KFGTP         KLIST
012900090112     C                   KFLD                    comOLD
013000090112     C                   KFLD                    SMLOLD
013100090112     C                   KFLD                    TSROLD
013200090112     C                   KFLD                    CTROLD
013300090112     C                   KFLD                    PRGOLD
013400090112     C* ACCESSO fifpd
013500090112     C     KfpdN         KLIST
013600090112     C                   KFLD                    comNEW
013700090112     C                   KFLD                    SMLNEW
013800090112     C                   KFLD                    tsrold
013900090112     C                   KFLD                    CTRold
014000090112     C                   KFLD                    PRGNEW
014100090112     C                   KFLD                    FPDctd
014200090112     C                   KFLD                    FPDCAP
014300090112     C                   KFLD                    FPDSGL
014400090112     C     KFPDP         KLIST
014500090112     C                   KFLD                    comOLD
014600090112     C                   KFLD                    SMLOLD
014700090112     C                   KFLD                    TSROLD
014800090112     C                   KFLD                    CTROLD
014900090112     C                   KFLD                    PRGOLD
015000090112     C                   KFLD                    FPTctd
015100090112     C     KFPDP2        KLIST
015200090112     C                   KFLD                    comOLD
015300090112     C                   KFLD                    SMLOLD
015400090112     C                   KFLD                    TSROLD
015500090112     C                   KFLD                    CTROLD
015600090112     C                   KFLD                    PRGOLD
015700090112     C                   KFLD                    FPDctd
015800090112     C                   KFLD                    FPDCAP
015900090112     C                   KFLD                    FPDSGL
016000090112     C* ACCESSO fifpt
016100090112     C     KFPTN         KLIST
016200090112     C                   KFLD                    comNEW
016300090112     C                   KFLD                    SMLNEW
016400090112     C                   KFLD                    TSRold
016500090112     C                   KFLD                    CTRold
016600090112     C                   KFLD                    PRGNEW
016700090112     C                   KFLD                    FPTctd
016800090112     C     KFPTP         KLIST
016900090112     C                   KFLD                    comOLD
017000090112     C                   KFLD                    SMLOLD
017100090112     C                   KFLD                    TSROLD
017200090112     C                   KFLD                    CTROLD
017300090112     C                   KFLD                    PRGOLD
017400090112     C                   KFLD                    FPTctd
017500090112     C*
017600090112     C* DEFINIZIONE CAMPI
017700090112     C     *LIKE         DEFINE    FGTDDT        WUDA                           *AAAA/MM/GG
017800090112     C*
017900090112     C*---------------------------------------------------------------*
018000090112     C***
018100090112     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
018200090112     C***
018300090112     C                   TIME                    W0120            12 0
018400090112     C                   MOVE      W0120         WDAT              6 0
018500090112     C*
018600090112     C                   Z-ADD     WDAT          G02DAT
018700090112     C                   MOVEL     *BLANK        G02ERR
018800090112     C                   CALL      'XSRDA8'
018900090112     C                   PARM                    WLBDAT
019000090112     C* UDATE A 8 IN AAAA/MM/GG
019100090112     C                   Z-ADD     G02INV        WUDA
019200090112     c                   endsr
