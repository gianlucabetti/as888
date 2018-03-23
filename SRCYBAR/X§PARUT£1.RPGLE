000100881114     H DECEDIT(',') DFTNAME(CN0103) DATEDIT(*DMY/)
000200020625      *--------------------------------------------------------
000300881114     FPARUT00F  IF   F  163     3AIDISK    KEYLOC(11)
000400020423     FAZUTE01L  IF   E           K DISK    USROPN
000500020625      *--------------------------------------------------------
000600020422     D CMD             S             35    DIM(1) CTDATA PERRCD(1)
000700881114     D KEY             DS
000800881114     D  KUT                    1      1P 0
000900881114     D  TIP                    2      3P 0
001000020625      *
001100020625     D ut§dse0f      E DS
001200020625     D  DCU                   98    397    DIM(50)
001300020625     D  TCU                  398    697    DIM(50)
001400020625      *
001500020422     D CNCR80        E DS
001600020423     D  REM                   58     60
001700020423     D  REMFIL                61     63
001800020625      *
001900881114     D                 DS
002000881114     D  NUMB                   1      4B 0
002100881114     D  NUMA                   1      4
002200020625      *
002300881114     D                 DS
002400881114     D  H                      1      2  0
002500881114     D  M                      3      4  0
002600881114     D  S                      5      6  0
002700881114     D  HMS                    1      6  0
002800020625      *
002900020423     D WLBDAT          DS
003000020423     D  G02DAT                 1      8  0
003100020423     D  G02INV                 9     16  0
003200020423     D  G02ERR                17     17
003300020423     D  G02TGI                18     22  0
003400020625      *
003500020423     D                SDS
003600020423     D  USER                 254    263
003700020625      *
003800020521     D ddatiute      e ds                  prefix(§§_)
003900020625     D fnlv55ds      E DS
004000020625      *
004100020625     D ParUte          s                   like(SimUte)
004200020625     D w0140           s             14  0
004300020625     D wdate           s              8  0
004400020625     D w003a           s              3
004500020625     D comman          s             35
004600020625     D lungh           s             15  5 inz(35)
004700020625      *--------------------------------------------------------
004800881114     IPARUT00F  AA  90   12 D0
004900881114     I                                  1   13  FILL
005000881114     I                                 14  163  CAMPAR
005100881114     I          BB  91   12 D1
005200881114     I                                  1   13  FILL1
005300990413     I                                 14   58  RAGUT
005400881114     I                                 62   77  CDFUT
005500881114     I                                 78  109  FLGUT
005600881114     I          CC  92   12 D2
005700881114     I                                  1   13  FILL2
005800881114     I                                 14  163  KCUDS
005900881114     I          DD  93   12 D3
006000881114     I                                  1   13  FILL3
006100881114     I                                 14   61  DTUDS
006200881114     I                                 62  157  NMUDS
006300891107     I          NS
006400891127     I                                 14  163  CA150
006500020625      *---------------------------------------------------------------*
006600881114     C     *ENTRY        PLIST
006700020625     C                   PARM                    ut§dse0f
006800020718      * imposto subito codut a 1
006900020718     c                   z-add     1             codut
007000020625      *
007100020625     C                   eval      *in30 = *off
007200020422     C                   MOVEL     REC80         CNCR80
007300020625      * Profilo utente - Se non passato lo prendo da infds
007400020423    2C     SIMUTE        IFEQ      *BLANKS
007500020423     C                   MOVE      USER          PARUTE
007600020423     C                   ELSE
007700020423     C                   MOVE      SIMUTE        PARUTE
007800020423    2C                   ENDIF
007900020625      *
008000020625     C                   TIME                    W0140
008100020423     C                   CLEAR                   WLBDAT
008200020423     C                   MOVE      W0140         G02DAT
008300020423     C                   CALL      'XSRDA8'
008400020423     C                   PARM                    WLBDAT
008500020625     C                   Z-ADD     G02INV        WDATE
008600020625      *
008700881114     C     DCU(50)       CASNE     *BLANK        ROUT
008800881114     C                   END
008900881114     C*--------- O.K.
009000881114     C                   Z-ADD     CODUT         KUT
009100881114     C                   Z-ADD     1             TIP
009200881114     C     KEY           CHAIN     PARUT00F                           70
009300881114     C  N70              MOVEA     CAMPAR        DCU
009400881114     C                   Z-ADD     2             TIP
009500881114     C     KEY           CHAIN     PARUT00F                           70
009600881114     C  N70              MOVEA     CAMPAR        DCU(26)
009700881114     C                   Z-ADD     3             TIP
009800881114     C     KEY           CHAIN     PARUT00F                           70
009900881114     C  N70              MOVEA     CAMPAR        TCU
010000881114     C                   Z-ADD     4             TIP
010100881114     C     KEY           CHAIN     PARUT00F                           70
010200881114     C  N70              MOVEA     CAMPAR        TCU(26)
010300891107     C*
010400891107     C* INSERIMENTO CAMPI NUOVI
010500891107     C                   Z-ADD     40            TIP
010600891107     C     KEY           CHAIN     PARUT00F                           70
010700891107     C  N70              MOVEL     CA150         REC40
010800891107     C                   Z-ADD     41            TIP
010900891107     C     KEY           CHAIN     PARUT00F                           70
011000891107     C  N70              MOVEL     CA150         REC41
011100891107     C                   Z-ADD     42            TIP
011200891107     C     KEY           CHAIN     PARUT00F                           70
011300891107     C  N70              MOVEL     CA150         REC42
011400891107     C                   Z-ADD     80            TIP
011500891107     C     KEY           CHAIN     PARUT00F                           70
011600891107     C  N70              MOVEL     CA150         REC80
011700891107     C*
011800881114     C                   Z-ADD     10            TIP
011900881114     C     KEY           CHAIN     PARUT00F                           70
012000881114     C                   Z-ADD     20            TIP
012100881114     C     KEY           CHAIN     PARUT00F                           70
012200881114     C                   Z-ADD     30            TIP
012300881114     C     KEY           CHAIN     PARUT00F                           70
012400881114     C   70              Z-SUB     1             CODUT
012500881114     C*--- FINE ------------------------------------------------------*
012600881114     C     FINE          TAG
012700881114     C   30              MOVE      NUMA          DCU(50)
012800020423     C                   MOVEL     REC80         CNCR80
012900020423     C                   MOVE      PARUTE        SIMUTE
013000020423     C                   CLEAR                   SIMPOU
013100020423     C                   CLEAR                   SIMTPP
013200020625      * VEDO SE ESISTE AREA DATI §DATIUTE
013300020521     c     *dtaara       define    §datiute      ddatiute
013400020521     C                   in(E)     *dtaara
013500020625      *
013600020625    1C                   IF        %error  or §§_RSUT = *blanks
013700020521     C* AREA DATI NON PRESENTE:
013800020521     C* ......... VEDO SE PRESENTE FILE AZUTE
013900020625      *
014000020625     C                   MOVEL     CMD(1)        COMMAN
014100020423     C                   CALL      'QCMDEXC'                            32
014200020423     C                   PARM                    COMMAN
014300020423     C                   PARM                    LUNGH
014400020625      * PRESENTE AZUTE
014500020625    2C     *IN32         IFEQ      *OFF
014600020423     C                   OPEN      AZUTE01L
014700020625     C     SIMUTE        CHAIN     AZUTE01L
014800020625    3C                   IF        %found(AZUTE01L)
014900020423     C                   Z-ADD     UTEFIL        SIMPOU
015000020521     C* Se simfel di parut <> 0 richiamo fnlv55r
015100020625    4C     SIMfel        IFNE      *zeros
015200020625     C                   CLEAR                   fnlv55ds
015300020423     C                   MOVEL     'P'           D55TPT
015400020423     C                   Z-ADD     UTEFIL        D55LIN
015500020423     C                   Z-ADD     WDATE         D55DRF
015600020423     C                   CALL      'FNLV55R'
015700020625     C                   PARM                    fnlv55ds
015800020423     C                   Z-ADD     D55TFP        SIMFEL
015900020625      *
016000020625    5C                   SELECT
016100020625     c                   WHEN      D55ERR <> *blanks
016200020625     c                   WHEN      SIMFEL = SIMPOU
016300020625     C                   movel     '1'           SIMTPP
016400020625     C                   OTHER
016500020625     C                   movel     '2'           SIMTPP
016600020625    5C                   ENDSL
016700020625      *
016800020625   X4C                   ELSE
016900020521     C* Se simfel di parut = 0 tengo simfel = 0 e simtpp = 'S'
017000020423     C                   MOVEL     'S'           SIMTPP
017100020625    4C                   ENDIF
017200020625    3C                   ENDIF
017300020625      *
017400020625      * NON PRESENTE AZUTE
017500020625   X2C                   ELSE
017600020625    3C                   if        REM = 'REM'
017700020625     C                   move      REMFIL        SIMPOU
017800020625   x3C                   ELSE
017900020625     C                   z-add     SIMFEL        SIMPOU
018000020625    3C                   ENDIF
018100020625      *
018200020625    3C                   SELECT
018300020625     C                   WHEN      SIMFEL = *ZEROS
018400020625     C                   movel     'S'           SIMTPP
018500020625     C                   WHEN      SIMFEL = SIMPOU
018600020625     C                   movel     '1'           SIMTPP
018700020423     C                   OTHER
018800020625     C                   movel     '2'           SIMTPP
018900020625    3C                   ENDSL
019000020625      *
019100020625    2C                   ENDIF
019200020625      *
019300020625      * Trucco RAGUT inserendo il p.o. utente
019400020625     C                   movel     SimPou        w003a
019500020625     C                   eval      RAGUT = %trim(Ragut) + ' (' +
019600020625     c                                     W003A + ')'
019700020625      *
019800020625      * se trovata area dati §datiute prendo i dati da quest'ultima
019900020625   x1c                   ELSE
020000020521     c                   z-add     §§_simfel     simfel
020100020521     c                   z-add     §§_dutpou     simpou
020200020521     c                   movel     §§_dutlpo     simtpp
020300020625     c                   movel(P)  §§_rsut       ragut
020400020625    1c                   endif
020500020625      *
020600020423     C                   MOVEL     CNCR80        REC80
020700020625      *
020800020625     C                   eval      *inLR = *on
020900020625      *---------------------------------------------------------------*
021000881114     C     ROUT          BEGSR
021100020625      *
021200881114     C                   MOVE      DCU(50)       NUMA
021300881114     C                   Z-ADD     NUMB          HMS
021400881114     C     H             ADD       M             CT                9 4
021500881114     C     CT            DIV       2             CT1               2 0
021600881114     C                   MVR                     REST              1 0
021700881114     C     REST          IFEQ      0
021800881114     C     CT            MULT      S             CT
021900881114     C                   ELSE
022000881114     C     CT            DIV       S             CT
022100881114     C                   END
022200881114     C                   Z-ADD     CT            NUMB
022300020625     C                   eval      *in30 = *on
022400020625      *
022500881114     C                   ENDSR
022600020625      *---------------------------------------------------------------*
022700020422**
022800020422CHKOBJ OBJ(AZUTE00F) OBJTYPE(*FILE)
