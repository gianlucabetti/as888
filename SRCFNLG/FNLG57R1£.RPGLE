000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200900418     H*                                                              *
000300121218     H*   - GIACENZE CLIENTE -  crea file di work
000400900423     H*                                                              *
000500900418     H*--------------------------------------------------------------*
000600120928     Ftigcp53l  iF   E           k disk
000700121218     FtiGNP01L  iF   E           K DISK
000800121003     Ftitas30c  iF   E           k disk    USROPN
000900121005     F                                     ignore(titasp00)
001000121218     Ftita430c  iF   E           k disk    USROPN
001100121002     FCNACO00F  IF   E           K DISK
001200120928     Ftabel00f  IF   E           K DISK
001300121218     Fwtigcp02f uf a E           k disk
001400120928      *-----------------------------------------------------
001500900418     D KPJBA         E DS
001600121001     D Dspr          E DS
001700121001     D Ds14          E DS
001800121218     D fnlg57ds      e ds
001900121218     D dta4a         e ds
002000121001     d coman           s             99
002100121001     d luncmd          s             15  5
002200121001     d file            s             10
002300121001     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002400121001     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002500121001     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002600121001     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002700121001     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002800121001     D cmd6            S              1    DIM(99) CTDATA PERRCD(99)
002900121003     D Cmd7            S             49    dim(1) ctdata perrcd(1)
003000121003     D Cmd8            S             49    dim(1) ctdata perrcd(1)
003100121218     D Cmd9            S             49    dim(1) ctdata perrcd(1)
003200121218     D Cm10            S             49    dim(1) ctdata perrcd(1)
003300121003     D Cmd             S             49
003400121001     d dataiso         s               d   datfmt(*iso)
003500121001     d dataeur         s               d   datfmt(*eur)
003600121001     d dataape         s               d   datfmt(*iso)
003700121001     d datachiu        s               d   datfmt(*iso)
003800121001     d dexreg          s             25
003900121003     d exreg           s              3  0
004000121001     d giorni          s              5  0
004100121001     d giornitot       s              6  0
004200121001     d diffe           s              5  0
004300121001     d giacres         s              5  0
004400121001     d giaccon         s              5  0
004500121001     d giactot         s              5  0
004600121003     d giacrest        s              6  0
004700121003     d giaccont        s              6  0
004800121003     d giactott        s              6  0
004900121001     d media           s              5  0
005000121001     d data8           s              8  0
005100121001     d datas1          s              8  0
005200121001     d datas2          s              8  0
005300121218     d
005400121218     D KTRC            S                   LIKE(TA4trc)
005500121218
005600121218     D                 DS
005700121218     D  gcpagc                 1      4  0
005800121218     D  gcpmgc                 5      8  0
005900121218     D  gcpdat                 1      8  0
006000121218     d
006100121002     D TCUDS           DS
006200121002     D  F34                    3      4
006300121002     D  F56                    5      6
006400121002     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006500121002     D  TCU                  398    697
006600121002     D                                     DIM(50)
006700121002     D  KCU                  698    847P 0
006800121002     D                                     DIM(50)
006900121002     D                                     PACKEVEN
007000121001      *______________________________________________________________________
007100120928      *
007200121001      * creo nome file per dati utente in libreria di wrk
007300121001     c*m                 eval      file = 'AU' + knmus
007400121001      *clear file
007500121218     c                   movea     cmd3          coman
007600121218     c                   exsr      comandi2
007700121218     c                   movea     cmd1          coman
007800121218     c                   exsr      comandi
007900121002     c                   movea     cmd2          coman
008000121002     c                   exsr      comandi
008100121001      *
008200121001      *genera file
008300120928     c                   exsr      elabora
008400121001      *
008500121218
008600121001     c                   seton                                        lr
008700120928      *
008800120928     C*-----------------------------------------------------*
008900120928     c     elabora       begsr
009000120928     C*-----------------------------------------------------*
009100121218     c     d57ksc        setll     tigcp53l
009200121218    1c                   do        *hival
009300121218     c     d57ksc        reade     tigcp53l
009400121218    2c                   if        %eof(tigcp53l)
009500120928     c                   leave
009600121218    2c                   endif
009700121218
009800121218    2c                   if        gcpdat >=d57datad AND
009900121218     c                             gcpdat<= d57dataa
010000121218
010100121218     c* Se richiesta la filiale la controllo
010200121218     c                   if        d57fil=0 or d57fil= gcpfgc
010300120928     c     kbol          chain     titas30c
010400121218    3c                   if        %found(titas30c)
010500121218     c                   eval      wrmn=tasrmn
010600121218     c* rif alfa
010700121218     c                   eval      ktrc = 'A'
010800121218     c     KTAStrc       CHAIN     TITA430C
010900121218    4c                   If        %FOUND
011000121218     c                   movel     TA4not        DTA4A
011100121218     c                   movel     §TA4ARMA      wrma
011200121218     c                   else
011300121218     c                   clear                   wrma
011400121218    4c                   endif
011500121218     c                   eval      waas=tasaas
011600121218     c                   eval      wmgs=tasmgs
011700121218
011800121218     C* CODICE MOTIVO GIACENZA
011900121218     C                   MOVEL     '2A'          TBLCOD
012000121218     C                   MOVEL     *BLANK        TBLKEY
012100121218     C                   MOVEL     gcpcmc        TBLKEY
012200121218     C     KTAB          CHAIN     TABEL                              21
012300121218     C   21              MOVEL     *ALL'?'       wDCMC
012400121218     C  N21              MOVEL     TBLUNI        wDCMC
012500121218     C* ULTERIORE MOTIVO
012600121218     C                   Z-ADD     10            §FAS              3 0
012700121218     C     KNOT          CHAIN     tignp01L                           21
012800121218     C  N21              MOVEL     GNPDMC        wVDMC
012900121218     C   21              MOVEL     *BLANKS       wVDMC
013000121218
013100121218     c                   write     wtigcp00
013200121218    3c                   endif
013300121218    2c                   endif
013400121218    2c                   endif
013500121218
013600121218    1c                   enddo
013700120928     c                   endsr
013800121001     c*-------------------------------------------------
013900121001     c     comandi       begsr
014000121001     c*-------------------------------------------------
014100121218     C                   close     wtigcp02f
014200121001      *
014300121001     C                   z-add     99            luncmd
014400121218     C                   CALL      'QCMDEXC'                            34
014500121001     C                   parm                    coman
014600121001     C                   parm                    luncmd
014700121001      *
014800121218     C                   OPEN      wtigcp02f
014900121001     c                   endsr
015000121001     c*-------------------------------------------------
015100121001     c     comandi2      begsr
015200121001     c*-------------------------------------------------
015300121001      *
015400121001     C                   z-add     99            luncmd
015500121001     C                   CALL      'QCMDEXC'                            34
015600121001     C                   parm                    coman
015700121001     C                   parm                    luncmd
015800121001      *
015900121001     c                   endsr
016000940926     C*-----------------------------------------------------*
016100940926     C*  Operazioni iniziali
016200940926     C*------------------------------------------------------*
016300940926     C     *INZSR        BEGSR
016400940926     C*
016500940926     C     *ENTRY        PLIST
016600121003     C                   PARM                    kpjba
016700121218     C                   movel     kpjbu         fnlg57ds
016800121002     C                   Z-ADD     1             CODUT
016900121002     C                   CALL      'X§PARUT'
017000121002     C                   PARM                    UTEDSE
017100121002     C                   MOVEL     RAGUT         RSUT             20
017200121002     C*--- RICERCA CAPOCONTI------------------------------------------*
017300121002     C                   DO        50            X                 2 0
017400121002     C                   MOVE      TCU(X)        TCUDS
017500121002     C     F56           IFEQ      'CG'
017600121002     C     F34           ANDEQ     '01'
017700121002     C                   Z-ADD     KCU(X)        KCI               4 0
017800121002     C                   END
017900121002     C                   END
018000120928     c*
018100120928     c     ktab          klist
018200120928     c                   kfld                    tblkut
018300120928     c                   kfld                    tblcod
018400120928     c                   kfld                    tblkey
018500120928     c                   z-add     1             tblkut
018600121218
018700121218     C     KNOT          KLIST
018800121218     C                   KFLD                    gcpAGC
018900121218     C                   KFLD                    gcpFGC
019000121218     C                   KFLD                    gcpNGC
019100121218     C                   KFLD                    gcpFRG
019200121218     C                   KFLD                    §FAS
019300121002      *
019400120928     c     kbol          klist
019500120928     c                   kfld                    gcpaas
019600120928     c                   kfld                    gcplnp
019700120928     c                   kfld                    gcpnrs
019800120928     c                   kfld                    gcpnsp
019900121218     C     ktastrc       KLIST
020000121218     C                   KFLD                    tasAAS
020100121218     C                   KFLD                    tasLNP
020200121218     C                   KFLD                    tasNRS
020300121218     C                   KFLD                    tasNSP
020400121218     C                   KFLD                    Ktrc
020500121002      *
020600121002     C     kaco          klist
020700121002     C                   kfld                    codut
020800121002     C                   kfld                    kci
020900121218     C                   kfld                    d57ksc
021000121002      *
021100121218     c                   move      d57datad      dataiso
021200121001     c                   move      dataiso       dataeur
021300121001     c                   move      dataeur       datas1
021400121218     c                   move      d57dataa      dataiso
021500121001     c                   move      dataiso       dataeur
021600121001     c                   move      dataeur       datas2
021700121001     C                   time                    w0120            14 0
021800121001     C                   move      w0120         wdat              8 0
021900121001     C                   movel     w0120         wora              6 0
022000120928      *
022100121002     C     KACO          CHAIN     CNACO000                           40
022200121002     c                   movel     acorag        acorag30         30
022300121003     c                   exsr      comando
022400121003     C                   open      titas30c
022500121218     C                   open      tita430c
022600940926     C                   ENDSR
022700121003      *---------------------------------------------------------------------
022800121003     C     comando       BEGSR
022900121003      *---------------------------------------------------------------------
023000121003     c                   if        %subst(knsif:7:1) = 'P'
023100121003     C                   movea     cmd7          cmd
023200121003     c                   else
023300121003     C                   movea     cmd8          cmd
023400121003     c                   endif
023500121003     C                   movel(p)  cmd           comman
023600121003     C                   call      'QCMDEXC'
023700121003     C                   parm                    comman           80
023800121003     C                   parm      49            lung             15 5
023900121003     C*
024000121218     c                   if        %subst(knsif:7:1) = 'P'
024100121218     C                   movea     cmd9          cmd
024200121218     c                   else
024300121218     C                   movea     cm10          cmd
024400121218     c                   endif
024500121218     C                   movel(p)  cmd           comman
024600121218     C                   call      'QCMDEXC'
024700121218     C                   parm                    comman           80
024800121218     C                   parm      49            lung             15 5
024900121003     C                   ENDSR
025000121001      *--------------------------------------------------------------
025100121001**  CMD1
025200121218CLRPFM    FILE(WTIGCP02F)
025300121001**  CMD2
025400121218CRTDUPOBJ OBJ(WTIGCP02f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
025500121001**  CMD3
025600121218DLTF FILE(QTEMP/WTIGCP02F)
025700121001**  CMD4
025800121001CHKOBJ  OBJ(WRKQUERY/xxxxxxxxxx) OBJTYPE(*FILE)
025900121001**  CMD5
026000121218CRTDUPOBJ OBJ(Wtigcp02f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xx
026100121001**  CMD6
026200121218CPYF FROMFILE(Wtigcp02F) TOFILE(WRKQUERY/xxxxxxxxxx) MBROPT(*REPLACE)
026300121003**  CMD7
026400121003OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
026500121003**  CMD8
026600121003OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRU/TITAS30C)
026700121218**  CMD9
026800121218OVRDBF FILE(TITA430C) TOFILE(GAITRAGRPS/TITA430C)
026900121218**  CM10
027000121218OVRDBF FILE(TITA430C) TOFILE(GAITRAGRU/TITA430C)
