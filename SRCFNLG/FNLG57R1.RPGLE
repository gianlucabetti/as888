000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200900418     H*                                                              *
000300121218     H*   - GIACENZE CLIENTE -  crea file di work
000400900423     H*                                                              *
000500900418     H*--------------------------------------------------------------*
000600121219     Ftigcp53l  iF   E           k disk    usropn
000700121219     Ftigcp52l  iF   E           k disk    usropn
000800121219     F                                     rename(TIGCP000:TIGCP052)
000900121218     FtiGNP01L  iF   E           K DISK
001000121003     Ftitas30c  iF   E           k disk    USROPN
001100121005     F                                     ignore(titasp00)
001200121218     Ftita430c  iF   E           k disk    USROPN
001300121002     FCNACO00F  IF   E           K DISK
001400120928     Ftabel00f  IF   E           K DISK
001500121218     Fwtigcp02f uf a E           k disk
001600120928      *-----------------------------------------------------
001700900418     D KPJBA         E DS
001800121001     D Dspr          E DS
001900121001     D Ds14          E DS
002000121218     D fnlg57ds      e ds
002100121218     D dta4a         e ds
002200121001     d coman           s             99
002300121001     d luncmd          s             15  5
002400121001     d file            s             10
002500121001     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002600121001     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002700121001     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002800121003     D Cmd7            S             49    dim(1) ctdata perrcd(1)
002900121003     D Cmd8            S             49    dim(1) ctdata perrcd(1)
003000121218     D Cmd9            S             49    dim(1) ctdata perrcd(1)
003100121218     D Cm10            S             49    dim(1) ctdata perrcd(1)
003200121003     D Cmd             S             49
003300121001     d dataiso         s               d   datfmt(*iso)
003400121001     d dataeur         s               d   datfmt(*eur)
003500121001     d dataape         s               d   datfmt(*iso)
003600121001     d datachiu        s               d   datfmt(*iso)
003700121001     d dexreg          s             25
003800121003     d exreg           s              3  0
003900121001     d giorni          s              5  0
004000121001     d giornitot       s              6  0
004100121001     d diffe           s              5  0
004200121001     d giacres         s              5  0
004300121001     d giaccon         s              5  0
004400121001     d giactot         s              5  0
004500121003     d giacrest        s              6  0
004600121003     d giaccont        s              6  0
004700121003     d giactott        s              6  0
004800121001     d media           s              5  0
004900121001     d data8           s              8  0
005000121001     d datas1          s              8  0
005100121001     d datas2          s              8  0
005200121219     d wAnno4          s              4  0
005300121219
005400121218     D KTRC            S                   LIKE(TA4trc)
005500121218
005600121218     D                 DS
005700121218     D  gcpagc                 1      4  0
005800121218     D  gcpmgc                 5      8  0
005900121218     D  gcpdat                 1      8  0
006000121219
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
008200121219      *genera wrkfile
008300120928     c                   exsr      elabora
008400121001      *
008500121218
008600121001     c                   seton                                        lr
008700120928      *
008800120928     C*-----------------------------------------------------*
008900120928     c     elabora       begsr
009000120928     C*-----------------------------------------------------*
009100121219     c                   if        D57ksc <> *zero
009200121219     c                   open      TIGCP53L
009300121218     c     d57ksc        setll     tigcp53l
009400121219     c                   else
009500121219     c                   open      TIGCP52L
009600121219     c                   eval      wAnno4 = D57dataD / 10000
009700121219     c     wAnno4        setll     TIGCP052
009800121219     c                   endif
009900121219      *
010000121218    1c                   do        *hival
010100121219      *
010200121219  1.5c                   if        D57ksc <> *zero
010300121218     c     d57ksc        reade     tigcp53l
010400121218    2c                   if        %eof(tigcp53l)
010500120928     c                   leave
010600121218    2c                   endif
010700121219     c                   else
010800121219     c                   read      TIGCP052
010900121219    2c                   if        %eof(TIGCP52L)
011000121219     c                   leave
011100121219    2c                   endif
011200121219  1.5c                   endif
011300121219
011400121219     c                   if        D57nrs <> 99  and
011500121219     c                             GCPnrs <> D57nrs
011600121219     c                   iter
011700121219     c                   endif
011800121218
011900121218    2c                   if        gcpdat >=d57datad AND
012000121218     c                             gcpdat<= d57dataa
012100121218
012200121218     c* Se richiesta la filiale la controllo
012300121219  2.5c                   if        d57fil=0 or d57fil= gcpfgc
012400121219      *
012500120928     c     kbol          chain     titas30c
012600121218    3c                   if        %found(titas30c)
012700121218     c                   eval      wrmn=tasrmn
012800121218     c* rif alfa
012900121218     c                   eval      ktrc = 'A'
013000121218     c     KTAStrc       CHAIN     TITA430C
013100121218    4c                   If        %FOUND
013200121218     c                   movel     TA4not        DTA4A
013300121218     c                   movel     §TA4ARMA      wrma
013400121218     c                   else
013500121218     c                   clear                   wrma
013600121218    4c                   endif
013700121218     c                   eval      waas=tasaas
013800121218     c                   eval      wmgs=tasmgs
013900121218
014000121218     C* CODICE MOTIVO GIACENZA
014100121218     C                   MOVEL     '2A'          TBLCOD
014200121218     C                   MOVEL     *BLANK        TBLKEY
014300121218     C                   MOVEL     gcpcmc        TBLKEY
014400121218     C     KTAB          CHAIN     TABEL                              21
014500121218     C   21              MOVEL     *ALL'?'       wDCMC
014600121218     C  N21              MOVEL     TBLUNI        wDCMC
014700121218     C* ULTERIORE MOTIVO
014800121218     C                   Z-ADD     10            §FAS              3 0
014900121218     C     KNOT          CHAIN     tignp01L                           21
015000121218     C  N21              MOVEL     GNPDMC        wVDMC
015100121218     C   21              MOVEL     *BLANKS       wVDMC
015200121219
015300121219     C* DISPOSIZIONE
015400121219     C                   MOVEL     '2D'          TBLCOD
015500121219     C                   MOVEL     *BLANK        TBLKEY
015600121219     C                   MOVEL     GCPdis        TBLKEY
015700121219     C     KTAB          CHAIN     TABEL                              21
015800121219     C   21              MOVEL     *ALL'?'       wDIS
015900121219     C  N21              MOVEL     TBLUNI        wDIS
016000121218
016100121218     c                   write     wtigcp00
016200121218    3c                   endif
016300121219      *
016400121219  2.5c                   endif
016500121219      *
016600121218    2c                   endif
016700121219      *
016800121218    1c                   enddo
016900121219      *
017000120928     c                   endsr
017100121001     c*-------------------------------------------------
017200121001     c     comandi       begsr
017300121001     c*-------------------------------------------------
017400121218     C                   close     wtigcp02f
017500121001      *
017600121001     C                   z-add     99            luncmd
017700121218     C                   CALL      'QCMDEXC'                            34
017800121001     C                   parm                    coman
017900121001     C                   parm                    luncmd
018000121001      *
018100121218     C                   OPEN      wtigcp02f
018200121001     c                   endsr
018300121001     c*-------------------------------------------------
018400121001     c     comandi2      begsr
018500121001     c*-------------------------------------------------
018600121001      *
018700121001     C                   z-add     99            luncmd
018800121001     C                   CALL      'QCMDEXC'                            34
018900121001     C                   parm                    coman
019000121001     C                   parm                    luncmd
019100121001      *
019200121001     c                   endsr
019300940926     C*-----------------------------------------------------*
019400940926     C*  Operazioni iniziali
019500940926     C*------------------------------------------------------*
019600940926     C     *INZSR        BEGSR
019700940926     C*
019800940926     C     *ENTRY        PLIST
019900121003     C                   PARM                    kpjba
020000121218     C                   movel     kpjbu         fnlg57ds
020100121002     C                   Z-ADD     1             CODUT
020200121002     C                   CALL      'X§PARUT'
020300121002     C                   PARM                    UTEDSE
020400121002     C                   MOVEL     RAGUT         RSUT             20
020500121002     C*--- RICERCA CAPOCONTI------------------------------------------*
020600121002     C                   DO        50            X                 2 0
020700121002     C                   MOVE      TCU(X)        TCUDS
020800121002     C     F56           IFEQ      'CG'
020900121002     C     F34           ANDEQ     '01'
021000121002     C                   Z-ADD     KCU(X)        KCI               4 0
021100121002     C                   END
021200121002     C                   END
021300120928     c*
021400120928     c     ktab          klist
021500120928     c                   kfld                    tblkut
021600120928     c                   kfld                    tblcod
021700120928     c                   kfld                    tblkey
021800120928     c                   z-add     1             tblkut
021900121218
022000121218     C     KNOT          KLIST
022100121218     C                   KFLD                    gcpAGC
022200121218     C                   KFLD                    gcpFGC
022300121218     C                   KFLD                    gcpNGC
022400121218     C                   KFLD                    gcpFRG
022500121218     C                   KFLD                    §FAS
022600121002      *
022700120928     c     kbol          klist
022800120928     c                   kfld                    gcpaas
022900120928     c                   kfld                    gcplnp
023000120928     c                   kfld                    gcpnrs
023100120928     c                   kfld                    gcpnsp
023200121218     C     ktastrc       KLIST
023300121218     C                   KFLD                    tasAAS
023400121218     C                   KFLD                    tasLNP
023500121218     C                   KFLD                    tasNRS
023600121218     C                   KFLD                    tasNSP
023700121218     C                   KFLD                    Ktrc
023800121002      *
023900121002     C     kaco          klist
024000121002     C                   kfld                    codut
024100121002     C                   kfld                    kci
024200121218     C                   kfld                    d57ksc
024300121002      *
024400121218     c                   move      d57datad      dataiso
024500121001     c                   move      dataiso       dataeur
024600121001     c                   move      dataeur       datas1
024700121218     c                   move      d57dataa      dataiso
024800121001     c                   move      dataiso       dataeur
024900121001     c                   move      dataeur       datas2
025000121001     C                   time                    w0120            14 0
025100121001     C                   move      w0120         wdat              8 0
025200121001     C                   movel     w0120         wora              6 0
025300120928      *
025400121002     C     KACO          CHAIN     CNACO000                           40
025500121002     c                   movel     acorag        acorag30         30
025600121003     c                   exsr      comando
025700121003     C                   open      titas30c
025800121218     C                   open      tita430c
025900940926     C                   ENDSR
026000121003      *---------------------------------------------------------------------
026100121003     C     comando       BEGSR
026200121003      *---------------------------------------------------------------------
026300121003     c                   if        %subst(knsif:7:1) = 'P'
026400121003     C                   movea     cmd7          cmd
026500121003     c                   else
026600121003     C                   movea     cmd8          cmd
026700121003     c                   endif
026800121003     C                   movel(p)  cmd           comman
026900121003     C                   call      'QCMDEXC'
027000121003     C                   parm                    comman           80
027100121003     C                   parm      49            lung             15 5
027200121003     C*
027300121218     c                   if        %subst(knsif:7:1) = 'P'
027400121218     C                   movea     cmd9          cmd
027500121218     c                   else
027600121218     C                   movea     cm10          cmd
027700121218     c                   endif
027800121218     C                   movel(p)  cmd           comman
027900121218     C                   call      'QCMDEXC'
028000121218     C                   parm                    comman           80
028100121218     C                   parm      49            lung             15 5
028200121003     C                   ENDSR
028300121001      *--------------------------------------------------------------
028400121001**  CMD1
028500121218CLRPFM    FILE(WTIGCP02F)
028600121001**  CMD2
028700121219CRTDUPOBJ OBJ(WTIGCP02F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
028800121001**  CMD3
028900121218DLTF FILE(QTEMP/WTIGCP02F)
029000121003**  CMD7
029100121003OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
029200121003**  CMD8
029300121003OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRU/TITAS30C)
029400121218**  CMD9
029500121218OVRDBF FILE(TITA430C) TOFILE(GAITRAGRPS/TITA430C)
029600121218**  CM10
029700121218OVRDBF FILE(TITA430C) TOFILE(GAITRAGRU/TITA430C)
