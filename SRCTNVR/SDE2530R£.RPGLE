000100150605     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000200150603      *_________________________________________________________________________
000300150603     ftitas30c  if   e           k disk
000400150603     f                                     ignore(titas000)
000500150603     ftitass0f  if   e             disk
000600150603     ftabel00f  if   e           k disk
000700150603     fazorg01l  if   e           k disk
000800150603     fsisde01l  uf a e           k disk
000900150603     D kpjba         e ds
001000150604     D dsdek30       e ds
001100150604     D ds14          e ds
001200150604     D dstb          e ds
001300150604     D ds2l          e ds
001400150610     D ds2w          e ds
001500150603     d woggi           s              8  0
001600150603     d times           s              6  0
001700150603     d dataora         s             14  0
001800150604     D* FILIALI CON RELATIVO RAGGRUPPAMENTO REGIONE
001900150604     D FIL             S              3  0 DIM(500)
002000150604     D RRE             S              3  0 DIM(500)
002100150604     D RRN             S              3  0 DIM(500)
002200150604     D* SCHIERA TABELLA REGIONI (14) E RELATIVI CODICI RAGGRUPPAMENTO REGIONI
002300150604     D REG             S              3  0 DIM(30)
002400150604     D RAR             S              3  0 DIM(30)
002500150604     D RAN             S              3  0 DIM(30)
002600150604     D* SCHIERA TABELLA FASCIE DI PESO (2L)
002700150604     D FPC             S              2    DIM(20)
002800150604     D KIC             S              6  0 DIM(20)
002900150604     D KFC             S              6  0 DIM(20)
003000150604     D* SCHIERA TABELLA TIPI BOLLA (TB)
003100150604     D TBL             S              2    DIM(50)
003200150604     D TPO             S              1    DIM(50)
003300150604     D RBL             S              1    DIM(50)
003400150610      * schiera spedizioni da considerare 1 flag = S tabella 2W
003500150610     D k2w             S              1    DIM(10)
003600150603      *--------------------------------------------------------------------
003700150604      * reperimento dati da titas30c
003800150604     c     *loval        setll     titas30c
003900150603     c                   do        *hival
004000150604     c                   read      titas30c
004100150608     c                   if        %eof(titas30c)
004200150603     c                   leave
004300150603     c                   end
004400150604     c                   exsr      peso
004500150608      * considero solo quelle che vanno da 25 kg a 30 kg
004600150610     c                   if        wdpes <= 25 or wdpes > 30
004700150608     c                   iter
004800150608     c                   endif
004900150608      *
005000150604     c                   exsr      volume
005100150604     c                   exsr      preparakey
005200150610      * considero solo quelle che vanno da 25 kg a 30 kg
005300150610     c     wrbl          lookup    k2w                                    99
005400150610     c  n99              iter
005500150604     c                   exsr      cercasde
005600150604     c                   enddo
005700150603      * lettura spunte su foglio 8 ricerca corrispondenza
005800150605     c                   if        1 = 2
005900150604     c     *start        setll     titass0f
006000150603     c                   do        *hival
006100150604     c                   read      titass0f
006200150608     c                   if        %eof(titass0f)
006300150603     c                   leave
006400150603     c                   end
006500150604     c                   enddo
006600150605     c                   end
006700150603
006800150603     c                   seton                                        lr
006900150604      *-----------------------------------------------
007000150604     c     preparakey    begsr
007100150604      *-----------------------------------------------
007200150604     C                   clear                   wTPO
007300150604     C                   clear                   wRBL
007400150604     c                   z-add     1             i
007500150609     c     tastbl        lookup    tbl(i)                                 99
007600150609     c                   if        *in99
007700150609     C                   MOVEL     TPO(I)        wTPO
007800150609     C                   MOVEL     RBL(I)        wRBL
007900150609     c                   endif
008000150609     C                   clear                   WDREP
008100150609     c                   z-add     1             i
008200150609     c     taslnp        lookup    fil(i)                                 99
008300150609     c   99              z-add     rrn(i)        wdrep
008400150609     C                   clear                   WDREA
008500150609     c                   z-add     1             i
008600150609     c     taslna        lookup    fil(i)                                 99
008700150609     c   99              z-add     rrn(i)        wdrea
008800150609     c                   endsr
008900150604      *-----------------------------------------------
009000150604     c     peso          begsr
009100150604      *-----------------------------------------------
009200150608     C                   clear                   WdPES
009300150608     C                   clear                   WdPCC
009400150608     C                   clear                   WdPLS
009500150604IF  2C     tasPKC        IFGT      *ZEROS
009600150604     C     tasNCL        ANDEQ     tasNCP
009700150604     C                   Z-ADD     tasPKC        WdPES             9 1
009800150604     C                   Z-ADD     tasPKB        WdPCC             9 1
009900150604     C                   Z-ADD     tasPKC        WdPLS             9 1
010000150604X   2C                   ELSE
010100150604     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
010200150604IF  3C     tasPKF        IFGT      tasPKC
010300150604     C                   Z-ADD     tasPKF        WdPES
010400150604X   3C                   ELSE
010500150604     C                   Z-ADD     tasPKC        WdPES
010600150604E   3C                   ENDIF
010700150604E   2C                   ENDIF
010800150604     C*
010900150604     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
011000150604     C     WdPES         IFEQ      *ZEROS
011100150604     C                   MOVEL     FPC(1)        WDFPC
011200150604     C                   ELSE
011300150604     C                   DO        20            I
011400150604     C     WdPES         IFGT      KIC(I)
011500150604     C     WdPES         ANDLE     KFC(I)
011600150604     C                   MOVEL     FPC(I)        WDFPC
011700150604     C                   LEAVE
011800150604     C                   ENDIF
011900150604     C                   ENDDO
012000150604     C                   ENDIF
012100150604     C*
012200150604     c                   endsr
012300150604      *-----------------------------------------------
012400150604     c     volume        begsr
012500150604      *-----------------------------------------------
012600150608     C                   clear                   WDVOL
012700150608     C                   clear                   WDVCC
012800150608     C                   clear                   WDVLS
012900150604     C     tasFVF        IFEQ      'T'
013000150604     C                   Z-ADD     tasvLF        WDVOL             9 3
013100150604     C                   Z-ADD     tasvLB        WDVCC             9 3
013200150604     C                   Z-ADD     tasvLF        WDVLS             9 3
013300150604     C                   ELSE
013400150604     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
013500150604     C     tasVLC        IFGT      *ZEROS
013600150604     C     tasNCL        ANDEQ     tasNCR
013700150604     C                   Z-ADD     tasVLC        WDVOL
013800150604     C                   Z-ADD     tasVLB        WDVCC
013900150604     C                   Z-ADD     tasVLC        WDVLS
014000150604     C                   ELSE
014100150604     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
014200150604     C     tasVLF        IFGT      tasVLC
014300150604     C                   Z-ADD     tasVLF        WDVOL
014400150604     C                   ELSE
014500150604     C                   Z-ADD     tasVLC        WDVOL
014600150604     C                   ENDIF
014700150604     C                   ENDIF
014800150604     C                   ENDIF
014900150604     c                   endsr
015000150603      *-----------------------------------------------
015100150604     c     cercasde      begsr
015200150603      *-----------------------------------------------
015300150604     C                   movel     'SDE'         SDEFILE
015400150604     C                   move      'K30'         SDETRD
015500150604     C                   Z-ADD     tasAAS        SDeANN
015600150604     C                   movel     tasmgs        SDeMES
015700150604     C                   Z-ADD     tasKSC        SDeKSC
015800150604     C                   Z-ADD     WDREP         SDeREP
015900150604     C                   Z-ADD     WDREA         SDeREA
016000150604     C                   MOVEL     WDFPC         SDeFPE
016100150604     C                   MOVEL     tasTSP        SDeTSP
016200150604     C                   Z-ADD     tasCTR        SDeCTR
016300150604     C                   MOVEL     wTPO          SDeTPO
016400150604     C                   MOVEL     wRBl          SDeRBL
016500150605     C                   MOVEL     *blank        SDeTPR
016600150604     c     ksde          chain     sisde01l
016700150604     c                   if        %found(sisde01l)
016800150604     c                   exsr      aggsde
016900150604     c                   else
017000150604     c                   exsr      wrtsde
017100150604     c                   endif
017200150603     c                   endsr
017300150604      *-----------------------------------------------
017400150604     c     wrtsde        begsr
017500150604      *-----------------------------------------------
017600150605     c                   z-add     0             SDEDLV
017700150605     c                   z-add     0             SDEFIL
017800150604     c                   z-add     taslnp        SDEFLP
017900150604     c                   z-add     taslna        SDEFLA
018000150604     c                   move      tasSV1        SDESVR
018100150604     c                   move      tasFTC        SDEFTC
018200150604     c                   z-add     tasTFP        SDETFP
018300150604     c                   z-add     tasTFA        SDETFA
018400150605     c                   move      *blank        SDEDEC
018500150604     c                   z-add     tasflp        SDETT1
018600150604     c                   z-add     tasfl2        SDETT2
018700150605     c                   clear                   dsdek30
018800150604     c                   z-add     1             §K30NSP
018900150604     c                   z-add     WdPES         §K30PKG
019000150604     c                   z-add     WdPCC         §K30PCC
019100150604     c                   z-add     WdPLS         §K30PLS
019200150604     c                   z-add     tasncl        §K30NCO
019300150604     c                   z-add     WDVOL         §K30VOL
019400150604     c                   z-add     WDVCC         §K30VCC
019500150604     c                   z-add     WDVLS         §K30VLS
019600150604     c                   movel     dsdek30       sdeuni
019700150604     c                   clear                   SDEFTR
019800150604     c                   clear                   SDEDTR
019900150604     c                   write     sisde000
020000150604     c                   endsr
020100150605      *-----------------------------------------------
020200150605     c     aggsde        begsr
020300150605      *-----------------------------------------------
020400150605     c                   movel     sdeuni        dsdek30
020500150605     c                   add       1             §K30NSP
020600150605     c                   add       WdPES         §K30PKG
020700150605     c                   add       WdPCC         §K30PCC
020800150605     c                   add       WdPLS         §K30PLS
020900150605     c                   add       tasncl        §K30NCO
021000150605     c                   add       WDVOL         §K30VOL
021100150605     c                   add       WDVCC         §K30VCC
021200150605     c                   add       WDVLS         §K30VLS
021300150605     c                   movel     dsdek30       sdeuni
021400150608     c                   update    sisde000
021500150605     c                   endsr
021600150603      *-----------------------------------------------
021700150603     c     *inzsr        begsr
021800150603      *-----------------------------------------------
021900150603     c
022000150603     c     *entry        plist
022100150603     c                   parm                    kpjba
022200150605     C     *LIKE         DEFINE    sdetpo        wtpo
022300150605     C     *LIKE         DEFINE    sderbl        wrbl
022400150605     C     *LIKE         DEFINE    SDeREP        WDREP
022500150605     C     *LIKE         DEFINE    SDeREA        WDREA
022600150605     C     *LIKE         DEFINE    SDeFPE        WDFPC
022700150604     C* CHIAVE LETTURA AZORG01L - COMPLETA
022800150604     C     keyorg        klist
022900150605     C                   kfld                    korfil            3 0
023000150603
023100150604     c     ktab          klist
023200150604     c                   kfld                    tblkut
023300150604     c                   kfld                    tblcod
023400150604     c                   kfld                    tblkey
023500150604     c                   z-add     1             tblkut
023600150603
023700150604     c     ksde          klist
023800150604     c                   kfld                    SDEFILE
023900150604     c                   kfld                    SDETRD
024000150604     c                   kfld                    SDEKSC
024100150604     c                   kfld                    SDEANN
024200150604     c                   kfld                    SDEMES
024300150604     c                   kfld                    SDEREP
024400150604     c                   kfld                    SDEREA
024500150604     c                   kfld                    SDEFPE
024600150604     c                   kfld                    SDETSP
024700150604     c                   kfld                    SDECTR
024800150604     c                   kfld                    SDETPO
024900150604     c                   kfld                    SDERBL
025000150604     c                   kfld                    SDETPR
025100150604      *
025200150603     c                   time                    dataora
025300150603     c                   move      dataora       woggi
025400150603     c                   movel     dataora       times
025500150604      *
025600150604     C* CARICA TABELLA -14- REGIONI E RAGGRUPPAMENTI REGIONI
025700150604     C                   Z-ADD     0             I                 4 0
025800150604     C                   MOVEL     '14'          tblCOD
025900150604     C                   MOVEL     *BLANKS       tblKEY
026000150604     C     Ktab          SETLL     TABEL00F                           99
026100150610     C  N99              READ      TABEL00F                               99
026200150604     C     *IN99         DOWEQ     '0'
026300150604     C     TBLCOD        ANDEQ     '14'
026400150604     C     TBLFLG        IFEQ      *BLANKS
026500150604     C                   ADD       1             I
026600150604     C                   MOVEL     TBLKEY        REG(I)
026700150604     C                   MOVEL     TBLUNI        DS14
026800150604     C                   MOVE      §14CRR        RAR(I)
026900150604     C                   MOVE      §14CRN        RAN(I)
027000150604     C                   END
027100150610     C                   READ      TABEL00F                               99
027200150604     C                   END
027300150604     C*
027400150604     C* CARICA ORGANIGRAMMA
027500150604     C                   Z-ADD     *ZEROS        KORFIL
027600150604     C     KEYORG        SETLL     AZORG01L                           99
027700150604     C  N99              READ      AZORG01L                               99
027800150604     C                   Z-ADD     *ZEROS        I
027900150604DO  2C     *IN99         DOWEQ     *OFF
028000150604IF  3C     ORGFVA        IFEQ      *BLANKS
028100150604IF  4C     ORGFAG        IFNE      'V'
028200150604     C                   ADD       1             I
028300150604     C                   Z-ADD     ORGFIL        FIL(I)
028400150604     C* RAGGRUPPAMENTI
028500150604     C                   Z-ADD     1             II                6 0
028600150604     C     ORGCRE        LOOKUP    REG(II)                                98
028700150604IF  5C     *IN98         IFEQ      *on
028800150604     C                   Z-ADD     RAR(II)       RRE(I)
028900150604     C                   Z-ADD     RAN(II)       RRN(I)
029000150604E   5C                   ENDIF
029100150604E   4C                   ENDIF
029200150604E   4C                   ENDIF
029300150604     C                   READ      AZORG01L                               99
029400150604E   2C                   ENDDO
029500150604      * tabella tipi bolla
029600150608     C                   Z-ADD     0             I
029700150605     C                   MOVEL     'TB'          tblCOD
029800150605     C                   MOVEL     *BLANKS       tblKEY
029900150605     C     Ktab          SETLL     TABEL00F                           99
030000150610     C  N99              READ      TABEL00F                               99
030100150604     C     *IN99         DOWEQ     '0'
030200150604     C     TBLCOD        ANDEQ     'TB'
030300150604     C     TBLFLG        IFEQ      *BLANKS
030400150604     C                   ADD       1             I
030500150604     C                   MOVEL     TBLKEY        TBL(I)
030600150604     C                   MOVEL     TBLUNI        DSTB
030700150604     C                   MOVEL     §TBTPO        TPO(I)
030800150604     C                   MOVEL     §TBRBL        RBL(I)
030900150604     C                   END
031000150610     C                   READ      TABEL00F                               99
031100150604     C                   END
031200150604IF  1C* caricamento tabella 2l fascie di peso
031300150604     C                   Z-ADD     *ZEROS        II                6 0
031400150604     C                   Z-ADD     *ZEROS        IC                6 0
031500150604     C                   Z-ADD     *ZEROS        IT                6 0
031600150605     C                   MOVEL     '2L'          tblCOD
031700150605     C                   MOVEL     *BLANKS       tblKEY
031800150604     C     Ktab          SETLL     TABEL00F                           99
031900150610     C  N99              READ      TABEL00F                               99
032000150604     C     *IN99         DOWEQ     '0'
032100150605     C     TBLCOD        ANDEQ     '2L'
032200150604     C     TBLFLG        IFEQ      *BLANKS
032300150604     C                   MOVEL     TBLKEY        WTM03             3
032400150604     C                   MOVEL     TBLKEY        WTM05             5
032500150604     C                   MOVEL     TBLUNI        DS2L
032600150605     C     WTM03         ifeq      'S2C'
032700150604     C                   ADD       1             IC
032800150604     C                   MOVE      WTM05         FPC(IC)
032900150604     C                   Z-ADD     §2LKGI        KIC(IC)
033000150604     C                   Z-ADD     §2LKGF        KFC(IC)
033100150604     C                   ENDIF
033200150604     c*
033300150604     C                   ENDIF
033400150610     C                   READ      TABEL00F                               99
033500150604     C                   ENDDO
033600150610      * tabella spedizioni da considerare riepiloghi o ricavi carica solo primo flag = S
033700150610     C                   Z-ADD     0             I
033800150610     C                   MOVEL     '2W'          tblCOD
033900150610     C                   MOVEL     *BLANKS       tblKEY
034000150610     C     Ktab          SETLL     TABEL00F                           99
034100150610     C  N99              READ      TABEL00F                               99
034200150610     C     *IN99         DOWEQ     '0'
034300150610     C     TBLCOD        ANDEQ     '2W'
034400150610     C     TBLFLG        IFEQ      *BLANKS
034500150610     C                   MOVEL     TBLUNI        DS2w
034600150610     C     §2wFsp        IFEQ      'S'
034700150610     C                   ADD       1             I
034800150610     C                   MOVEL     TBLKEY        K2w(I)
034900150610     C                   END
035000150610     C                   END
035100150610     C                   READ      TABEL00F                               99
035200150610     C                   END
035300150604     C*
035400150603     c                   endsr
