000100021214     H DECEDIT('0,') DATEDIT(*DMY.)
000200021214      *
000300021214     FTABEl00F  IF   E           K DISK
000400021214     FFNART01L  IF   E           K DISK
000500021216     FFnanm02L  UF a E           K DISK
000600021216     FFIBRV09L  IF   E           K DISK
000700021216     FFNFVA01L  IF   E           K DISK
000800021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021216
001000021216     D DSLV53        E DS                  EXTNAME(FNLV53DS)
001100021216     D DSLV55        E DS                  EXTNAME(FNLV55DS)
001200021216     D CNCR80        E DS
001300021214     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400021214     D DS7c          E DS
001500021216     D WLBDA8          DS
001600021216     D  G08DAT                 1      8  0
001700021216     D  G08INV                 9     16  0
001800021216     D  G08ERR                17     17
001900021216     D  G08TGI                18     22  0
002000021214      *--------------------------------------------------
002100021214      *
002200021214     C     2002          setll     fnart000
002300021214     C     2002          reade     fnart000
002400021214     c                   DOW       not %EOF(FNART01L)
002500021214      *
002600021220     c                   if        artlnp <> 180       and
002700021220     c                             artdfv >= WdataA    and
002800021216     c                             artdfv <= WdataDA   and
002900021214     c                             artdam  = 0
003000021214     c     kanm          chain     fnanm000
003100021214     c                   IF        not %FOUND(FNANM02L)
003200021217     c                   exsr      CRTANM
003300021214     C                   ADD       1             CONTA             6 0
003400021214     C   OF              EXCEPT    TESTA
003500021214     C                   EXCEPT    DETTA
003600021214     c                   ENDIF
003700021214     c                   ENDIF
003800021214      *
003900021214     C     2002          reade     fnart000
004000021214     c                   ENDDO
004100021214      *
004200021216     C                   MOVEL     'C'           D53TLA
004300021216     C                   CALL      'FNLV53R'
004400021216     C                   PARM                    DSLV53
004500021216      *
004600021216     C                   MOVEL     'C'           D55TLA
004700021216     C                   CALL      'FNLV55R'
004800021216     C                   PARM                    DSLV53
004900021216      *
005000021214     C                   eval       *inlr = *on
005100021214     C**************************************************************************
005200021214     C* CREO ANOMALIA
005300021214     C**************************************************************************
005400021214     C     CRTANM        BEGSR
005500021214      *
005600021214     C                   Z-ADD     ARTAAS        ANMAAS
005700021214     C                   Z-ADD     ARTLNP        ANMLNP
005800021214     C                   Z-ADD     ARTNSP        ANMNSP
005900021214      *
006000021214     C                   MOVEL     artLNP        ANMFLS
006100021214     C                   MOVEL     artLNA        ANMLNA
006200021214     C                   MOVEL     artNRS        ANMNRS
006300021214     C                   MOVEL     artNSC        ANMSCN
006400021216      *** p.o. che esegue l'ida
006500021216     C                   MOVEL     SIMFEL        ANMFLE
006600021216      *** dati spunta da ultima spunta partenze
006700021216     C                   clear                   ANMNPS
006800021216     C                   clear                   ANMnfv
006900021216     C                   clear                   ANMCDU
007000021216     C                   clear                   WDCS              8 0
007100021216     C                   clear                   WHCS              4 0
007200021216     c     KBRV          setll     fibrv000
007300021216     c     KBRV          reade     fibrv000
007400021216     c                   DOW       NOT  %eof(FIBRV09L)
007500021216     c                   if        BRVDfS > WDCS or
007600021216     c                             (BRVDfS = WDCS and BRVHmS > WHCS)
007700021216     C                   Z-ADD     brvnps        ANMNPS
007800021216     C                   Z-ADD     brvnfv        ANMnfv
007900021216     C                   MOVEL     brvfgs        ANMCDU
008000021216     C                   z-add     BRVDCS        WDCS
008100021216     C                   z-add     BRVHCS        WHCS
008200021216     c                   Endif
008300021216     c     KBRV          reade     fibrv000
008400021216     c                   ENDDO
008500021214      *** data fv arrivi
008600021216     C* DETERMINO DATA DEL FOGLIO DELL'ANOMALIA
008700021216     C                   clear                   ANMDAO
008800021216     c                   z-add     anmnfv        wnfv5             5 0
008900021216     c     kfva          chain     FNFVA000
009000021216     c                   IF        %FOUND(FNFVA01L)
009100021216     C                   Z-ADD     FVADFA        ANMDAO
009200021216     c                   ENDIF
009300021216      *
009400021216     C                   MOVEL     ANMCDU        WFL2              3 0
009500021216     C                   MOVEL     ANMLNP        WFL1              3 0
009600021214     C*
009700021214     C                   EXSR      CRTANM1
009800021214
009900021214     C*
010000021214     C*    KARB          CHAIN     FNARB01L                           34
010100021214    1c*                  If        %FOUND(FNARB01L)
010200021214    2C*    ARBAMA        IFLT      2
010300021214     C*                  Z-ADD     0             ARBAMA
010400021214    2C*                  ENDIF
010500021214     C*                  UPDATE    FNARB000
010600021214    1C*                  ENDIF
010700021214      *
010800021214     C                   ENDSR
010900021214     C**************************************************************************
011000021214     C* CREO ANOMALIA
011100021214     C**************************************************************************
011200021214     C     CRTANM1       BEGSR
011300021214     C*
011400021214     C                   Z-ADD     COMCAA        ANMCAA
011500021214      *
011600021214     C                   MOVEL     'I'           ANMFSA
011700021214     C                   MOVEL     §7CTAN        ANMTAN
011800021214     C                   MOVEL     §7CSAN        ANMSAN
011900021214     C                   MOVEL     §7CAIE        ANMAIE
012000021214     C                   Z-ADD     §7CLID        ANMLID
012100021214     C                   MOVEL     '  '          ANMCCH
012200021214     C                   CLEAR                   ANMATB
012300021214     C* CHIUSURA AUTOMATICA
012400021214    1C     §7CCHA        IFEQ      'S'
012500021216     C                   Z-ADD     ANMDAO        ANMDCH
012600021214     C                   MOVEL     'I'           ANMFSC
012700021214     c                   ELSE
012800021214     C                   Z-ADD     0             ANMDCH
012900021214     C                   MOVEL     ' '           ANMFSC
013000021214    1C                   END
013100021214      *
013200021214     C                   CLEAR                   ANMFT1
013300021214     C                   Z-ADD     0             ANMFL1
013400021214     C                   CLEAR                   ANMFT2
013500021214     C                   Z-ADD     0             ANMFL2
013600021214     C* ESTERNA
013700021214    1C     §7CAIE        IFEQ      'E'
013800021214     C* VEDO A CHI TRASMETTERE
013900021214     C                   MOVEL     WFL1          ANMFL1
014000021214     C                   MOVEL     WFL2          ANMFL2
014100021214     C* SE UGUALI CANCELLO IL 2°
014200021214    2C     ANMFL1        IFEQ      ANMFL2
014300021214     C                   Z-ADD     0             ANMFL2
014400021214   X2C                   ELSE
014500021214     C                   CLEAR                   DSLV55
014600021214     C                   MOVEL     'P'           D55TPT
014700021214     C                   MOVE      ANMFL1        D55LIN
014800021214     C                   MOVE      DATEU8        D55DRF
014900021214     C                   CALL      'FNLV55R'
015000021214     C                   PARM                    DSLV55
015100021214    3C     D55ERR        IFNE      *BLANKS
015200021216     C                   MOVE      ANMFL1        WTFP1             3 0
015300021214   x3C                   ELSE
015400021214     C                   MOVE      D55TFP        WTFP1
015500021214    3C                   END
015600021214     C                   CLEAR                   DSLV55
015700021214     C                   MOVEL     'P'           D55TPT
015800021214     C                   MOVE      ANMFL2        D55LIN
015900021214     C                   MOVE      DATEU8        D55DRF
016000021214     C                   CALL      'FNLV55R'
016100021214     C                   PARM                    DSLV55
016200021214    3C     D55ERR        IFNE      *BLANKS
016300021216     C                   MOVE      ANMFL2        WTFP2             3 0
016400021214   x3C                   ELSE
016500021214     C                   MOVE      D55TFP        WTFP2
016600021214    3C                   END
016700021214    3C     WTFP1         IFEQ      WTFP2
016800021214     C                   CLEAR                   ANMFL2
016900021214    3C                   END
017000021214    2C                   ENDIF
017100021214     C*
017200021214    1C                   ENDIF
017300021214     C*
017400021214     C                   WRITE     FNANM000
017500021214     C*
017600021214     C                   ENDSR
017700021214      *---------------------------------------------------------
017800021214     c     *inzsr        BEGSR
017900021214      *
018000021216     C     Kanm          KLIST
018100021216     C                   KFLD                    artfls
018200021216     C                   KFLD                    artlna
018300021216     C                   KFLD                    artnrs
018400021216     C                   KFLD                    artnsc
018500021214     C                   KFLD                    COMCAA
018600021216      *
018700021216     C     KBRV          KLIST
018800021216     C                   KFLD                    Knpg
018900021216     C                   KFLD                    artlnp
019000021216     C                   KFLD                    artlna
019100021216     C                   KFLD                    artnrs
019200021216     C                   KFLD                    artnsc
019300021214      *
019400021216     C     Kfva          KLIST
019500021216     C                   KFLD                    anmcdu
019600021216     C                   KFLD                    wnfv5
019700021216      *
019800021216     C     Ktab          KLIST
019900021216     C                   KFLD                    tblkut
020000021216     C                   KFLD                    tblcod
020100021216     C                   KFLD                    tblkey
020200021214      *
020300021216     c     *entry        plist
020400021216     C                   PARM                    PDATAa            8
020500021216     C                   PARM                    PDATAda           8
020600021216      *
020700021214     C                   CALL      'X§PARUT'
020800021214     C                   PARM                    UT§DSE
020900021216     C                   MOVEL     REC80         CNCR80
021000021214     C                   MOVEL     RAGUT         RSUT             20
021100021214      *
021200021216     c                   movel     PdataA        WDATAa            8 0
021300021216     c                   movel     PdataDA       WDATAda           8 0
021400021214     c                   z-add     5             comcaa            3 0
021500021216     c                   z-add     1             knpg              1 0
021600021214     c                   except    testa
021700021214      *
021800021214     C                   z-add     1             tblkut
021900021214     C                   MOVEL     '7C'          tblCOD
022000021214     C                   MOVEL     COMCAA        tblKEY
022100021214     C     KTAB          CHAIN     TABEL                              39
022200021214     C  N39              MOVEL     TBLUNI        DS7C
022300021214     C   39              CLEAR                   DS7C
022400021216      * RILEVO DATA GIORNO IN AAAA/MM/GG  E ORA
022500021216     C                   TIME                    WTIME            14 0
022600021216     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
022700021216     C                   CLEAR                   WLBDA8
022800021216     C                   MOVE      WDATE         G08DAT
022900021216     C                   CALL      'XSRDA8'
023000021216     C                   PARM                    WLBDA8
023100021216     C                   MOVE      G08INV        DATEU8            8 0
023200021216     C                   MOVEL     WTIME         UTIME             6 0
023300021216     C*
023400021214     C                   ENDSR
023500021214      *---------------------------------------------------------
023600021214     OQSYSPRT   E            TESTA          1 02
023700021214     O                       RSUT                20
023800021214     O                       *DATE            +   5 '  /  /    '
023900021214     O                                        +   5 'COLLI PARTITI NON ARRIVATI'
024000021214     O                                        +   1 'SENZA ANOMALIA 005'
024100021214     O                                        +   7 'TNVRAANM051'
024200021214     O                                          126 'PAG.'
024300021214     O                       PAGE               132
024400021214     O          E            DETTA       1
024500021217     O                                            5 'SPED.'
024600021217     O                       ARtAAS            +  2
024700021214     O                       ARtLNP            +  1
024800021214     O                       ARtNRS            +  1
024900021214     O                       ARtNSP        Z   +  1
025000021217     O                                         +  2 'SEGNAC'
025100021217     O                       ARtNSC        Z   +  1
025200021217     O                                         +  2 'data FVP'
025300021214     O                       ARtDFV            +  1 '    /  /  '
025400021217     O                                         +  2 'PTC PAR'
025500021214     O                       ARtNPP            +  1
025600021217     O                                         +  2 'USC TRAN'
025700021214     O                       ARtDUT            +  1 '    /  /  '
025800021217     O                       CONTA         2   +  3
