000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300021214     FTABEl00F  IF   E           K DISK
000400050118     FFNART01L  uF   E           K DISK
000500050118     FFNARb01L  iF   E           K DISK
000600050118     FFnanm02L  iF a E           K DISK
000700021216     FFIBRV09L  IF   E           K DISK
000800021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021216
001000021216     D DSLV53        E DS                  EXTNAME(FNLV53DS)
001100021216     D DSLV55        E DS                  EXTNAME(FNLV55DS)
001200021216     D CNCR80        E DS
001300050118     D fidn12ds      E DS
001400021214     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500021214     D DS7c          E DS
001600021216     D WLBDA8          DS
001700021216     D  G08DAT                 1      8  0
001800021216     D  G08INV                 9     16  0
001900021216     D  G08ERR                17     17
002000021216     D  G08TGI                18     22  0
002100050118     d Dataiso         s               d   datfmt(*iso)
002200021214      *--------------------------------------------------
002300050118     C     *LOVAL        setll     fnart000
002400050118     C                   read      fnart000
002500021214     c                   DOW       not %EOF(FNART01L)
002600050118     C                   IF        ARTDAM=0 AND ARTDCM=0  AND ARTDFV<20050114
002700050118     c     karb          chain     fnarb01l
002800050118     c                   if        arbaas<2005 or (arbmgs<0117 and arbaas=2005)
002900021214      *
003000021214     c     kanm          chain     fnanm000
003100021214     c                   IF        not %FOUND(FNANM02L)
003200050118     c* Verifico se c'e' C.A.
003300050118     c                   clear                   fidn12ds
003400050118     c                   eval      i12aas=artaas
003500050118     c                   eval      i12lnp=artlnp
003600050118     c                   eval      i12nrs=artnrs
003700050118     c                   eval      i12nsp=artnsp
003800050118     c                   eval      i12fel='E'
003900050118     c                   eval      i12fan='E'
004000050118     c                   call      'FIDN12R'
004100050118     c                   parm                    fidn12ds
004200050118     c                   if        o12err<>' ' or o12nca=0
004300050118     c*
004400021217     c                   exsr      CRTANM
004500021214     C                   ADD       1             CONTA             6 0
004600021214     C   OF              EXCEPT    TESTA
004700021214     C                   EXCEPT    DETTA
004800050118     c*
004900050118     c                   if        artdet>0 and artdut=0  and wcrea='S'
005000050118     c                   clear                   dslv53
005100050118     c                   eval      d53tfo='A'
005200050118     c                   eval      d53nfv=anmnfv
005300050118     c                   eval      d53npg=1
005400050118     c                   eval      d53fgs=anmcdu
005500050118     c                   call      'FNLV53R'
005600050118     c                   parm                    dslv53
005700050118     c                   if        d53err=' '
005800050118     c                   movel     d53dfv        artdut
005900050118     c                   movel     anmnps        artput
006000050118     c                   endif
006100050118     c
006200050118     c                   update    fnart000
006300050118     c                   endif
006400050118     c
006500021214     c                   ENDIF
006600050118     c                   ENDIF
006700050118     c                   ENDIF
006800021214     c                   ENDIF
006900021214      *
007000050118     C                   read      fnart000
007100021214     c                   ENDDO
007200021214      *
007300021216     C                   MOVEL     'C'           D53TLA
007400021216     C                   CALL      'FNLV53R'
007500021216     C                   PARM                    DSLV53
007600021216      *
007700021216     C                   MOVEL     'C'           D55TLA
007800021216     C                   CALL      'FNLV55R'
007900021216     C                   PARM                    DSLV53
008000050118     c
008100050118     C                   MOVEL     'C'           i12TLA
008200050118     C                   CALL      'FIDN12R'
008300050118     C                   PARM                    fidn12ds
008400021216      *
008500021214     C                   eval       *inlr = *on
008600021214     C**************************************************************************
008700021214     C* CREO ANOMALIA
008800021214     C**************************************************************************
008900050118     C     CRTANM        BEGSR
009000021214      *
009100021214     C                   Z-ADD     ARTAAS        ANMAAS
009200021214     C                   Z-ADD     ARTLNP        ANMLNP
009300021214     C                   Z-ADD     ARTNSP        ANMNSP
009400021214      *
009500021214     C                   MOVEL     artLNP        ANMFLS
009600021214     C                   MOVEL     artLNA        ANMLNA
009700021214     C                   MOVEL     artNRS        ANMNRS
009800021214     C                   MOVEL     artNSC        ANMSCN
009900050118     C                   MOVEL     *DATE         W0060             6 0
010000050118     C                   TIME                    WORA              6 0
010100050118     C                   MOVE      WORA          W0040             4 0
010200050118     C                   MOVEL     W0060         ANMFLO
010300050118     C                   MOVE      W0040         ANMFLO
010400050118     c* non imposto la zona
010500050118     c                   eval      anmfl4=999
010600021216      *** p.o. che esegue l'ida
010700021216     C                   MOVEL     SIMFEL        ANMFLE
010800021216      *** dati spunta da ultima spunta partenze
010900021216     C                   clear                   ANMNPS
011000021216     C                   clear                   ANMnfv
011100021216     C                   clear                   ANMCDU
011200021216     C                   clear                   WDCS              8 0
011300021216     C                   clear                   WHCS              4 0
011400021216     c     KBRV          setll     fibrv000
011500021216     c     KBRV          reade     fibrv000
011600021216     c                   DOW       NOT  %eof(FIBRV09L)
011700021216     c                   if        BRVDfS > WDCS or
011800021216     c                             (BRVDfS = WDCS and BRVHmS > WHCS)
011900021216     C                   Z-ADD     brvnps        ANMNPS
012000021216     C                   Z-ADD     brvnfv        ANMnfv
012100021216     C                   MOVEL     brvfgs        ANMCDU
012200021216     C                   z-add     BRVDCS        WDCS
012300021216     C                   z-add     BRVHCS        WHCS
012400021216     c                   Endif
012500021216     c     KBRV          reade     fibrv000
012600021216     c                   ENDDO
012700050118     C
012800050118     c                   Move      *date         Dataiso
012900050118     C                   MOVE      DATAISO       ANMDAO
013000050118     C
013100021216     C                   MOVEL     ANMCDU        WFL2              3 0
013200021216     C                   MOVEL     ANMLNP        WFL1              3 0
013300021214     C*
013400021214     C                   EXSR      CRTANM1
013500021214      *
013600021214     C                   ENDSR
013700021214     C**************************************************************************
013800021214     C* CREO ANOMALIA
013900021214     C**************************************************************************
014000021214     C     CRTANM1       BEGSR
014100021214     C*
014200021214     C                   Z-ADD     COMCAA        ANMCAA
014300021214      *
014400050118     C                   MOVEL     '2'           ANMFSA
014500021214     C                   MOVEL     §7CTAN        ANMTAN
014600021214     C                   MOVEL     §7CSAN        ANMSAN
014700021214     C                   MOVEL     §7CAIE        ANMAIE
014800021214     C                   Z-ADD     §7CLID        ANMLID
014900021214     C                   MOVEL     '  '          ANMCCH
015000021214     C                   CLEAR                   ANMATB
015100021214     C* CHIUSURA AUTOMATICA
015200021214    1C     §7CCHA        IFEQ      'S'
015300021216     C                   Z-ADD     ANMDAO        ANMDCH
015400021214     C                   MOVEL     'I'           ANMFSC
015500021214     c                   ELSE
015600021214     C                   Z-ADD     0             ANMDCH
015700021214     C                   MOVEL     ' '           ANMFSC
015800021214    1C                   END
015900021214      *
016000021214     C                   CLEAR                   ANMFT1
016100021214     C                   Z-ADD     0             ANMFL1
016200021214     C                   CLEAR                   ANMFT2
016300021214     C                   Z-ADD     0             ANMFL2
016400021214     C* ESTERNA
016500021214    1C     §7CAIE        IFEQ      'E'
016600021214     C* VEDO A CHI TRASMETTERE
016700021214     C                   MOVEL     WFL1          ANMFL1
016800021214     C                   MOVEL     WFL2          ANMFL2
016900021214     C* SE UGUALI CANCELLO IL 2°
017000021214    2C     ANMFL1        IFEQ      ANMFL2
017100021214     C                   Z-ADD     0             ANMFL2
017200021214   X2C                   ELSE
017300021214     C                   CLEAR                   DSLV55
017400021214     C                   MOVEL     'P'           D55TPT
017500021214     C                   MOVE      ANMFL1        D55LIN
017600021214     C                   MOVE      DATEU8        D55DRF
017700021214     C                   CALL      'FNLV55R'
017800021214     C                   PARM                    DSLV55
017900021214    3C     D55ERR        IFNE      *BLANKS
018000021216     C                   MOVE      ANMFL1        WTFP1             3 0
018100021214   x3C                   ELSE
018200021214     C                   MOVE      D55TFP        WTFP1
018300021214    3C                   END
018400021214     C                   CLEAR                   DSLV55
018500021214     C                   MOVEL     'P'           D55TPT
018600021214     C                   MOVE      ANMFL2        D55LIN
018700021214     C                   MOVE      DATEU8        D55DRF
018800021214     C                   CALL      'FNLV55R'
018900021214     C                   PARM                    DSLV55
019000021214    3C     D55ERR        IFNE      *BLANKS
019100021216     C                   MOVE      ANMFL2        WTFP2             3 0
019200021214   x3C                   ELSE
019300021214     C                   MOVE      D55TFP        WTFP2
019400021214    3C                   END
019500021214    3C     WTFP1         IFEQ      WTFP2
019600021214     C                   CLEAR                   ANMFL2
019700021214    3C                   END
019800021214    2C                   ENDIF
019900021214     C*
020000021214    1C                   ENDIF
020100021214     C*
020200050118     c                   if        wcrea='S'
020300021214     C                   WRITE     FNANM000
020400050118     c                   endif
020500021214     C*
020600021214     C                   ENDSR
020700021214      *---------------------------------------------------------
020800021214     c     *inzsr        BEGSR
020900021214      *
021000021216     C     Kanm          KLIST
021100021216     C                   KFLD                    artfls
021200021216     C                   KFLD                    artlna
021300021216     C                   KFLD                    artnrs
021400021216     C                   KFLD                    artnsc
021500021214     C                   KFLD                    COMCAA
021600050118     C     Karb          KLIST
021700050118     C                   KFLD                    artaas
021800050118     C                   KFLD                    artlnp
021900050118     C                   KFLD                    artnrs
022000050118     C                   KFLD                    artnsp
022100021216      *
022200021216     C     KBRV          KLIST
022300021216     C                   KFLD                    Knpg
022400021216     C                   KFLD                    artlnp
022500021216     C                   KFLD                    artlna
022600021216     C                   KFLD                    artnrs
022700021216     C                   KFLD                    artnsc
022800021216      *
022900021216     C     Ktab          KLIST
023000021216     C                   KFLD                    tblkut
023100021216     C                   KFLD                    tblcod
023200021216     C                   KFLD                    tblkey
023300021214      *
023400021216     c     *entry        plist
023500050118     C                   PARM                    wcrea             1
023600021216      *
023700021214     C                   CALL      'X§PARUT'
023800021214     C                   PARM                    UT§DSE
023900021216     C                   MOVEL     REC80         CNCR80
024000021214     C                   MOVEL     RAGUT         RSUT             20
024100021214      *
024200021214     c                   z-add     5             comcaa            3 0
024300021216     c                   z-add     1             knpg              1 0
024400021214     c                   except    testa
024500021214      *
024600021214     C                   z-add     1             tblkut
024700021214     C                   MOVEL     '7C'          tblCOD
024800021214     C                   MOVEL     COMCAA        tblKEY
024900021214     C     KTAB          CHAIN     TABEL                              39
025000021214     C  N39              MOVEL     TBLUNI        DS7C
025100021214     C   39              CLEAR                   DS7C
025200021216      * RILEVO DATA GIORNO IN AAAA/MM/GG  E ORA
025300021216     C                   TIME                    WTIME            14 0
025400021216     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
025500021216     C                   CLEAR                   WLBDA8
025600021216     C                   MOVE      WDATE         G08DAT
025700021216     C                   CALL      'XSRDA8'
025800021216     C                   PARM                    WLBDA8
025900021216     C                   MOVE      G08INV        DATEU8            8 0
026000021216     C                   MOVEL     WTIME         UTIME             6 0
026100021216     C*
026200021214     C                   ENDSR
026300021214      *---------------------------------------------------------
026400021214     OQSYSPRT   E            TESTA          1 02
026500021214     O                       RSUT                20
026600021214     O                       *DATE            +   5 '  /  /    '
026700021214     O                                        +   5 'COLLI PARTITI NON ARRIVATI'
026800021214     O                                        +   1 'SENZA ANOMALIA 005'
026900050118     O                                        +   7 'TNVRAANM053'
027000021214     O                                          126 'PAG.'
027100021214     O                       PAGE               132
027200021214     O          E            DETTA       1
027300021217     O                                            5 'SPED.'
027400021217     O                       ARtAAS            +  2
027500021214     O                       ARtLNP            +  1
027600021214     O                       ARtNRS            +  1
027700021214     O                       ARtNSP        Z   +  1
027800050118     O                       ARtlna            +  1
027900050118     O                       ARbmgs            +  1 '  /  '
028000021217     O                                         +  2 'SEGNAC'
028100021217     O                       ARtNSC        Z   +  1
028200050118     O                                         +  2 'ARTDFV'
028300021214     O                       ARtDFV            +  1 '    /  /  '
028400050118     O                                         +  2 'PTC'
028500021214     O                       ARtNPP            +  1
028600050118     O                                         +  2 'ARTDUT'
028700021214     O                       ARtDUT            +  1 '    /  /  '
028800050118     O                                         +  2 'Anom'
028900050118     O                       anmnps            +  1
029000050118     O                       anmcdu            +  1
029100050118     o                                         +  0 '-'
029200050118     O                       anmnfv        z   +  0
