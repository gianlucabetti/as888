000100921019     H DECEDIT('0,') DATEDIT(*DMY.)
000200941121     F* TNSD81R *-----------------------------------------------------*
000300131025     F* RIPRISTINO /annullamento BOLLE  della 102                                   *
000400921019     F*---------------------------------------------------------------*
000500941121     FTNSD81D   CF   E             WORKSTN
000600921119      *
000700021030     FFIVABWKL  UF A E           K DISK
000800021030     F                                     RENAME(FIVAB000:FIVAB777)
000900921118      *
001000131025     FFIVAB46F  UF   E             DISK    USROPN extfile(wFIVAB46)
001100921119     F*---------------------------------------------------------------*
001200131025     D TES             S             15    DIM(3) CTDATA PERRCD(1)
001300131025     d
001400921019     D KPJBA         E DS
001500921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600131025     d wfivab46        s             21    inz('GAITRAGRU /FIVAB46F')
001700921119     I*---------------------------------------------------------------*
001800021030     IFIVAB777
001900021030     I              VABFGS                      FGS
002000960104     I              VABATB                      ATB
002100960104     I              VABLNP                      LNP
002200960104     I              VABAAS                      AAS
002300960104     I              VABMGS                      MGS
002400960104     I              VABNRS                      NRS
002500960104     I              VABNSP                      NSP
002600960104     I              VABCBO                      CBO
002700960104     I              VABLNA                      LNA
002800960104     I              VABRSD                      RSD
002900960104     I              VABRD2                      RD2
003000960104     I              VABIND                      IND
003100960104     I              VABCAD                      CAD
003200960104     I              VABLOD                      LOD
003300960104     I              VABPRD                      PRD
003400960104     I              VABNZD                      NZD
003500960104     I              VABGC1                      GC1
003600960104     I              VABGC2                      GC2
003700960104     I              VABCTR                      CTR
003800960104     I              VABCTS                      CTS
003900960104     I              VABFTM                      FTM
004000960104     I              VABIAS                      IAS
004100960104     I              VABVAS                      VAS
004200960104     I              VABTSP                      TSP
004300960104     I              VABNAS                      NAS
004400960104     I              VABNCL                      NCL
004500960104     I              VABPKB                      PKB
004600960104     I              VABVLB                      VLB
004700960104     I              VABQFT                      QFT
004800960104     I              VABCAS                      CAS
004900960104     I              VABTIC                      TIC
005000960104     I              VABVCA                      VCA
005100960104     I              VABGCA                      GCA
005200960104     I              VABANT                      ANT
005300960104     I              VABSCL                      SCL
005400960104     I              VABCCM                      CCM
005500960104     I              VABRMN                      RMN
005600960104     I              VABRMA                      RMA
005700960104     I              VABRMO                      RMO
005800960104     I              VABCMO                      CMO
005900960104     I              VABNMO                      NMO
006000960104     I              VABNCD                      NCD
006100960104     I              VABNCA                      NCA
006200960104     I              VABXCO                      XCO
006300021030     I              VABNOT                      NOI
006400960104     I              VABNT2                      NT2
006500960104     I              VABZNC                      ZNC
006600960104     I              VABCTM                      CTM
006700960104     I              VABFFD                      FFD
006800960104     I              VABDCR                      DCR
006900960104     I              VABTCR                      TCR
007000960104     I              VABHCR                      HCR
007100960104     I              VABVMD                      VMD
007200960104     I              VABVAD                      VAD
007300960104     I              VABGMA                      GMA
007400960104     I              VABGGA                      GGA
007500960104     I              VABGVA                      GVA
007600960104     I              VABTC1                      TC1
007700960104     I              VABTC2                      TC2
007800921019     C*****************************************************************
007900921118      *
008000921019     C     *ENTRY        PLIST
008100921019     C                   PARM                    KPJBA
008200921019     C                   Z-ADD     1             CODUT
008300921019     C                   CALL      'X§PARUT'
008400921019     C                   PARM                    UT§DSE
008500921019     C                   MOVEL     RAGUT         RSUT             20
008600131025
008700131025     c                   select
008800131025     c                   when      %subst(kpjbu:20:3)='ACQ'
008900131025     c                   eval      vidtes2=tes(1)
009000131025     c                   when      %subst(kpjbu:20:3)='SED'
009100131025     c                   eval      vidtes2=tes(2)
009200131025     c                   eval      %subst(wfivab46:17:2)='50'
009300131025     c                   other
009400131025     c                   eval      vidtes2=tes(3)
009500131025     c                   endsl
009600921118      *
009700921119     C     INIZ1         TAG
009800921118     C                   Z-ADD     102           VVVLNP
009900921118     C                   Z-ADD     01            VVVNRS
010000921119     C                   Z-ADD     0             VVVNSP
010100921119     C                   Z-ADD     0             VVVRMN
010200921119     C                   SETOFF                                       78
010300921118      *
010400921118     C     *INKF         DOUEQ     '1'
010500921118     C     *IN99         ANDEQ     '0'
010600921118     C     *INKC         OREQ      '1'
010700941121     C                   EXFMT     TNSD81A
010800921118     C                   SETOFF                                       991011
010900921119     C                   SETOFF                                       7778
011000980223     C*
011100980223     C     VVVLNP        IFNE      102
011200980223     C                   SETON                                        1299
011300980223     C                   END
011400980223     C   99              GOTO      ENDCIC
011500921118      *
011600921118     C     VVVNSP        IFEQ      *ZEROS
011700921119     C     VVVRMN        ANDEQ     *ZEROS
011800921118     C                   SETON                                        1199      DATI OBBLIGATOR
011900921118     C                   END
012000921118     C   99              GOTO      ENDCIC
012100921118      *
012200021030      * Apro e leggo sempre fivab46f
012300131025
012400131025     c                   open(e)   fivab46f
012500131025     c                   if        %error
012600131025     c                   eval      %subst(wfivab46:7:4)='GRPS'
012700131025     c                   open(e)   fivab46f
012800131025     c                   endif
012900131025
013000021030     C                   READ      FIVAB46F                               10
013100921119     C     *IN10         DOUEQ     '1'
013200921119     C     *IN77         OREQ      '1'
013300921119     C     *IN78         OREQ      '1'
013400921118      *
013500960104     C     VVVRMN        IFEQ      VABRMN
013600960104     C     VVVLNP        IFNE      VABLNP
013700960104     C     VVVNRS        ORNE      VABNRS
013800960104     C     VVVNSP        ORNE      VABNSP
013900921119     C                   SETON                                        7799      RECORD ESISTENTE MA
014000921119     C                   GOTO      END2                                         DATI VIDEO INCONGRUE
014100921119     C                   ENDIF
014200921119     C                   ENDIF
014300921118      *
014400960104     C     VVVLNP        IFEQ      VABLNP
014500960104     C     VVVNRS        ANDEQ     VABNRS
014600960104     C     VVVNSP        ANDEQ     VABNSP
014700960104     C     VVVRMN        ANDNE     VABRMN
014800921119     C                   SETON                                        7799      RECORD ESISTENTE MA
014900921119     C                   GOTO      END2                                         DATI VIDEO INCONGRUE
015000921119     C                   ENDIF
015100921119      *
015200960104     C     VVVLNP        IFEQ      VABLNP
015300960104     C     VVVNRS        ANDEQ     VABNRS
015400960104     C     VVVNSP        ANDEQ     VABNSP
015500960104     C     VVVRMN        ANDEQ     VABRMN
015600921119     C                   SETON                                        78        RECORD TROVATO
015700921119     C                   GOTO      END2
015800921119     C                   ENDIF
015900921118      *
016000021030     C                   READ      FIVAB46F                               10
016100921119     C     END2          TAG
016200921119     C                   ENDDO
016300921118      *
016400921119     C   10              SETON                                        99        RECORD NON TROVATO
016500131025
016600021030      * se non ho letto o se non ho dato F6 chiudo il file
016700021030     C                   IF        *in99=*on  or *inkf=*off
016800021030     C                   CLOSE     FIVAB46F
016900021030     C                   ENDIF
017000921118      *
017100921118     C     ENDCIC        TAG
017200921118     C                   ENDDO
017300921118      *
017400921118     C   KC              GOTO      FINE
017500921118      *
017600921118      *EMETTO 2° VIDEATA CON DATI BOLLA.
017700921118      *
017800960104     C     VABFFD        IFEQ      'S'
017900921119     C                   MOVE      'SI'          VFERMO
018000921119     C                   END
018100960104     C                   MOVEL     VABLOD        VIDLOD
018200921118      *
018300921119     C     INIZ2         TAG
018400941121     C                   EXFMT     TNSD81B
018500921119     C                   SETOFF                                       30
018600131025     c
018700021030     C   KL              CLOSE     FIVAB46F
018800921119     C   KL              GOTO      INIZ1
018900921119     C   KC              GOTO      FINE
019000921119      *
019100921201     C   KQ
019200921119     COR KF              GOTO      ELABY
019300921119     C                   GOTO      INIZ2
019400921118      *
019500921119     C     ELABY         TAG
019600021030      *Il record non deve esistere sul FIVABWKF in caso di RIPRISTINO.
019700021030     C   KFVABRMN        CHAIN     FIVABWKL                           20
019800921119     C  N20
019900921119     CAN KF              SETON                                        30        AVVERTIRE CED !
020000921119     C  N20
020100921119     CAN KF              GOTO      INIZ2                                        AVVERTIRE CED !
020200921119      *
020300921118      *
020400960104     C                   MOVE      VABATB        ATB
020500960104     C                   Z-ADD     VABLNP        LNP
020600021030     C                   Z-ADD     VABFGS        FGS
020700960104     C                   Z-ADD     VABAAS        AAS
020800960104     C                   Z-ADD     VABMGS        MGS
020900960104     C                   Z-ADD     VABNRS        NRS
021000960104     C                   Z-ADD     VABNSP        NSP
021100960104     C                   MOVE      VABCBO        CBO
021200960104     C                   Z-ADD     VABLNA        LNA
021300960104     C                   MOVEL     VABRSD        RSD
021400960104     C                   MOVEL     VABRD2        RD2
021500960104     C                   MOVEL     VABIND        IND
021600960104     C                   MOVEL     VABCAD        CAD
021700960104     C                   MOVEL     VABLOD        LOD
021800960104     C                   MOVE      VABPRD        PRD
021900960104     C                   MOVE      VABNZD        NZD
022000960104     C                   MOVEL     VABGC1        GC1
022100960104     C                   MOVEL     VABGC2        GC2
022200960104     C                   Z-ADD     VABCTR        CTR
022300960104     C                   MOVEL     VABCTS        CTS
022400960104     C                   MOVE      VABFTM        FTM
022500960104     C                   MOVE      VABIAS        IAS
022600960104     C                   MOVE      VABVAS        VAS
022700960104     C                   MOVE      VABTSP        TSP
022800960104     C                   MOVEL     VABNAS        NAS
022900960104     C                   Z-ADD     VABNCL        NCL
023000960104     C                   Z-ADD     VABPKB        PKB
023100960104     C                   Z-ADD     VABVLB        VLB
023200960104     C                   Z-ADD     VABANT        ANT
023300960104     C                   Z-ADD     VABQFT        QFT
023400960104     C                   Z-ADD     VABCAS        CAS
023500960104     C                   MOVEL     VABTIC        TIC
023600960104     C                   MOVE      VABVCA        VCA
023700960104     C                   MOVE      VABGCA        GCA
023800960104     C                   MOVE      VABSCL        SCL
023900960104     C                   Z-ADD     VABCCM        CCM
024000960104     C                   Z-ADD     VABRMN        RMN
024100960104     C                   MOVEL     VABRMA        RMA
024200960104     C                   MOVEL     VABRMO        RMO
024300960104     C                   MOVEL     VABCMO        CMO
024400960104     C                   MOVEL     VABNMO        NMO
024500960104     C                   Z-ADD     VABNCD        NCD
024600960104     C                   Z-ADD     VABNCA        NCA
024700960104     C                   MOVE      VABXCO        XCO
024800021030     C                   MOVEL     VABNOT        NOI
024900960104     C                   MOVEL     VABNT2        NT2
025000960104     C                   Z-ADD     VABZNC        ZNC
025100960104     C                   MOVEL     VABCTM        CTM
025200960104     C                   MOVE      VABFFD        FFD
025300960104     C                   Z-ADD     VABDCR        DCR
025400960104     C                   MOVE      VABTCR        TCR
025500960104     C                   Z-ADD     VABHCR        HCR
025600960104     C                   MOVEL     VABVMD        VMD
025700960104     C                   MOVEL     VABVAD        VAD
025800960104     C                   MOVEL     VABGMA        GMA
025900960104     C                   MOVEL     VABGGA        GGA
026000960104     C                   MOVEL     VABGVA        GVA
026100960104     C                   MOVEL     VABTC1        TC1
026200960104     C                   MOVEL     VABTC2        TC2
026300921118      *
026400021030     C   KF              WRITE     FIVAB777
026500921118      *
026600021030      * Cancello e chiudo sempre fivab46f
026700021030     C                   DELETE    FIVAB000
026800021030     C                   CLOSE     FIVAB46F
026900921118      *
027000921119     C                   GOTO      INIZ1
027100921119      *
027200921118     C     FINE          TAG
027300921118     C                   SETON                                        LR
027400131025**
027500131025A C Q U I S T I
027600131025S   E   D   E
027700131025A  L  T  R  O
