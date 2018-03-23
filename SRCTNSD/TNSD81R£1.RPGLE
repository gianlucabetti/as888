000100921019     H DECEDIT('0,') DATEDIT(*DMY.)
000200941121     F* TNSD81R *-----------------------------------------------------*
000300921019     F*----------------*                                              *
000400921118     F* RIPRISTINO BOLLE                                              *
000500921019     F*---------------------------------------------------------------*
000600941121     FTNSD81D   CF   E             WORKSTN
000700921119      *
000800021030     FFIVABWKL  UF A E           K DISK
000900021030     F                                     RENAME(FIVAB000:FIVAB777)
001000921118      *
001100021030     FFIVAB46F  UF   E             DISK    USROPN
001200021030     F**FNVAB47F  UF   E             DISK    USROPN
001300021030     F**                                     RENAME(FNVAB000:FNVAB47)
001400921119     F*---------------------------------------------------------------*
001500921019     D KPJBA         E DS
001600921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
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
008600921118      *
008700921119     C     INIZ1         TAG
008800921118     C                   Z-ADD     102           VVVLNP
008900921118     C                   Z-ADD     01            VVVNRS
009000921119     C                   Z-ADD     0             VVVNSP
009100921119     C                   Z-ADD     0             VVVRMN
009200921119     C                   SETOFF                                       78
009300921118      *
009400921118     C     *INKF         DOUEQ     '1'
009500921118     C     *IN99         ANDEQ     '0'
009600921118     C     *INKC         OREQ      '1'
009700941121     C                   EXFMT     TNSD81A
009800921118     C                   SETOFF                                       991011
009900921119     C                   SETOFF                                       7778
010000980223     C*
010100980223     C     VVVLNP        IFNE      102
010200980223     C     VVVLNP        ANDNE     201
010300980223     C                   SETON                                        1299
010400980223     C                   END
010500980223     C   99              GOTO      ENDCIC
010600921118      *
010700921118     C     VVVNSP        IFEQ      *ZEROS
010800921119     C     VVVRMN        ANDEQ     *ZEROS
010900921118     C                   SETON                                        1199      DATI OBBLIGATOR
011000921118     C                   END
011100921118     C   99              GOTO      ENDCIC
011200921118      *
011300021030      * Apro e leggo sempre fivab46f
011400021030     C**   VVVLNP        IFEQ      102
011500021030     C                   OPEN      FIVAB46F
011600021030     C                   READ      FIVAB46F                               10
011700021030     C**                 ELSE
011800021030     C**                 OPEN      FNVAB47F
011900021030     C**                 READ      FNVAB47F                               10
012000021030     C**                 END
012100921119     C     *IN10         DOUEQ     '1'
012200921119     C     *IN77         OREQ      '1'
012300921119     C     *IN78         OREQ      '1'
012400921118      *
012500960104     C     VVVRMN        IFEQ      VABRMN
012600960104     C     VVVLNP        IFNE      VABLNP
012700960104     C     VVVNRS        ORNE      VABNRS
012800960104     C     VVVNSP        ORNE      VABNSP
012900921119     C                   SETON                                        7799      RECORD ESISTENTE MA
013000921119     C                   GOTO      END2                                         DATI VIDEO INCONGRUE
013100921119     C                   ENDIF
013200921119     C                   ENDIF
013300921118      *
013400960104     C     VVVLNP        IFEQ      VABLNP
013500960104     C     VVVNRS        ANDEQ     VABNRS
013600960104     C     VVVNSP        ANDEQ     VABNSP
013700960104     C     VVVRMN        ANDNE     VABRMN
013800921119     C                   SETON                                        7799      RECORD ESISTENTE MA
013900921119     C                   GOTO      END2                                         DATI VIDEO INCONGRUE
014000921119     C                   ENDIF
014100921119      *
014200960104     C     VVVLNP        IFEQ      VABLNP
014300960104     C     VVVNRS        ANDEQ     VABNRS
014400960104     C     VVVNSP        ANDEQ     VABNSP
014500960104     C     VVVRMN        ANDEQ     VABRMN
014600921119     C                   SETON                                        78        RECORD TROVATO
014700921119     C                   GOTO      END2
014800921119     C                   ENDIF
014900921118      *
015000021030      * Leggo sempre fivab46f
015100021030     C**   VVVLNP        IFEQ      102
015200021030     C                   READ      FIVAB46F                               10
015300021030     C**                 ELSE
015400021030     C**                 READ      FNVAB47F                               10
015500021030     C**                 END
015600921119     C     END2          TAG
015700921119     C                   ENDDO
015800921118      *
015900921119     C   10              SETON                                        99        RECORD NON TROVATO
016000021030     C**   99
016100021030     C**ORNKFVVVLNP        IFEQ      102
016200021030      * se non ho letto o se non ho dato F6 chiudo il file
016300021030     C                   IF        *in99=*on  or *inkf=*off
016400021030     C                   CLOSE     FIVAB46F
016500021030     C                   ENDIF
016600021030     C**                   ELSE
016700021030     C**                   CLOSE     FNVAB47F
016800021030     C**                   END
016900921118      *
017000921118     C     ENDCIC        TAG
017100921118     C                   ENDDO
017200921118      *
017300921118     C   KC              GOTO      FINE
017400921118      *
017500921118      *EMETTO 2° VIDEATA CON DATI BOLLA.
017600921118      *
017700960104     C     VABFFD        IFEQ      'S'
017800921119     C                   MOVE      'SI'          VFERMO
017900921119     C                   END
018000960104     C                   MOVEL     VABLOD        VIDLOD
018100921118      *
018200921119     C     INIZ2         TAG
018300941121     C                   EXFMT     TNSD81B
018400921119     C                   SETOFF                                       30
018500021030     C** KLVVVLNP        IFEQ      102
018600021030     C   KL              CLOSE     FIVAB46F
018700021030     C**                 ELSE
018800021030     C**                 CLOSE     FNVAB47F
018900021030     C**                 END
019000921119     C   KL              GOTO      INIZ1
019100921119     C   KC              GOTO      FINE
019200921119      *
019300921201     C   KQ
019400921119     COR KF              GOTO      ELABY
019500921119     C                   GOTO      INIZ2
019600921118      *
019700921119     C     ELABY         TAG
019800021030      *Il record non deve esistere sul FIVABWKF in caso di RIPRISTINO.
019900021030     C   KFVABRMN        CHAIN     FIVABWKL                           20
020000921119     C  N20
020100921119     CAN KF              SETON                                        30        AVVERTIRE CED !
020200921119     C  N20
020300921119     CAN KF              GOTO      INIZ2                                        AVVERTIRE CED !
020400921119      *
020500921118      *
020600960104     C                   MOVE      VABATB        ATB
020700960104     C                   Z-ADD     VABLNP        LNP
020800021030     C                   Z-ADD     VABFGS        FGS
020900960104     C                   Z-ADD     VABAAS        AAS
021000960104     C                   Z-ADD     VABMGS        MGS
021100960104     C                   Z-ADD     VABNRS        NRS
021200960104     C                   Z-ADD     VABNSP        NSP
021300960104     C                   MOVE      VABCBO        CBO
021400960104     C                   Z-ADD     VABLNA        LNA
021500960104     C                   MOVEL     VABRSD        RSD
021600960104     C                   MOVEL     VABRD2        RD2
021700960104     C                   MOVEL     VABIND        IND
021800960104     C                   MOVEL     VABCAD        CAD
021900960104     C                   MOVEL     VABLOD        LOD
022000960104     C                   MOVE      VABPRD        PRD
022100960104     C                   MOVE      VABNZD        NZD
022200960104     C                   MOVEL     VABGC1        GC1
022300960104     C                   MOVEL     VABGC2        GC2
022400960104     C                   Z-ADD     VABCTR        CTR
022500960104     C                   MOVEL     VABCTS        CTS
022600960104     C                   MOVE      VABFTM        FTM
022700960104     C                   MOVE      VABIAS        IAS
022800960104     C                   MOVE      VABVAS        VAS
022900960104     C                   MOVE      VABTSP        TSP
023000960104     C                   MOVEL     VABNAS        NAS
023100960104     C                   Z-ADD     VABNCL        NCL
023200960104     C                   Z-ADD     VABPKB        PKB
023300960104     C                   Z-ADD     VABVLB        VLB
023400960104     C                   Z-ADD     VABANT        ANT
023500960104     C                   Z-ADD     VABQFT        QFT
023600960104     C                   Z-ADD     VABCAS        CAS
023700960104     C                   MOVEL     VABTIC        TIC
023800960104     C                   MOVE      VABVCA        VCA
023900960104     C                   MOVE      VABGCA        GCA
024000960104     C                   MOVE      VABSCL        SCL
024100960104     C                   Z-ADD     VABCCM        CCM
024200960104     C                   Z-ADD     VABRMN        RMN
024300960104     C                   MOVEL     VABRMA        RMA
024400960104     C                   MOVEL     VABRMO        RMO
024500960104     C                   MOVEL     VABCMO        CMO
024600960104     C                   MOVEL     VABNMO        NMO
024700960104     C                   Z-ADD     VABNCD        NCD
024800960104     C                   Z-ADD     VABNCA        NCA
024900960104     C                   MOVE      VABXCO        XCO
025000021030     C                   MOVEL     VABNOT        NOI
025100960104     C                   MOVEL     VABNT2        NT2
025200960104     C                   Z-ADD     VABZNC        ZNC
025300960104     C                   MOVEL     VABCTM        CTM
025400960104     C                   MOVE      VABFFD        FFD
025500960104     C                   Z-ADD     VABDCR        DCR
025600960104     C                   MOVE      VABTCR        TCR
025700960104     C                   Z-ADD     VABHCR        HCR
025800960104     C                   MOVEL     VABVMD        VMD
025900960104     C                   MOVEL     VABVAD        VAD
026000960104     C                   MOVEL     VABGMA        GMA
026100960104     C                   MOVEL     VABGGA        GGA
026200960104     C                   MOVEL     VABGVA        GVA
026300960104     C                   MOVEL     VABTC1        TC1
026400960104     C                   MOVEL     VABTC2        TC2
026500921118      *
026600021030     C   KF              WRITE     FIVAB777
026700921118      *
026800021030     C**   VVVLNP        IFEQ      102
026900021030      * Cancello e chiudo sempre fivab46f
027000021030     C                   DELETE    FIVAB000
027100021030     C                   CLOSE     FIVAB46F
027200021030     C**                 ELSE
027300021030     C**                 DELETE    FNVAB47
027400021030     C**                 CLOSE     FNVAB47F
027500021030     C**                 END
027600921118      *
027700921119     C                   GOTO      INIZ1
027800921119      *
027900921118     C     FINE          TAG
028000921118     C                   SETON                                        LR
028100921118      *
