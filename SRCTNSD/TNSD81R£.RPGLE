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
001200131230     D TES             S             15    DIM(3)  CTDATA PERRCD(1)
001300131230     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001400131025     d
001500921019     D KPJBA         E DS
001600921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700131025     d wfivab46        s             21    inz('GAITRAGRU /FIVAB46F')
001800921119     I*---------------------------------------------------------------*
001900021030     IFIVAB777
002000021030     I              VABFGS                      FGS
002100960104     I              VABATB                      ATB
002200960104     I              VABLNP                      LNP
002300960104     I              VABAAS                      AAS
002400960104     I              VABMGS                      MGS
002500960104     I              VABNRS                      NRS
002600960104     I              VABNSP                      NSP
002700960104     I              VABCBO                      CBO
002800960104     I              VABLNA                      LNA
002900960104     I              VABRSD                      RSD
003000960104     I              VABRD2                      RD2
003100960104     I              VABIND                      IND
003200960104     I              VABCAD                      CAD
003300960104     I              VABLOD                      LOD
003400960104     I              VABPRD                      PRD
003500960104     I              VABNZD                      NZD
003600960104     I              VABGC1                      GC1
003700960104     I              VABGC2                      GC2
003800960104     I              VABCTR                      CTR
003900960104     I              VABCTS                      CTS
004000960104     I              VABFTM                      FTM
004100960104     I              VABIAS                      IAS
004200960104     I              VABVAS                      VAS
004300960104     I              VABTSP                      TSP
004400960104     I              VABNAS                      NAS
004500960104     I              VABNCL                      NCL
004600960104     I              VABPKB                      PKB
004700960104     I              VABVLB                      VLB
004800960104     I              VABQFT                      QFT
004900960104     I              VABCAS                      CAS
005000960104     I              VABTIC                      TIC
005100960104     I              VABVCA                      VCA
005200960104     I              VABGCA                      GCA
005300960104     I              VABANT                      ANT
005400960104     I              VABSCL                      SCL
005500960104     I              VABCCM                      CCM
005600960104     I              VABRMN                      RMN
005700960104     I              VABRMA                      RMA
005800960104     I              VABRMO                      RMO
005900960104     I              VABCMO                      CMO
006000960104     I              VABNMO                      NMO
006100960104     I              VABNCD                      NCD
006200960104     I              VABNCA                      NCA
006300960104     I              VABXCO                      XCO
006400021030     I              VABNOT                      NOI
006500960104     I              VABNT2                      NT2
006600960104     I              VABZNC                      ZNC
006700960104     I              VABCTM                      CTM
006800960104     I              VABFFD                      FFD
006900960104     I              VABDCR                      DCR
007000960104     I              VABTCR                      TCR
007100960104     I              VABHCR                      HCR
007200960104     I              VABVMD                      VMD
007300960104     I              VABVAD                      VAD
007400960104     I              VABGMA                      GMA
007500960104     I              VABGGA                      GGA
007600960104     I              VABGVA                      GVA
007700960104     I              VABTC1                      TC1
007800960104     I              VABTC2                      TC2
007900921019     C*****************************************************************
008000921118      *
008100921019     C     *ENTRY        PLIST
008200921019     C                   PARM                    KPJBA
008300921019     C                   Z-ADD     1             CODUT
008400921019     C                   CALL      'X§PARUT'
008500921019     C                   PARM                    UT§DSE
008600921019     C                   MOVEL     RAGUT         RSUT             20
008700131025
008800131025     c                   select
008900131025     c                   when      %subst(kpjbu:20:3)='ACQ'
009000131025     c                   eval      vidtes2=tes(1)
009100131025     c                   when      %subst(kpjbu:20:3)='SED'
009200131025     c                   eval      vidtes2=tes(2)
009300131025     c                   eval      %subst(wfivab46:17:2)='50'
009400131025     c                   other
009500131025     c                   eval      vidtes2=tes(3)
009600131025     c                   endsl
009700921118      *
009800921119     C     INIZ1         TAG
009900921118     C                   Z-ADD     102           VVVLNP
010000921118     C                   Z-ADD     01            VVVNRS
010100921119     C                   Z-ADD     0             VVVNSP
010200921119     C                   Z-ADD     0             VVVRMN
010300921119     C                   SETOFF                                       78
010400921118      *
010500921118     C     *INKF         DOUEQ     '1'
010600921118     C     *IN99         ANDEQ     '0'
010700921118     C     *INKC         OREQ      '1'
010800941121     C                   EXFMT     TNSD81A
010900921118     C                   SETOFF                                       991011
011000131230     C                   SETOFF                                       777828
011100140114     c                   clear                   altralnp          3 0
011200140114     c                   clear                   altranrs          2 0
011300140114     c                   clear                   altransp          7 0
011400980223     C*
011500980223     C     VVVLNP        IFNE      102
011600131230     C                   SETON                                        2899
011700131230     c                   eval      vidmsg=msg(1)
011800131230     C                   GOTO      ENDCIC
011900131230     C                   ENDif
012000921118      *
012100921118     C     VVVNSP        IFEQ      *ZEROS
012200131230     C     VVVRMN        oreq      *ZEROS
012300131230     C                   SETON                                        2899      DATI OBBLIGATOR
012400131230     c                   eval      vidmsg=msg(2)
012500131230     C                   GOTO      ENDCIC
012600131230     C                   END
012700921118      *
012800021030      * Apro e leggo sempre fivab46f
012900131025
013000131025     c                   open(e)   fivab46f
013100131025     c                   if        %error
013200131025     c                   eval      %subst(wfivab46:7:4)='GRPS'
013300131025     c                   open(e)   fivab46f
013400131025     c                   endif
013500131025
013600021030     C                   READ      FIVAB46F                               10
013700921119     C     *IN10         DOUEQ     '1'
013800131230     C     *IN28         OREQ      '1'
013900921119     C     *IN78         OREQ      '1'
014000921118      *
014100960104     C     VVVRMN        IFEQ      VABRMN
014200131230     c
014300140114     c* se trovo una bolla con lo stesso RMN ma altra sped. salvo i dati e do errore solo se
014400140114     c*  non ne trovo una uguale
014500960104     C     VVVLNP        IFNE      VABLNP
014600960104     C     VVVNRS        ORNE      VABNRS
014700960104     C     VVVNSP        ORNE      VABNSP
014800140114     c                   eval      altralnp=vablnp
014900140114     c                   eval      altranrs=vabnrs
015000140114     c                   eval      altransp=vabnsp
015100921119     C                   ENDIF
015200921119     C                   ENDIF
015300921118      *
015400960104     C     VVVLNP        IFEQ      VABLNP
015500960104     C     VVVNRS        ANDEQ     VABNRS
015600960104     C     VVVNSP        ANDEQ     VABNSP
015700131230     c
015800960104     C     VVVRMN        ANDNE     VABRMN
015900131230     C                   SETON                                        2899      RECORD ESISTENTE MA
016000131230     c                   eval      vidmsg=msg(4)
016100131230     c                   eval      %subst(vidmsg:63:15)=%editc(vabrmn:'Z')
016200921119     C                   GOTO      END2                                         DATI VIDEO INCONGRUE
016300921119     C                   ENDIF
016400921119      *
016500960104     C     VVVLNP        IFEQ      VABLNP
016600960104     C     VVVNRS        ANDEQ     VABNRS
016700960104     C     VVVNSP        ANDEQ     VABNSP
016800960104     C     VVVRMN        ANDEQ     VABRMN
016900921119     C                   SETON                                        78        RECORD TROVATO
017000921119     C                   GOTO      END2
017100921119     C                   ENDIF
017200921118      *
017300021030     C                   READ      FIVAB46F                               10
017400921119     C     END2          TAG
017500921119     C                   ENDDO
017600921118      *
017700140114     c* Se ho trovato un'altra bolla do errore
017800140114     c                   if        altransp>0
017900140114     c                   eval      vidmsg=msg(3)
018000140114     c                   eval      %subst(vidmsg:62:3)=%editc(altralnp:'X')
018100140114     c                   eval      %subst(vidmsg:66:2)=%editc(altranrs:'Z')
018200140114     c                   eval      %subst(vidmsg:69:7)=%editc(altransp:'Z')
018300140114     C                   SETON                                        2899      RECORD ESISTENTE MA
018400140114     c                   clear                   altralnp
018500140114     c                   clear                   altranrs
018600140114     c                   clear                   altransp
018700140114     c                   endif
018800140114     c
018900140114     c  n99              if        *in10
019000131230     c                   eval      vidmsg=msg(5)
019100131230     C                   SETON                                        9928      RECORD NON TROVATO
019200131230     c                   endif
019300131025
019400021030      * se non ho letto o se non ho dato F6 chiudo il file
019500021030     C                   IF        *in99=*on  or *inkf=*off
019600021030     C                   CLOSE     FIVAB46F
019700021030     C                   ENDIF
019800921118      *
019900921118     C     ENDCIC        TAG
020000921118     C                   ENDDO
020100921118      *
020200921118     C   KC              GOTO      FINE
020300921118      *
020400921118      *EMETTO 2° VIDEATA CON DATI BOLLA.
020500921118      *
020600960104     C     VABFFD        IFEQ      'S'
020700921119     C                   MOVE      'SI'          VFERMO
020800921119     C                   END
020900960104     C                   MOVEL     VABLOD        VIDLOD
021000921118      *
021100921119     C     INIZ2         TAG
021200941121     C                   EXFMT     TNSD81B
021300131230     C                   SETOFF                                       3028
021400131025     c
021500021030     C   KL              CLOSE     FIVAB46F
021600921119     C   KL              GOTO      INIZ1
021700131230
021800921119     C   KC              GOTO      FINE
021900921119      *
022000921201     C   KQ
022100921119     COR KF              GOTO      ELABY
022200921119     C                   GOTO      INIZ2
022300921118      *
022400921119     C     ELABY         TAG
022500021030      *Il record non deve esistere sul FIVABWKF in caso di RIPRISTINO.
022600131230     c                   if        *inkf
022700131230     C     VABRMN        CHAIN     FIVABWKL                           20
022800131230     c                   if        not *in20
022900131230     c                   eval      vidmsg=msg(6)
023000131230     C                   SETON                                        3028      RECORD NON TROVATO
023100131230     C                   GOTO      INIZ2                                        AVVERTIRE CED !
023200131230     c                   endif
023300131230     c                   endif
023400921119      *
023500921118      *
023600960104     C                   MOVE      VABATB        ATB
023700960104     C                   Z-ADD     VABLNP        LNP
023800021030     C                   Z-ADD     VABFGS        FGS
023900960104     C                   Z-ADD     VABAAS        AAS
024000960104     C                   Z-ADD     VABMGS        MGS
024100960104     C                   Z-ADD     VABNRS        NRS
024200960104     C                   Z-ADD     VABNSP        NSP
024300960104     C                   MOVE      VABCBO        CBO
024400960104     C                   Z-ADD     VABLNA        LNA
024500960104     C                   MOVEL     VABRSD        RSD
024600960104     C                   MOVEL     VABRD2        RD2
024700960104     C                   MOVEL     VABIND        IND
024800960104     C                   MOVEL     VABCAD        CAD
024900960104     C                   MOVEL     VABLOD        LOD
025000960104     C                   MOVE      VABPRD        PRD
025100960104     C                   MOVE      VABNZD        NZD
025200960104     C                   MOVEL     VABGC1        GC1
025300960104     C                   MOVEL     VABGC2        GC2
025400960104     C                   Z-ADD     VABCTR        CTR
025500960104     C                   MOVEL     VABCTS        CTS
025600960104     C                   MOVE      VABFTM        FTM
025700960104     C                   MOVE      VABIAS        IAS
025800960104     C                   MOVE      VABVAS        VAS
025900960104     C                   MOVE      VABTSP        TSP
026000960104     C                   MOVEL     VABNAS        NAS
026100960104     C                   Z-ADD     VABNCL        NCL
026200960104     C                   Z-ADD     VABPKB        PKB
026300960104     C                   Z-ADD     VABVLB        VLB
026400960104     C                   Z-ADD     VABANT        ANT
026500960104     C                   Z-ADD     VABQFT        QFT
026600960104     C                   Z-ADD     VABCAS        CAS
026700960104     C                   MOVEL     VABTIC        TIC
026800960104     C                   MOVE      VABVCA        VCA
026900960104     C                   MOVE      VABGCA        GCA
027000960104     C                   MOVE      VABSCL        SCL
027100960104     C                   Z-ADD     VABCCM        CCM
027200960104     C                   Z-ADD     VABRMN        RMN
027300960104     C                   MOVEL     VABRMA        RMA
027400960104     C                   MOVEL     VABRMO        RMO
027500960104     C                   MOVEL     VABCMO        CMO
027600960104     C                   MOVEL     VABNMO        NMO
027700960104     C                   Z-ADD     VABNCD        NCD
027800960104     C                   Z-ADD     VABNCA        NCA
027900960104     C                   MOVE      VABXCO        XCO
028000021030     C                   MOVEL     VABNOT        NOI
028100960104     C                   MOVEL     VABNT2        NT2
028200960104     C                   Z-ADD     VABZNC        ZNC
028300960104     C                   MOVEL     VABCTM        CTM
028400960104     C                   MOVE      VABFFD        FFD
028500960104     C                   Z-ADD     VABDCR        DCR
028600960104     C                   MOVE      VABTCR        TCR
028700960104     C                   Z-ADD     VABHCR        HCR
028800960104     C                   MOVEL     VABVMD        VMD
028900960104     C                   MOVEL     VABVAD        VAD
029000960104     C                   MOVEL     VABGMA        GMA
029100960104     C                   MOVEL     VABGGA        GGA
029200960104     C                   MOVEL     VABGVA        GVA
029300960104     C                   MOVEL     VABTC1        TC1
029400960104     C                   MOVEL     VABTC2        TC2
029500921118      *
029600021030     C   KF              WRITE     FIVAB777
029700921118      *
029800021030      * Cancello e chiudo sempre fivab46f
029900021030     C                   DELETE    FIVAB000
030000021030     C                   CLOSE     FIVAB46F
030100921118      *
030200921119     C                   GOTO      INIZ1
030300921119      *
030400921118     C     FINE          TAG
030500921118     C                   SETON                                        LR
030600131025**
030700131025A C Q U I S T I
030800131025S   E   D   E
030900131025A  L  T  R  O
031000131230**
031100131230Linea di partenza errata: accettata soltanto linea 102
031200131230Immettere sia il numero di bolla che il riferimento mittente
031300131230Trovata bolla con riferimento mitt. indicato ma con numero   xxx xx xxxxxxx
031400131230Trovata bolla con num.spedizione indicato ma con rif.mittente xxxxxxxxxxxxxxx
031500131230Bolla non trovata
031600131230Impossibile ripristinare:riferimento mittente già presente nelle bolle da conf.
