000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950214     F*--------------------------------------------------------------*
000300020716     F*       reperimento lista AS/P.O. a cui trasmettere
000400000000     F*--------------------------------------------------------------*
000500900514     FAZORG01L  IF   E           K DISK
000600020716     D MSG             S             78    DIM(2) CTDATA PERRCD(1)              MSG VIDEO
000700020716     D og143         E DS
000800020716     D DSLV55        E DS                  EXTNAME(FNLV55DS)
000900020716     D tibs56ds      E DS
001000021011     d  ski                    7    756  0 dim(250)
001100020716     d wfel            s                   like(orgfel)
001200020716     d wtfp            s                   like(d55tfp)
001300020716     d wogas           s                   like(§ogas)
001400020716     D                 DS
001500020716     D  WGG                    1      2  0
001600020716     D  WMM                    3      4  0
001700020716     D  WAA                    5      8  0
001800020716     D  WDATA                  1      8  0
001900020716     f*---------------------------------------------------------------------
002000020716     c     *entry        plist
002100020716     C                   PARM                    tibs56ds
002200020716     C****
002300021011     c                   clear                   ski
002400021011     C****
002500020716    1c                   if        i56ppo=0
002600020716     c                   eval      i56ppo=46
002700020716    1c                   endif
002800020716     c
002900020716     c     i56ppo        chain     azorg01l                           31
003000020716    1c                   if        not *in31
003100020716     c                   movel     orgde3        og143
003200020716     c                   movel     §ogas         wogas
003300020716     c                   movel     orgfel        wfel
003400020716     c                   else
003500020716     c                   movel     '1'           o56err
003600020716     c                   movel     msg(1)        o56msg
003700020716     c                   goto      fine
003800020716    1c                   endif
003900020716     c**
004000020716     c** controllo la modalità di caricamento
004100020716     c                   if        i56mod<>'UNI' and i56mod<>'GRU' and
004200020827     c                             i56mod<>'AZI' AND I56MOD<>'POA' and
004300020912     c                             i56mod<>'POS' and I56Mod <> 'TPS'
004400020716     c                   movel     '1'           o56err
004500020716     c                   movel     msg(2)        o56msg
004600020716     c                   goto      fine
004700020716     c                   endif
004800021003     c**
004900021003     c** Richiamo con POA da SEDE ritorna solo 046 in schiera
005000021003     c                   if        i56mod = 'POA' and i56ppo = 046
005100021003     c                   movel     046           SKI(1)
005200021003     c                   goto      fine
005300021003     c                   endif
005400020716     c
005500020912     c                   if        i56mod='AZI' or I56Mod = 'TPS'
005600020716     C** udate che prendo da TIME
005700020716     C                   TIME                    W0140            14 0
005800020716     C                   MOVE      W0140         UDATE8            8 0          GG/MM/AAAA
005900020716     C                   Z-ADD     UDATE8        WDATA
006000020716     C                   Z-ADD     WGG           WMMGG             4 0
006100020716     C                   MOVEL     WMM           WMMGG
006200020716     C                   Z-ADD     WMMGG         dateu             8 0          AAAA/MM/GG
006300020716     C                   MOVEL     WAA           dateu
006400020716     c* predno il terminal di partenza di ppo
006500020716     C                   CLEAR                   DSLV55
006600020716     C                   MOVEL     'P'           D55TPT
006700020716     C                   MOVEL     i56ppo        D55LIN
006800020716     C                   MOVEL     dateu         D55DRF
006900020716     C                   CALL      'FNLV55R'
007000020716     C                   PARM                    DSLV55
007100020716    5C     D55ERR        IFNE      ' '
007200020716     C                   MOVEL     ORGFIL        wtfp
007300020716     c                   else
007400020716     C                   MOVEL     d55tfp        wtfp
007500020716    5C                   ENDIF
007600020716     c                   endif
007700020716     c**
007800020528     c**
007900020515     C                   Z-ADD     1             C                 4 0
008000971120     C     *LOVAL        SETLL     AZORG
008100971120     C                   READ      AZORG                                  31
008200971120    1C     *IN31         DOWEQ     '0'
008300971120     C*
008400020716    2c                   if        orgfag='F' or orgfag='A' or orgfil=46
008500020716     C* NON ANNULLATO
008600971120    3C     ORGFVA        IFEQ      ' '
008700020912
008800020912     c**
008900020912      * Solo per alcuni richiami
009000020912     c                   If        I56Mod ='AZI' or I56Mod = 'TPS'
009100020912     C* CONTROLLO SE E' TERMINAL
009200020912     C                   CLEAR                   DSLV55
009300020912     C                   MOVEL     'P'           D55TPT
009400020912     C                   MOVEL     ORGFIL        D55LIN
009500020912     C                   MOVEL     dateu         D55DRF
009600020912     C                   CALL      'FNLV55R'
009700020912     C                   PARM                    DSLV55
009800020912    5C     D55ERR        IFNE      ' '
009900020912     C                   MOVEL     ORGFIL        D55TFP
010000020912    5C                   ENDIF
010100020912     c                   EndIf
010200020716     c**
010300020716     A***  PER UNI - GLI ORGFEL A CUI TRASMETTERE
010400020716     A***            ESCLUSO IL PROPRIO E COMPRESA LA SEDE(46)
010500020716    4c                   if        i56mod='UNI' and orgfel<>wfel
010600020716     C     ORGFEL        LOOKUP    ski                                    30
010700020716     C  N30              Z-ADD     ORGFEL        ski(C)
010800020515     C  N30              ADD       1             C
010900020515    4C                   ENDIF
011000020716     c
011100020716     A***  PER GRU - GLI ORGFEL A CUI TASMETTERE + 946,
011200020716     A***            ESCLUSO IL PROPRIO E COMPRESA LA SEDE(46)
011300020716    4c                   if        i56mod='GRU'
011400020716     c                   movel     orgde3        og143
011500020716    5c                   if        §ogas=946
011600020716    6c                   if        wogas<>946
011700020716     C     §ogas         LOOKUP    ski                                    30
011800020716     C  N30              Z-ADD     §ogas         ski(C)
011900020716     C  N30              ADD       1             C
012000020716    6c                   endif
012100020716     c*
012200020716   x5c                   else
012300020717    6c                   if        orgfel<>wfel  or
012400020717     c                             wogas=946 and §ogas<>946
012500020716     C     ORGFEL        LOOKUP    ski                                    30
012600020716     C  N30              Z-ADD     ORGFEL        ski(C)
012700020716     C  N30              ADD       1             C
012800020716    6C                   ENDIF
012900020716    5C                   ENDIF
013000020716    4C                   ENDIF
013100020716     c*
013200020716     A***  PER AZI - I SIMFEL A CUI TRASMETTERE
013300020716     A***            ESCLUSO IL PROPRIO E COMPRESA LA SEDE(46)
013400020912    4c                   if        i56mod='AZI' and
013500020912     c                             orgfil=d55tfp and d55tfp<>wtfp
013600020716     C     d55tfp        LOOKUP    ski                                    30
013700020716     C  N30              Z-ADD     D55TFP        ski(C)
013800020716     C  N30              ADD       1             C
013900020912    4C                   ENDIF
014000020716     C*
014100020716     A***  PER POA - I P.O. PRESENTI SULLO STESSO AS DI I56PPO
014200020716     A***            COMPRESO IL PROPRIO ED ESCLUSA LA SEDE
014300020716    4c                   if        i56mod='POA'
014400020827     C                   IF        ORGFEL=WFEL and orgfil<>46
014500020716     C     ORGFIL        LOOKUP    ski                                    30
014600020716     C  N30              Z-ADD     ORGFIL        ski(C)
014700020716     C  N30              ADD       1             C
014800020716    5C                   ENDIF
014900020716    4C                   ENDIF
015000020827     A***  PER POS - I P.O. PRESENTI SULLO STESSO §ogas DI I56PPO
015100020827     A***            COMPRESO IL PROPRIO ED ESCLUSA LA SEDE
015200020827    4c                   if        i56mod='POS'
015300020827     c                   movel     orgde3        og143
015400020827     C                   IF        §ogas=Wogas
015500020827     C     ORGFIL        LOOKUP    ski                                    30
015600020827     C  N30              Z-ADD     ORGFIL        ski(C)
015700020827     C  N30              ADD       1             C
015800020827    5C                   ENDIF
015900020827    4C                   ENDIF
016000020912
016100020912      * Per TPS - I Terminal di partenza presenti sullo stesso §ogas di I56PPO
016200020912      *           compreso il proprio ed esclusa la sede
016300020912    4c                   If        I56Mod = 'TPS'
016400020912     c                   Eval      Og143 = Orgde3
016500020912    5c                   If        §ogas = Wogas and
016600020912     c                             OrgFil = D55tfp
016700020912     c     D55tfp        Lookup    Ski                                    30
016800020912     c  n30              Z-Add     D55tfp        Ski(C)
016900020912     c  n30              Add       1             c
017000020912    5c                   EndIf
017100020912    4c                   EndIf
017200020716     c**
017300971120    3C                   END
017400020716    2C                   END
017500020716     C                   READ      AZORG                                  31
017600020716    1C                   ENDdo
017700020716     c**
017800020716     c     fine          tag
017900020716     c*
018000020716     C*
018100020716     C                   CLEAR                   DSLV55
018200020716     C                   MOVEL     'C'           D55TLA
018300020716     C                   CALL      'FNLV55R'
018400020716     C                   PARM                    DSLV55
018500020716     c**
018600020716     c                   seton                                        lr
018700020716**
018800020716Punto operativo di partenza inesistente o non utilizzabile
018900020716Madalità di richiamo del pgm non prevista
