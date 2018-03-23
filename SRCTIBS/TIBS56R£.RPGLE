000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950214     F*--------------------------------------------------------------*
000300020716     F*       reperimento lista AS/P.O. a cui trasmettere
000400000000     F*--------------------------------------------------------------*
000500900514     FAZORG01L  IF   E           K DISK
000600020716     D MSG             S             78    DIM(2) CTDATA PERRCD(1)              MSG VIDEO
000700020716     D og143         E DS
000800020716     D DSLV55        E DS                  EXTNAME(FNLV55DS)
000900020716     D tibs56ds      E DS
001000020716     d  ski                    7    756  0 dim(250)
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
002300020716    1c                   if        i56ppo=0
002400020716     c                   eval      i56ppo=46
002500020716    1c                   endif
002600020716     c
002700020716     c     i56ppo        chain     azorg01l                           31
002800020716    1c                   if        not *in31
002900020716     c                   movel     orgde3        og143
003000020716     c                   movel     §ogas         wogas
003100020716     c                   movel     orgfel        wfel
003200020716     c                   else
003300020716     c                   movel     '1'           o56err
003400020716     c                   movel     msg(1)        o56msg
003500020716     c                   goto      fine
003600020716    1c                   endif
003700020716     c**
003800020716     c** controllo la modalità di caricamento
003900020716     c                   if        i56mod<>'UNI' and i56mod<>'GRU' and
004000020827     c                             i56mod<>'AZI' AND I56MOD<>'POA' and
004100020912     c                             i56mod<>'POS' and I56Mod <> 'TPS'
004200020716     c                   movel     '1'           o56err
004300020716     c                   movel     msg(2)        o56msg
004400020716     c                   goto      fine
004500020716     c                   endif
004600021003     c**
004700021003     c** Richiamo con POA da SEDE ritorna solo 046 in schiera
004800021003     c                   if        i56mod = 'POA' and i56ppo = 046
004900021003     c                   movel     046           SKI(1)
005000021003     c                   goto      fine
005100021003     c                   endif
005200020716     c
005300020912     c                   if        i56mod='AZI' or I56Mod = 'TPS'
005400020716     C** udate che prendo da TIME
005500020716     C                   TIME                    W0140            14 0
005600020716     C                   MOVE      W0140         UDATE8            8 0          GG/MM/AAAA
005700020716     C                   Z-ADD     UDATE8        WDATA
005800020716     C                   Z-ADD     WGG           WMMGG             4 0
005900020716     C                   MOVEL     WMM           WMMGG
006000020716     C                   Z-ADD     WMMGG         dateu             8 0          AAAA/MM/GG
006100020716     C                   MOVEL     WAA           dateu
006200020716     c* predno il terminal di partenza di ppo
006300020716     C                   CLEAR                   DSLV55
006400020716     C                   MOVEL     'P'           D55TPT
006500020716     C                   MOVEL     i56ppo        D55LIN
006600020716     C                   MOVEL     dateu         D55DRF
006700020716     C                   CALL      'FNLV55R'
006800020716     C                   PARM                    DSLV55
006900020716    5C     D55ERR        IFNE      ' '
007000020716     C                   MOVEL     ORGFIL        wtfp
007100020716     c                   else
007200020716     C                   MOVEL     d55tfp        wtfp
007300020716    5C                   ENDIF
007400020716     c                   endif
007500020716     c**
007600020528     c**
007700020515     C                   Z-ADD     1             C                 4 0
007800971120     C     *LOVAL        SETLL     AZORG
007900971120     C                   READ      AZORG                                  31
008000971120    1C     *IN31         DOWEQ     '0'
008100971120     C*
008200020716    2c                   if        orgfag='F' or orgfag='A' or orgfil=46
008300020716     C* NON ANNULLATO
008400971120    3C     ORGFVA        IFEQ      ' '
008500020912
008600020912     c**
008700020912      * Solo per alcuni richiami
008800020912     c                   If        I56Mod ='AZI' or I56Mod = 'TPS'
008900020912     C* CONTROLLO SE E' TERMINAL
009000020912     C                   CLEAR                   DSLV55
009100020912     C                   MOVEL     'P'           D55TPT
009200020912     C                   MOVEL     ORGFIL        D55LIN
009300020912     C                   MOVEL     dateu         D55DRF
009400020912     C                   CALL      'FNLV55R'
009500020912     C                   PARM                    DSLV55
009600020912    5C     D55ERR        IFNE      ' '
009700020912     C                   MOVEL     ORGFIL        D55TFP
009800020912    5C                   ENDIF
009900020912     c                   EndIf
010000020716     c**
010100020716     A***  PER UNI - GLI ORGFEL A CUI TRASMETTERE
010200020716     A***            ESCLUSO IL PROPRIO E COMPRESA LA SEDE(46)
010300020716    4c                   if        i56mod='UNI' and orgfel<>wfel
010400020716     C     ORGFEL        LOOKUP    ski                                    30
010500020716     C  N30              Z-ADD     ORGFEL        ski(C)
010600020515     C  N30              ADD       1             C
010700020515    4C                   ENDIF
010800020716     c
010900020716     A***  PER GRU - GLI ORGFEL A CUI TASMETTERE + 946,
011000020716     A***            ESCLUSO IL PROPRIO E COMPRESA LA SEDE(46)
011100020716    4c                   if        i56mod='GRU'
011200020716     c                   movel     orgde3        og143
011300020716    5c                   if        §ogas=946
011400020716    6c                   if        wogas<>946
011500020716     C     §ogas         LOOKUP    ski                                    30
011600020716     C  N30              Z-ADD     §ogas         ski(C)
011700020716     C  N30              ADD       1             C
011800020716    6c                   endif
011900020716     c*
012000020716   x5c                   else
012100020717    6c                   if        orgfel<>wfel  or
012200020717     c                             wogas=946 and §ogas<>946
012300020716     C     ORGFEL        LOOKUP    ski                                    30
012400020716     C  N30              Z-ADD     ORGFEL        ski(C)
012500020716     C  N30              ADD       1             C
012600020716    6C                   ENDIF
012700020716    5C                   ENDIF
012800020716    4C                   ENDIF
012900020716     c*
013000020716     A***  PER AZI - I SIMFEL A CUI TRASMETTERE
013100020716     A***            ESCLUSO IL PROPRIO E COMPRESA LA SEDE(46)
013200020912    4c                   if        i56mod='AZI' and
013300020912     c                             orgfil=d55tfp and d55tfp<>wtfp
013400020716     C     d55tfp        LOOKUP    ski                                    30
013500020716     C  N30              Z-ADD     D55TFP        ski(C)
013600020716     C  N30              ADD       1             C
013700020912    4C                   ENDIF
013800020716     C*
013900020716     A***  PER POA - I P.O. PRESENTI SULLO STESSO AS DI I56PPO
014000020716     A***            COMPRESO IL PROPRIO ED ESCLUSA LA SEDE
014100020716    4c                   if        i56mod='POA'
014200020827     C                   IF        ORGFEL=WFEL and orgfil<>46
014300020716     C     ORGFIL        LOOKUP    ski                                    30
014400020716     C  N30              Z-ADD     ORGFIL        ski(C)
014500020716     C  N30              ADD       1             C
014600020716    5C                   ENDIF
014700020716    4C                   ENDIF
014800020827     A***  PER POS - I P.O. PRESENTI SULLO STESSO §ogas DI I56PPO
014900020827     A***            COMPRESO IL PROPRIO ED ESCLUSA LA SEDE
015000020827    4c                   if        i56mod='POS'
015100020827     c                   movel     orgde3        og143
015200020827     C                   IF        §ogas=Wogas
015300020827     C     ORGFIL        LOOKUP    ski                                    30
015400020827     C  N30              Z-ADD     ORGFIL        ski(C)
015500020827     C  N30              ADD       1             C
015600020827    5C                   ENDIF
015700020827    4C                   ENDIF
015800020912
015900020912      * Per TPS - I Terminal di partenza presenti sullo stesso §ogas di I56PPO
016000020912      *           compreso il proprio ed esclusa la sede
016100020912    4c                   If        I56Mod = 'TPS'
016200020912     c                   Eval      Og143 = Orgde3
016300020912    5c                   If        §ogas = Wogas and
016400020912     c                             OrgFil = D55tfp
016500020912     c     D55tfp        Lookup    Ski                                    30
016600020912     c  n30              Z-Add     D55tfp        Ski(C)
016700020912     c  n30              Add       1             c
016800020912    5c                   EndIf
016900020912    4c                   EndIf
017000020716     c**
017100971120    3C                   END
017200020716    2C                   END
017300020716     C                   READ      AZORG                                  31
017400020716    1C                   ENDdo
017500020716     c**
017600020716     c     fine          tag
017700020716     c*
017800020716     C*
017900020716     C                   CLEAR                   DSLV55
018000020716     C                   MOVEL     'C'           D55TLA
018100020716     C                   CALL      'FNLV55R'
018200020716     C                   PARM                    DSLV55
018300020716     c**
018400020716     c                   seton                                        lr
018500020716**
018600020716Punto operativo di partenza inesistente o non utilizzabile
018700020716Madalità di richiamo del pgm non prevista
