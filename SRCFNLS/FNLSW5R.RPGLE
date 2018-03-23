000100980717     H DECEDIT('0,') DATEDIT(*DMY.)
000200150227     H* FNLSW5R *----------------------------------------------------*
000300150227     H*       inserimento CUP e CIG in sede
000400000000     H*--------------------------------------------------------------*
000500150227     FFNLSW5D   CF   E             WORKSTN
000600150227     fTitas30c  if   e           k Disk    UsrOpn extfile(Wtitas)
000700150227     fTita430c  if   e           k Disk    UsrOpn extfile(Wtita4)
000800150227     D MSG             S             78    DIM(04) CTDATA PERRCD(1)
000900980108     D*
001000920421     D KPJBA         E DS
001100941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
001200941228     D***
001300941228     D WLBDAT          DS                  INZ
001400941228     D  G02DAT                 1      8  0
001500941228     D  G02INV                 9     16  0
001600941228     D  G02ERR                17     17
001700941228     D  G02TGI                18     22  0
001800941228     D***
001900941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
002000941228     D***
002100941228     D WGIDAT          DS                  INZ
002200941228     D  GIODAT                 1      8  0
002300941228     D  GIOINV                 9     16  0
002400941228     D  GIOTGI                17     21  0
002500021203
002600110908     D TIBS34DS      E DS
002700110908     d Azuteds       e ds                  extname(Azute00f)
002800110908     d dDatiute      e ds
002900150227     D DSLV19        E DS                  EXTNAME(FNLV19DS)
003000111003     d Dataiso         s               d   datfmt(*iso)
003100111003     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003200111003     d dateu           s              8  0
003300150227     D DSBL44        E DS
003400150227     d Wtitas          s             21    inz('GAITRAGRPS/TITAS30C')
003500150227     d Wtita4          s             21    inz('GAITRAGRPS/TITA430C')
003600900702     C*****************************************************************
003700000000     C     *ENTRY        PLIST
003800000000     C                   PARM                    KPJBA
003900950530     C*
004000110912     C                   Z-ADD     1             CODUT             1 0
004100110908     c     *dtaara       define    §azute        azuteds
004200110908     c     *dtaara       define    §datiute      ddatiute
004300110908     c                   in(E)     *dtaara
004400110908    1c                   If        %error  or RSUT = *blanks
004500110908     c                   Clear                   Tibs34ds
004600110908     c                   Call      'TIBS34R'
004700110908     c                   Parm                    Tibs34ds
004800110908     c                   In        *dtaara
004900110908    1c                   EndIf
005000920901     C*---------------------------------------------------------------*
005100111003     c                   eval      dataiso=datasys
005200111003     c                   eval      dateu= %dec(dataiso)
005300150227     c                   movel     dateu         vidaas
005400150227     c                   z-add     40            vidnrs
005500150227     c
005600150227     C                   OPEN(e)   TITAS30C
005700150227     c
005800150227     c                   if        not %open(TITAS30C)
005900150227     c                   eval      %subst(Wtitas:7:4)='GRU '
006000150227     C                   OPEN      Titas30C
006100150227     c                   endif
006200150227     C                   OPEN(e)   TITA430C
006300150227     c
006400150227     c                   if        not %open(TITA430C)
006500150227     c                   eval      %subst(Wtita4:7:4)='GRU '
006600150227     C                   OPEN      Tita430C
006700150227     c                   endif
006800111003     C*---------------------------------------------------------------*
006900150227     C     KTAs          KLIST
007000150227     C                   KFLD                    vidaas
007100150227     C                   KFLD                    vidlnp
007200150227     C                   KFLD                    vidnrs
007300150227     C                   KFLD                    vidnsp
007400150227     C     KTA4          KLIST
007500150227     C                   KFLD                    vidaas
007600150227     C                   KFLD                    vidlnp
007700150227     C                   KFLD                    vidnrs
007800150227     C                   KFLD                    vidnsp
007900150227     C                   KFLD                    ktrc              1
008000150227     c                   eval      ktrc='4'
008100941228     C*---------------------------------------------------------------*
008200150227     c* solo utente di  verona
008300150227     c                   if        dutpou <>043 and dutpou<>046
008400150227     C                   MOVEL     MSG(1)        VIDMSG
008500150227     c                   seton                                        2899
008600150227     c                   else
008700150227     c                   setoff                                       289099
008800150227     c                   endif
008900150227     c                   clear                   wcupcup
009000150227     c                   clear                   wcupcig
009100920730     C*
009200920424     C     INIZIO        TAG
009300150227     c                   exfmt     LS01WCUP
009400150227     c                   setoff                                       2890
009500150227
009600150227     c                   if        *in99
009700150227     c                   goto      fine
009800150227     c                   endif
009900150227     c
010000150227     c                   if        *inkc
010100150227     c                   goto      fine
010200150227     c                   endif
010300150227     c
010400150227     c* controllo la spedizione su TITAS
010500150227     c     ktas          chain     titas30c
010600150227     c                   if        not %found(titas30c)
010700150227     C                   MOVEL     MSG(2)        VIDMSG
010800150227     c                   seton                                        2890
010900150227     c                   goto      inizio
011000150227     c                   endif
011100150227     c                   if        vidnrs<>40
011200150227     C                   MOVEL     MSG(3)        VIDMSG
011300150227     c                   seton                                        2890
011400150227     c                   goto      inizio
011500150227     c                   endif
011600150227     c* F9 visual da bolla        tita4 record "4" lo propongo
011700150227     c                   if        *inki
011800150227     c     kta4          chain     tita430c
011900150227     c                   if        %found(tita430c)
012000150227     C                   MOVEL     ta4NOT        DSBL44
012100150227     c                   else
012200150227     C                   CLEAR                   DSBL44
012300150227     c                   endif
012400150227     c
012500150227     c                   if        §b44cup<> *blanks or §b44cig<>*blanks
012600150227     c                   movel     §b44cup       wcupcup
012700150227     c                   movel     §b44cig       wcupcig
012800150227     c                   endif
012900150227     c                   goto      inizio
013000150227     c                   endif
013100150227     c
013200150227     c                   if        wcupcup=  *blanks and wcupcig= *blanks
013300150227     C                   MOVEL     MSG(4)        VIDMSG
013400150227     c                   seton                                        2890
013500150227     c                   goto      inizio
013600150227     c                   endif
013700150227     c*
013800150227     c                   if         *inkf
013900150227     c                   Clear                   dsbl44
014000150227     c                   Clear                   dslv19
014100150227     C                   MOVEL     'T'           D19ftr
014200150227     C                   MOVEL     'S'           D19agg
014300150227     C                   MOVEL     tasAAS        D19AAS
014400150227     C                   MOVEL     tasLNP        D19LNP
014500150227     C                   MOVEL     tasNRS        D19NRS
014600150227     C                   MOVEL     tasNSP        D19NSP
014700150227     c                   Movel     '4'           d19trc
014800150227     c                   movel     wcupcup       §b44cup
014900150227     c                   movel     wcupcig       §b44cig
015000150227     c                   Movel     dsbl44        d19nt1
015100150227     c                   Call      'FNLV19R'
015200150227     c                   Parm                    Dslv19
015300150227     c*
015400150227     c* celar dei campi a video
015500150227     c                   clear                   vidlnp
015600150227     c                   clear                   vidnsp
015700150227     c                   clear                   wcupcup
015800150227     c                   clear                   wcupcig
015900150227     c                   endif
016000150227
016100150227     c                   goto      inizio
016200150227     c
016300000000     C     FINE          TAG
016400000000     C                   SETON                                        LR
016500920730     C*
016600961017**  MSG
016700150227Utenti abilitati:solo della fil.043 o di sede
016800150227spedizione inesistente                                                        2
016900150227spedizione non serie 40 : WURTH                                               3
017000150227inserire CUP e/o CIG
