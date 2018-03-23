000100040119      *===============================================================*
000200050613      * Tist89r    * Confronto fatturazione (progetto 581)
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000050613     Ftist89d   cf   e             workstn
001100040119      *
001200050614     D msg             S             78    DIM(4) CTDATA PERRCD(1)
001300050614
001400050526     d kpjba         e ds
001500050614     d param           ds
001600050614     D  pardrf                 1      8  0
001700050614     D  paraam                 9     14  0
001800080714     D  v1crif                15     15
001900050613     d Azuteds       e ds                  Extname(Azute00f)
002000050613     d dDatiute      e ds
002100050613     d Tibs34ds      e ds                  Inz
002200050614     D WLBDA8          DS                  INZ
002300050614     D  G08DAT                 1      8  0
002400050614     D  G08INV                 9     16  0
002500050614     D  G08ERR                17     17
002600050614     D  G08TGI                18     22  0
002700050614
002800050527     D*
002900040119
003000040119      *
003100050613     c     *entry        plist
003200050613     c                   parm                    kpjba
003300060914     c                   movel     kpjbu         pilota            1
003400050613     c*
003500050613     c     *dtaara       define    §azute        azuteds
003600050613     c     *dtaara       define    §datiute      ddatiute
003700050613     c                   in(E)     *dtaara
003800050613     c                   If        %error  or RSUT = *blanks
003900050613     c                   CLEAR                   tibs34ds
004000050613     c                   CALL      'TIBS34R'
004100050613     c                   PARM                    tibs34ds
004200050613     c                   in        *dtaara
004300050613     c                   EndIf
004400050614     c*
004500050614     c                   clear                   v1cdrf
004600050614     c                   clear                   v1caam
004700050614     c* emissione video
004800050613     c     emiss         tag
004900050613     C                   exfmt     st89d01
005000050614     c                   setoff                                       9028
005100050613     C                   if        *in03
005200050613     C                   goto      fine
005300050613     c                   endif
005400050614     c                   exsr      ctrd01
005500050614     c                   if        *in06 = *off  or *in90 = *on
005600050613     c                   goto      emiss
005700050613     c                   endif
005800070411     c* Tolgo il controllo del pilota in quanto non serve più in quanto veniva usato
005900070411      * quando esistevano due programmi di confronto fatturazione
006000070411     c******             If        pilota = 'N'
006100080714     c                   eval      kcoaz = 'ST92'
006200060914     c                   movel(p)  param         kpjbu
006300080714     c                   call      'BCH10'
006400060914     c                   parm                    kpjba
006500070411     c******             else
006600070411     c******             eval      kcoaz = 'ST90'
006700070411     c******             movel     param         kpjbu
006800070411     c******             call      'BCH10'
006900070411     c******             parm                    kpjba
007000070411     c******             endif
007100050613     c     fine          tag
007200050613     c                   seton                                        lr
007300050614     c**********************************************************************
007400050614     C     ctrd01        begsr
007500050614     c                   setoff                                       4041
007600050614     c                   clear                   pardrf
007700050614     c                   clear                   paraam
007800050614     c* controllo data
007900080714     c* .....obbligatorietà se non è stato specificato che si trattta di rifatturazione
008000080714     c                   if        v1cdrf = *zeros and v1crif = ' '
008100050614     c                   seton                                        902840
008200050614     c                   movel     msg(1)        v1cmsg
008300050614     c                   goto      endctr
008400050614     c                   endif
008500050614     c* .....correttezza
008600080714     c                   If        v1cdrf > *zeros
008700050614     c                   clear                   wlbda8
008800050614     C                   z-add     v1cdrf        G08dat
008900050614     C                   CALL      'XSRDA8'
009000050614     C                   PARM                    WLBDA8
009100050614IF  2C     G08ERR        IFEQ      '1'
009200050614     C                   seton                                        409028
009300050614     C                   MOVEL     MSG(2)        v1cmsg
009400050614     c                   goto      endctr
009500050614E   2C                   ENDIF
009600050614     C                   Z-ADD     G08DAT        V1cdrf
009700050614     C                   Z-ADD     G08INV        pardrf            8 0
009800080714     c                   endif
009900050614     c*
010000050614     c* controllo mese/giorno
010100050614     c* .....obbligatorietà
010200050614     c     v1caam        ifeq      *zeros
010300050614     c                   seton                                        902841
010400050614     c                   movel     msg(3)        v1cmsg
010500050614     c                   goto      endctr
010600050614     c                   endif
010700050614     c* .....correttezza
010800050614     C                   MOVEL     v1caam        G08INV
010900050614     C                   MOVE      01            G08INV
011000050614     C                   Z-ADD     *ZEROS        G08DAT
011100050614     C                   MOVEL     '3'           G08ERR
011200050614     C                   CALL      'XSRDA8'
011300050614     C                   PARM                    WLBDA8
011400050614IF  2C     G08ERR        IFEQ      '1'
011500050614     C                   seton                                        419028
011600050614     C                   MOVEL     MSG(4)        v1cmsg
011700050614     c                   goto      endctr
011800050614E   2C                   ENDIF
011900050614     c                   z-add     v1caam        paraam
012000050614     c     endctr        endsr
012100050614**
012200050614Data obbligatoria
012300050614Data errata
012400050614Anno/Mese obbligatorio
012500050614Anno/Mese errato
