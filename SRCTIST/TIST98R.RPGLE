000100040119      *===============================================================*
000200111019      * Tist98r    * Simulazione fatturazione
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000111019     Ftist98d   cf   e             workstn
001100040119      *
001200111019     D msg             S             78    DIM(5) CTDATA PERRCD(1)
001300050614
001400050526     d kpjba         e ds
001500050614     d param           ds
001600050614     D  pardrf                 1      8  0
001700050614     D  paraam                 9     14  0
001800080714     D  v1crif                15     15
001900111019     D  v1clib                16     25
002000050613     d Azuteds       e ds                  Extname(Azute00f)
002100050613     d dDatiute      e ds
002200050613     d Tibs34ds      e ds                  Inz
002300111019      *
002400111019     D CHKO            s             40    dim(1) ctdata  perrcd(1)
002500111019      *
002600050614     D WLBDA8          DS                  INZ
002700050614     D  G08DAT                 1      8  0
002800050614     D  G08INV                 9     16  0
002900050614     D  G08ERR                17     17
003000050614     D  G08TGI                18     22  0
003100050614
003200050527     D*
003300040119
003400040119      *
003500050613     c     *entry        plist
003600050613     c                   parm                    kpjba
003700060914     c                   movel     kpjbu         pilota            1
003800050613     c*
003900050613     c     *dtaara       define    §azute        azuteds
004000050613     c     *dtaara       define    §datiute      ddatiute
004100050613     c                   in(E)     *dtaara
004200050613     c                   If        %error  or RSUT = *blanks
004300050613     c                   CLEAR                   tibs34ds
004400050613     c                   CALL      'TIBS34R'
004500050613     c                   PARM                    tibs34ds
004600050613     c                   in        *dtaara
004700050613     c                   EndIf
004800050614     c*
004900050614     c                   clear                   v1cdrf
005000050614     c                   clear                   v1caam
005100050614     c* emissione video
005200050613     c     emiss         tag
005300111019     C                   exfmt     st98d01
005400050614     c                   setoff                                       9028
005500050613     C                   if        *in03
005600050613     C                   goto      fine
005700050613     c                   endif
005800050614     c                   exsr      ctrd01
005900050614     c                   if        *in06 = *off  or *in90 = *on
006000050613     c                   goto      emiss
006100050613     c                   endif
006200111019
006300111019     c                   eval      kcoaz = 'ST99'
006400060914     c                   movel(p)  param         kpjbu
006500111019     c******             call      'BCH10'
006600111019     c                   call      'TIST99C'
006700060914     c                   parm                    kpjba
006800111019
006900050613     c     fine          tag
007000050613     c                   seton                                        lr
007100050614     c**********************************************************************
007200050614     C     ctrd01        begsr
007300050614     c                   setoff                                       4041
007400050614     c                   clear                   pardrf
007500050614     c                   clear                   paraam
007600050614     c* controllo data
007700111019     c* .....obbligatorietà se non è stato specificato che si trattta di SIMULAZIONE
007800080714     c                   if        v1cdrf = *zeros and v1crif = ' '
007900050614     c                   seton                                        902840
008000050614     c                   movel     msg(1)        v1cmsg
008100050614     c                   goto      endctr
008200050614     c                   endif
008300050614     c* .....correttezza
008400080714     c                   If        v1cdrf > *zeros
008500050614     c                   clear                   wlbda8
008600050614     C                   z-add     v1cdrf        G08dat
008700050614     C                   CALL      'XSRDA8'
008800050614     C                   PARM                    WLBDA8
008900050614IF  2C     G08ERR        IFEQ      '1'
009000050614     C                   seton                                        409028
009100050614     C                   MOVEL     MSG(2)        v1cmsg
009200050614     c                   goto      endctr
009300050614E   2C                   ENDIF
009400050614     C                   Z-ADD     G08DAT        V1cdrf
009500050614     C                   Z-ADD     G08INV        pardrf            8 0
009600080714     c                   endif
009700050614     c*
009800050614     c* controllo mese/giorno
009900050614     c* .....obbligatorietà
010000050614     c     v1caam        ifeq      *zeros
010100050614     c                   seton                                        902841
010200050614     c                   movel     msg(3)        v1cmsg
010300050614     c                   goto      endctr
010400050614     c                   endif
010500050614     c* .....correttezza
010600050614     C                   MOVEL     v1caam        G08INV
010700050614     C                   MOVE      01            G08INV
010800050614     C                   Z-ADD     *ZEROS        G08DAT
010900050614     C                   MOVEL     '3'           G08ERR
011000050614     C                   CALL      'XSRDA8'
011100050614     C                   PARM                    WLBDA8
011200050614IF  2C     G08ERR        IFEQ      '1'
011300050614     C                   seton                                        419028
011400050614     C                   MOVEL     MSG(4)        v1cmsg
011500050614     c                   goto      endctr
011600050614E   2C                   ENDIF
011700050614     c                   z-add     v1caam        paraam
011800111019     c* controllo libreria delle tariffe
011900111019     c* .....non è obbligatoria ma se scritta deve esistere
012000111019     c     v1clib        ifne      *blanks
012100111019
012200111019     c                   eval      %subst(chko: 12: 10) = v1clib
012300111019     c                   eval      comman=chko(1)
012400111019     c                   call(e)   'QCMDEXC'
012500111019     c                   parm                    comman           80
012600111019     c                   parm      40            lung             15 5
012700111019     c                   if        %error
012800111019     c                   seton                                        902842
012900111019     c                   movel     msg(5)        v1cmsg
013000111019     c                   goto      endctr
013100111019     c                   endif
013200111019     c                   endif
013300111019
013400050614     c     endctr        endsr
013500050614**
013600050614Data obbligatoria
013700050614Data errata
013800050614Anno/Mese obbligatorio
013900050614Anno/Mese errato
014000111019Libreria inesistente
014100111019**
014200111019CHKOBJ OBJ(          ) OBJTYPE(*LIB)
