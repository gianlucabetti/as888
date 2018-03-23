000100040119      *===============================================================*
000200120511      * Ticf50r    * Confronto fatturazione solo assegnati
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000120511     Fticf50d   cf   e             workstn
001100040119      *
001200050614     D msg             S             78    DIM(4) CTDATA PERRCD(1)
001300050614
001400050526     d kpjba         e ds
001500050614     d param           ds
001600101108     D  paraam                 1      6  0
001700101108     d Azuteds       e ds                  Extname(Azute00f)
001800050613     d dDatiute      e ds
001900050613     d Tibs34ds      e ds                  Inz
002000050614     D WLBDA8          DS                  INZ
002100050614     D  G08DAT                 1      8  0
002200050614     D  G08INV                 9     16  0
002300050614     D  G08ERR                17     17
002400050614     D  G08TGI                18     22  0
002500050614
002600050527     D*
002700040119
002800040119      *
002900050613     c     *entry        plist
003000050613     c                   parm                    kpjba
003100050613     c*
003200050613     c     *dtaara       define    §azute        azuteds
003300050613     c     *dtaara       define    §datiute      ddatiute
003400050613     c                   in(E)     *dtaara
003500050613     c                   If        %error  or RSUT = *blanks
003600050613     c                   CLEAR                   tibs34ds
003700050613     c                   CALL      'TIBS34R'
003800050613     c                   PARM                    tibs34ds
003900050613     c                   in        *dtaara
004000050613     c                   EndIf
004100050614     c*
004200050614     c                   clear                   v1caam
004300050614     c* emissione video
004400050613     c     emiss         tag
004500120511     C                   exfmt     cf50d01
004600050614     c                   setoff                                       9028
004700050613     C                   if        *in03
004800050613     C                   goto      fine
004900050613     c                   endif
005000050614     c                   exsr      ctrd01
005100050614     c                   if        *in06 = *off  or *in90 = *on
005200050613     c                   goto      emiss
005300050613     c                   endif
005400101108
005500120511     c                   eval      kcoaz = 'CF51'
005600060914     c                   movel(p)  param         kpjbu
005700080714     c                   call      'BCH10'
005800060914     c                   parm                    kpjba
005900050613     c     fine          tag
006000050613     c                   seton                                        lr
006100050614     c**********************************************************************
006200050614     C     ctrd01        begsr
006300050614     c                   setoff                                       4041
006400050614     c                   clear                   paraam
006500050614     c*
006600050614     c* controllo mese/giorno
006700050614     c* .....obbligatorietà
006800050614     c     v1caam        ifeq      *zeros
006900050614     c                   seton                                        902841
007000050614     c                   movel     msg(3)        v1cmsg
007100050614     c                   goto      endctr
007200050614     c                   endif
007300050614     c* .....correttezza
007400050614     C                   MOVEL     v1caam        G08INV
007500050614     C                   MOVE      01            G08INV
007600050614     C                   Z-ADD     *ZEROS        G08DAT
007700050614     C                   MOVEL     '3'           G08ERR
007800050614     C                   CALL      'XSRDA8'
007900050614     C                   PARM                    WLBDA8
008000050614IF  2C     G08ERR        IFEQ      '1'
008100050614     C                   seton                                        419028
008200050614     C                   MOVEL     MSG(4)        v1cmsg
008300050614     c                   goto      endctr
008400050614E   2C                   ENDIF
008500050614     c                   z-add     v1caam        paraam
008600050614     c     endctr        endsr
008700050614**
008800050614Data obbligatoria
008900050614Data errata
009000050614Anno/Mese obbligatorio
009100050614Anno/Mese errato
