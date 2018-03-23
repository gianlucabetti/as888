000100040818     H DECEDIT('0,') DATEDIT(*ymd.)
000200980721      *----------------------------------------------------*
000300010406      * Annullamento / Ripristino ORM
000400980721      *----------------------------------------------------*
000500010406     Ffnorm01l  UF   E           K DISK
000600010406     Ffnorn01l  UF   E           K DISK
000700010406     Ffnorf01l  UF   E           K DISK
000800010406     Ffnorv01l  UF   E           K DISK
000900010406     Ffnorp01l  UF   E           K DISK
001000070705     ffnorg01l  uf   e           k disk
001100100218     fFnora01l  uf   e           k disk
001200100218     fFnore01l  uf   e           k disk
001300100811     fFidpo01l  uf   e           k disk
001400980721
001500980721     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG ERRORI
001600010406     D kpoe            s                   LIKE(ORMpoe)
001700010406     D knsr            s                   LIKE(ORMnsr)
001800010406     D knor            s                   LIKE(ORMnor)
001900010406     D knrv            s                   LIKE(ORMnrv)
002000010406     D watb            s                   LIKE(ORMatb)
002100000000     D KPJBA         E DS
002200010406     D fior30ds      E DS
002300980721
002400010406     c                   IF        i30mod <> 'A'  and  i30mod <> 'R'
002500010406     c                                            and  i30mod <> 'D'
002600010406     c                   movel     'E'           o30err
002700010406     c                   movel     MSG(1)        o30msg
002800980721     C                   ELSE
002900010411
003000010411      * FNORN
003100010411     c     korm          setll     fnorn01l
003200010411     c                   DO        *hival
003300010411     c     korm          reade     fnorn01l
003400010411     c                   if        %eof(fnorn01l)
003500010411     c                   leave
003600010411     c                   endif
003700040818     c  n02              movel     watb          ORNatb
003800040818     c  n02              Z-Add     *Date         OrnDtr
003900010411     c  n02              update    fnorn000
004000010411     c   02              delete    fnorn000
004100010411     c                   ENDDO
004200010411
004300010411      * FNORF
004400010411     c     korm          setll     fnorf01l
004500010411     c                   DO        *hival
004600010411     c     korm          reade     fnorf01l
004700010411     c                   if        %eof(fnorf01l)
004800010411     c                   leave
004900010411     c                   endif
005000040818     c  n02              movel     watb          ORFatb
005100010411     c  n02              update    fnorf000
005200010411     c   02              delete    fnorf000
005300010411     c                   ENDDO
005400070705
005500070705      * FNORG
005600070705     c     korm          setll     fnorg01l
005700070705     c                   DO        *hival
005800070705     c     korm          reade     fnorg01l
005900070705     c                   if        %eof(fnorg01l)
006000070705     c                   leave
006100070705     c                   endif
006200070705     c  n02              movel     watb          ORgatb
006300070705     c  n02              update    fnorg000
006400070705     c   02              delete    fnorg000
006500070705     c                   ENDDO
006600100218
006700100218      * FNORA
006800100218     c     korm          setll     fnora01l
006900100218     c                   DO        *hival
007000100218     c     korm          reade     fnora01l
007100100218     c                   if        %eof(fnora01l)
007200100218     c                   leave
007300100218     c                   endif
007400100218     c   02              delete    fnora000
007500100218     c                   ENDDO
007600100218
007700100218      * FNORE
007800100218     c     korm          setll     fnore01l
007900100218     c                   DO        *hival
008000100218     c     korm          reade     fnore01l
008100100218     c                   if        %eof(fnore01l)
008200100218     c                   leave
008300100218     c                   endif
008400100218     c   02              delete    fnore000
008500100218     c                   ENDDO
008600100811
008700100811      * FIDPO
008800100811     c     korm          chain     fidpo01l
008900100811     c                   if        %found(fidpo01l)
009000100811     c  n02              eval      DPOann = watb
009100100811     c  n02              update    fidpo000
009200100811     c   02              delete    fidpo000
009300100811     c                   ENDIF
009400980721
009500010406      * FNORM
009600010406     c     korm          chain     fnorm01l
009700010406     c                   if        %found(fnorm01l)
009800010406
009900010406     C                   if        *in02 = *on
010000010406      * FNORV
010100010406     c     korm          setll     fnorv01l
010200010406     c                   DO        *hival
010300010406     c     korm          reade     fnorv01l
010400010406     c                   if        %eof(fnorv01l)
010500010406     c                   leave
010600010406     c                   endif
010700010406     c                   delete    fnorv000
010800010406     c                   ENDDO
010900010406      * FNORP
011000010406     c     korm          setll     fnorp01l
011100010406     c                   DO        *hival
011200010406     c     korm          reade     fnorp01l
011300010406     c                   if        %eof(fnorp01l)
011400010406     c                   leave
011500010406     c                   endif
011600010406     c                   delete    fnorp000
011700010406     c                   ENDDO
011800010406
011900010406     c                   delete    fnorm000
012000010406     c                   endif
012100010411
012200040818     c  n02              movel     watb          ORMatb
012300040818     c  n02              Z-Add     *Date         OrmDtt
012400010411     c  n02              update    fnorm000
012500980721     c                   endif
012600980721
012700980721     c                   ENDIF
012800980721
012900980721     c                   EVAL      *INLR = *ON
013000980721
013100980721      *-------------------------------------------------------------------------*
013200980721      * Subroutine iniziale
013300980721      *-------------------------------------------------------------------------*
013400980721     c     *inzsr        BEGSR
013500980721      *
013600980721     C     *ENTRY        PLIST
013700980721     C                   PARM                    KPJBA
013800010406     c                   parm                    fior30ds
013900980721      *
014000010406     c                   IF        i30MOD = 'A'
014100980721     c                   movel     'A'           Watb
014200980721     c                   ENDIF
014300980721
014400010406     c                   IF        i30MOD = 'R'
014500980721     c                   movel     ' '           Watb
014600980721     c                   ENDIF
014700990820      *
014800010406     c                   IF        i30MOD = 'D'
014900990820     c                   seton                                        02
015000990820     c                   ENDIF
015100980721      *
015200010406     c                   movel     i30poe        kpoe
015300010406     c                   movel     i30nsr        knsr
015400010406     c                   movel     i30nor        knor
015500010406     c                   movel     i30nrv        knrv
015600980721      *
015700010406     C     Korm          KLIST
015800010406     C                   KFLD                    Kpoe
015900010406     C                   KFLD                    Knsr
016000010406     C                   KFLD                    Knor
016100010406     C                   KFLD                    Knrv
016200980721      *
016300980721     c                   endsr
016400980721      *-------------------------------------------------------------------------*
016500980721**  MSG lung. 78                                                             *
016600980721Modalità richiamo programma errata: telefonare in Sede !!!
