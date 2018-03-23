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
001300131030     fFnorpe1l  uf   e           k disk
001400131030     fFnorve1l  uf   e           k disk
001500100811     fFidpo01l  uf   e           k disk
001600980721
001700980721     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG ERRORI
001800010406     D kpoe            s                   LIKE(ORMpoe)
001900010406     D knsr            s                   LIKE(ORMnsr)
002000010406     D knor            s                   LIKE(ORMnor)
002100010406     D knrv            s                   LIKE(ORMnrv)
002200010406     D watb            s                   LIKE(ORMatb)
002300000000     D KPJBA         E DS
002400010406     D fior30ds      E DS
002500980721
002600010406     c                   IF        i30mod <> 'A'  and  i30mod <> 'R'
002700010406     c                                            and  i30mod <> 'D'
002800010406     c                   movel     'E'           o30err
002900010406     c                   movel     MSG(1)        o30msg
003000980721     C                   ELSE
003100010411
003200010411      * FNORN
003300010411     c     korm          setll     fnorn01l
003400010411     c                   DO        *hival
003500010411     c     korm          reade     fnorn01l
003600010411     c                   if        %eof(fnorn01l)
003700010411     c                   leave
003800010411     c                   endif
003900040818     c  n02              movel     watb          ORNatb
004000040818     c  n02              Z-Add     *Date         OrnDtr
004100010411     c  n02              update    fnorn000
004200010411     c   02              delete    fnorn000
004300010411     c                   ENDDO
004400010411
004500010411      * FNORF
004600010411     c     korm          setll     fnorf01l
004700010411     c                   DO        *hival
004800010411     c     korm          reade     fnorf01l
004900010411     c                   if        %eof(fnorf01l)
005000010411     c                   leave
005100010411     c                   endif
005200040818     c  n02              movel     watb          ORFatb
005300010411     c  n02              update    fnorf000
005400010411     c   02              delete    fnorf000
005500010411     c                   ENDDO
005600070705
005700070705      * FNORG
005800070705     c     korm          setll     fnorg01l
005900070705     c                   DO        *hival
006000070705     c     korm          reade     fnorg01l
006100070705     c                   if        %eof(fnorg01l)
006200070705     c                   leave
006300070705     c                   endif
006400070705     c  n02              movel     watb          ORgatb
006500070705     c  n02              update    fnorg000
006600070705     c   02              delete    fnorg000
006700070705     c                   ENDDO
006800100218
006900100218      * FNORA
007000100218     c     korm          setll     fnora01l
007100100218     c                   DO        *hival
007200100218     c     korm          reade     fnora01l
007300100218     c                   if        %eof(fnora01l)
007400100218     c                   leave
007500100218     c                   endif
007600100218     c   02              delete    fnora000
007700100218     c                   ENDDO
007800100218
007900100218      * FNORE
008000100218     c     korm          setll     fnore01l
008100100218     c                   DO        *hival
008200100218     c     korm          reade     fnore01l
008300100218     c                   if        %eof(fnore01l)
008400100218     c                   leave
008500100218     c                   endif
008600100218     c   02              delete    fnore000
008700100218     c                   ENDDO
008800131030
008900131030      * FNORPE
009000131030     c     korm          setll     fnorpe1l
009100131030     c                   DO        *hival
009200131030     c     korm          reade     fnorpe1l
009300131030     c                   if        %eof(fnorpe1l)
009400131030     c                   leave
009500131030     c                   endif
009600131030     c   02              delete    fnorpe00
009700131030     c                   ENDDO
009800131030
009900131030      * FNORVE
010000131030     c     korm          setll     fnorve1l
010100131030     c                   DO        *hival
010200131030     c     korm          reade     fnorve1l
010300131030     c                   if        %eof(fnorve1l)
010400131030     c                   leave
010500131030     c                   endif
010600131030     c   02              delete    fnorve00
010700131030     c                   ENDDO
010800100811
010900100811      * FIDPO
011000100811     c     korm          chain     fidpo01l
011100100811     c                   if        %found(fidpo01l)
011200100811     c  n02              eval      DPOann = watb
011300100811     c  n02              update    fidpo000
011400100811     c   02              delete    fidpo000
011500100811     c                   ENDIF
011600980721
011700010406      * FNORM
011800010406     c     korm          chain     fnorm01l
011900010406     c                   if        %found(fnorm01l)
012000010406
012100010406     C                   if        *in02 = *on
012200010406      * FNORV
012300010406     c     korm          setll     fnorv01l
012400010406     c                   DO        *hival
012500010406     c     korm          reade     fnorv01l
012600010406     c                   if        %eof(fnorv01l)
012700010406     c                   leave
012800010406     c                   endif
012900010406     c                   delete    fnorv000
013000010406     c                   ENDDO
013100010406      * FNORP
013200010406     c     korm          setll     fnorp01l
013300010406     c                   DO        *hival
013400010406     c     korm          reade     fnorp01l
013500010406     c                   if        %eof(fnorp01l)
013600010406     c                   leave
013700010406     c                   endif
013800010406     c                   delete    fnorp000
013900010406     c                   ENDDO
014000010406
014100010406     c                   delete    fnorm000
014200010406     c                   endif
014300010411
014400040818     c  n02              movel     watb          ORMatb
014500040818     c  n02              Z-Add     *Date         OrmDtt
014600010411     c  n02              update    fnorm000
014700980721     c                   endif
014800980721
014900980721     c                   ENDIF
015000980721
015100980721     c                   EVAL      *INLR = *ON
015200980721
015300980721      *-------------------------------------------------------------------------*
015400980721      * Subroutine iniziale
015500980721      *-------------------------------------------------------------------------*
015600980721     c     *inzsr        BEGSR
015700980721      *
015800980721     C     *ENTRY        PLIST
015900980721     C                   PARM                    KPJBA
016000010406     c                   parm                    fior30ds
016100980721      *
016200010406     c                   IF        i30MOD = 'A'
016300980721     c                   movel     'A'           Watb
016400980721     c                   ENDIF
016500980721
016600010406     c                   IF        i30MOD = 'R'
016700980721     c                   movel     ' '           Watb
016800980721     c                   ENDIF
016900990820      *
017000010406     c                   IF        i30MOD = 'D'
017100990820     c                   seton                                        02
017200990820     c                   ENDIF
017300980721      *
017400010406     c                   movel     i30poe        kpoe
017500010406     c                   movel     i30nsr        knsr
017600010406     c                   movel     i30nor        knor
017700010406     c                   movel     i30nrv        knrv
017800980721      *
017900010406     C     Korm          KLIST
018000010406     C                   KFLD                    Kpoe
018100010406     C                   KFLD                    Knsr
018200010406     C                   KFLD                    Knor
018300010406     C                   KFLD                    Knrv
018400980721      *
018500980721     c                   endsr
018600980721      *-------------------------------------------------------------------------*
018700980721**  MSG lung. 78                                                             *
018800980721Modalità richiamo programma errata: telefonare in Sede !!!
