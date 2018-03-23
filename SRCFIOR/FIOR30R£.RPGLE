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
001600141110     fFISAO01L  UF   E           K DISK
001700980721
001800980721     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG ERRORI
001900010406     D kpoe            s                   LIKE(ORMpoe)
002000010406     D knsr            s                   LIKE(ORMnsr)
002100010406     D knor            s                   LIKE(ORMnor)
002200010406     D knrv            s                   LIKE(ORMnrv)
002300010406     D watb            s                   LIKE(ORMatb)
002400000000     D KPJBA         E DS
002500010406     D fior30ds      E DS
002600980721
002700010406     c                   IF        i30mod <> 'A'  and  i30mod <> 'R'
002800010406     c                                            and  i30mod <> 'D'
002900010406     c                   movel     'E'           o30err
003000010406     c                   movel     MSG(1)        o30msg
003100980721     C                   ELSE
003200010411
003300010411      * FNORN
003400010411     c     korm          setll     fnorn01l
003500010411     c                   DO        *hival
003600010411     c     korm          reade     fnorn01l
003700010411     c                   if        %eof(fnorn01l)
003800010411     c                   leave
003900010411     c                   endif
004000040818     c  n02              movel     watb          ORNatb
004100040818     c  n02              Z-Add     *Date         OrnDtr
004200010411     c  n02              update    fnorn000
004300010411     c   02              delete    fnorn000
004400010411     c                   ENDDO
004500010411
004600010411      * FNORF
004700010411     c     korm          setll     fnorf01l
004800010411     c                   DO        *hival
004900010411     c     korm          reade     fnorf01l
005000010411     c                   if        %eof(fnorf01l)
005100010411     c                   leave
005200010411     c                   endif
005300040818     c  n02              movel     watb          ORFatb
005400010411     c  n02              update    fnorf000
005500010411     c   02              delete    fnorf000
005600010411     c                   ENDDO
005700070705
005800070705      * FNORG
005900070705     c     korm          setll     fnorg01l
006000070705     c                   DO        *hival
006100070705     c     korm          reade     fnorg01l
006200070705     c                   if        %eof(fnorg01l)
006300070705     c                   leave
006400070705     c                   endif
006500070705     c  n02              movel     watb          ORgatb
006600070705     c  n02              update    fnorg000
006700070705     c   02              delete    fnorg000
006800070705     c                   ENDDO
006900100218
007000100218      * FNORA
007100100218     c     korm          setll     fnora01l
007200100218     c                   DO        *hival
007300100218     c     korm          reade     fnora01l
007400100218     c                   if        %eof(fnora01l)
007500100218     c                   leave
007600100218     c                   endif
007700100218     c   02              delete    fnora000
007800100218     c                   ENDDO
007900100218
008000100218      * FNORE
008100100218     c     korm          setll     fnore01l
008200100218     c                   DO        *hival
008300100218     c     korm          reade     fnore01l
008400100218     c                   if        %eof(fnore01l)
008500100218     c                   leave
008600100218     c                   endif
008700100218     c   02              delete    fnore000
008800100218     c                   ENDDO
008900131030
009000131030      * FNORPE
009100131030     c     korm          setll     fnorpe1l
009200131030     c                   DO        *hival
009300131030     c     korm          reade     fnorpe1l
009400131030     c                   if        %eof(fnorpe1l)
009500131030     c                   leave
009600131030     c                   endif
009700131030     c   02              delete    fnorpe00
009800131030     c                   ENDDO
009900131030
010000131030      * FNORVE
010100131030     c     korm          setll     fnorve1l
010200131030     c                   DO        *hival
010300131030     c     korm          reade     fnorve1l
010400131030     c                   if        %eof(fnorve1l)
010500131030     c                   leave
010600131030     c                   endif
010700131030     c   02              delete    fnorve00
010800131030     c                   ENDDO
010900100811
011000100811      * FIDPO
011100100811     c     korm          chain     fidpo01l
011200100811     c                   if        %found(fidpo01l)
011300100811     c  n02              eval      DPOann = watb
011400100811     c  n02              update    fidpo000
011500100811     c   02              delete    fidpo000
011600100811     c                   ENDIF
011700141110
011800141110      * FISAO
011900141110     c     korm          setll     fisao01l
012000141110     c                   DO        *hival
012100141110     c     korm          reade     fisao01l
012200141110     c                   IF        %eof(fisao01l)
012300141110     c                   leave
012400141110     c                   ENDIF
012500141110     c                   delete    fisao000
012600141110     c                   ENDDO
012700980721
012800010406      * FNORM
012900010406     c     korm          chain     fnorm01l
013000010406     c                   if        %found(fnorm01l)
013100010406
013200010406     C                   if        *in02 = *on
013300010406      * FNORV
013400010406     c     korm          setll     fnorv01l
013500010406     c                   DO        *hival
013600010406     c     korm          reade     fnorv01l
013700010406     c                   if        %eof(fnorv01l)
013800010406     c                   leave
013900010406     c                   endif
014000010406     c                   delete    fnorv000
014100010406     c                   ENDDO
014200010406      * FNORP
014300010406     c     korm          setll     fnorp01l
014400010406     c                   DO        *hival
014500010406     c     korm          reade     fnorp01l
014600010406     c                   if        %eof(fnorp01l)
014700010406     c                   leave
014800010406     c                   endif
014900010406     c                   delete    fnorp000
015000010406     c                   ENDDO
015100010406
015200010406     c                   delete    fnorm000
015300010406     c                   endif
015400010411
015500040818     c  n02              movel     watb          ORMatb
015600040818     c  n02              Z-Add     *Date         OrmDtt
015700010411     c  n02              update    fnorm000
015800980721     c                   endif
015900980721
016000980721     c                   ENDIF
016100980721
016200980721     c                   EVAL      *INLR = *ON
016300980721
016400980721      *-------------------------------------------------------------------------*
016500980721      * Subroutine iniziale
016600980721      *-------------------------------------------------------------------------*
016700980721     c     *inzsr        BEGSR
016800980721      *
016900980721     C     *ENTRY        PLIST
017000980721     C                   PARM                    KPJBA
017100010406     c                   parm                    fior30ds
017200980721      *
017300010406     c                   IF        i30MOD = 'A'
017400980721     c                   movel     'A'           Watb
017500980721     c                   ENDIF
017600980721
017700010406     c                   IF        i30MOD = 'R'
017800980721     c                   movel     ' '           Watb
017900980721     c                   ENDIF
018000990820      *
018100010406     c                   IF        i30MOD = 'D'
018200990820     c                   seton                                        02
018300990820     c                   ENDIF
018400980721      *
018500010406     c                   movel     i30poe        kpoe
018600010406     c                   movel     i30nsr        knsr
018700010406     c                   movel     i30nor        knor
018800010406     c                   movel     i30nrv        knrv
018900980721      *
019000010406     C     Korm          KLIST
019100010406     C                   KFLD                    Kpoe
019200010406     C                   KFLD                    Knsr
019300010406     C                   KFLD                    Knor
019400010406     C                   KFLD                    Knrv
019500980721      *
019600980721     c                   endsr
019700980721      *-------------------------------------------------------------------------*
019800980721**  MSG lung. 78                                                             *
019900980721Modalità richiamo programma errata: telefonare in Sede !!!
