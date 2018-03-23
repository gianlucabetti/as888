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
001300150702     fFnort01l  uf   e           k disk
001400131030     fFnorpe1l  uf   e           k disk
001500131030     fFnorve1l  uf   e           k disk
001600100811     fFidpo01l  uf   e           k disk
001700141110     fFISAO01L  UF   E           K DISK
001800980721
001900980721     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG ERRORI
002000010406     D kpoe            s                   LIKE(ORMpoe)
002100010406     D knsr            s                   LIKE(ORMnsr)
002200010406     D knor            s                   LIKE(ORMnor)
002300010406     D knrv            s                   LIKE(ORMnrv)
002400010406     D watb            s                   LIKE(ORMatb)
002500000000     D KPJBA         E DS
002600010406     D fior30ds      E DS
002700980721
002800010406     c                   IF        i30mod <> 'A'  and  i30mod <> 'R'
002900010406     c                                            and  i30mod <> 'D'
003000010406     c                   movel     'E'           o30err
003100010406     c                   movel     MSG(1)        o30msg
003200980721     C                   ELSE
003300010411
003400010411      * FNORN
003500010411     c     korm          setll     fnorn01l
003600010411     c                   DO        *hival
003700010411     c     korm          reade     fnorn01l
003800010411     c                   if        %eof(fnorn01l)
003900010411     c                   leave
004000010411     c                   endif
004100040818     c  n02              movel     watb          ORNatb
004200040818     c  n02              Z-Add     *Date         OrnDtr
004300010411     c  n02              update    fnorn000
004400010411     c   02              delete    fnorn000
004500010411     c                   ENDDO
004600010411
004700010411      * FNORF
004800010411     c     korm          setll     fnorf01l
004900010411     c                   DO        *hival
005000010411     c     korm          reade     fnorf01l
005100010411     c                   if        %eof(fnorf01l)
005200010411     c                   leave
005300010411     c                   endif
005400040818     c  n02              movel     watb          ORFatb
005500010411     c  n02              update    fnorf000
005600010411     c   02              delete    fnorf000
005700010411     c                   ENDDO
005800070705
005900070705      * FNORG
006000070705     c     korm          setll     fnorg01l
006100070705     c                   DO        *hival
006200070705     c     korm          reade     fnorg01l
006300070705     c                   if        %eof(fnorg01l)
006400070705     c                   leave
006500070705     c                   endif
006600070705     c  n02              movel     watb          ORgatb
006700070705     c  n02              update    fnorg000
006800070705     c   02              delete    fnorg000
006900070705     c                   ENDDO
007000100218
007100100218      * FNORA
007200100218     c     korm          setll     fnora01l
007300100218     c                   DO        *hival
007400100218     c     korm          reade     fnora01l
007500100218     c                   if        %eof(fnora01l)
007600100218     c                   leave
007700100218     c                   endif
007800100218     c   02              delete    fnora000
007900100218     c                   ENDDO
008000100218
008100100218      * FNORE
008200100218     c     korm          setll     fnore01l
008300100218     c                   DO        *hival
008400100218     c     korm          reade     fnore01l
008500100218     c                   if        %eof(fnore01l)
008600100218     c                   leave
008700100218     c                   endif
008800100218     c   02              delete    fnore000
008900100218     c                   ENDDO
009000150702
009100150702      * FNORT
009200150702     c     korm          setll     fnort01l
009300150702     c                   DO        *hival
009400150702     c     korm          reade     fnort01l
009500150702     c                   if        %eof(fnort01l)
009600150702     c                   leave
009700150702     c                   endif
009800150702     c   02              delete    fnort000
009900150702     c                   ENDDO
010000131030
010100131030      * FNORPE
010200131030     c     korm          setll     fnorpe1l
010300131030     c                   DO        *hival
010400131030     c     korm          reade     fnorpe1l
010500131030     c                   if        %eof(fnorpe1l)
010600131030     c                   leave
010700131030     c                   endif
010800131030     c   02              delete    fnorpe00
010900131030     c                   ENDDO
011000131030
011100131030      * FNORVE
011200131030     c     korm          setll     fnorve1l
011300131030     c                   DO        *hival
011400131030     c     korm          reade     fnorve1l
011500131030     c                   if        %eof(fnorve1l)
011600131030     c                   leave
011700131030     c                   endif
011800131030     c   02              delete    fnorve00
011900131030     c                   ENDDO
012000100811
012100100811      * FIDPO
012200100811     c     korm          chain     fidpo01l
012300100811     c                   if        %found(fidpo01l)
012400100811     c  n02              eval      DPOann = watb
012500100811     c  n02              update    fidpo000
012600100811     c   02              delete    fidpo000
012700100811     c                   ENDIF
012800141110
012900141110      * FISAO
013000141110     c     korm          setll     fisao01l
013100141110     c                   DO        *hival
013200141110     c     korm          reade     fisao01l
013300141110     c                   IF        %eof(fisao01l)
013400141110     c                   leave
013500141110     c                   ENDIF
013600141110     c                   delete    fisao000
013700141110     c                   ENDDO
013800980721
013900010406      * FNORM
014000010406     c     korm          chain     fnorm01l
014100010406     c                   if        %found(fnorm01l)
014200010406
014300010406     C                   if        *in02 = *on
014400010406      * FNORV
014500010406     c     korm          setll     fnorv01l
014600010406     c                   DO        *hival
014700010406     c     korm          reade     fnorv01l
014800010406     c                   if        %eof(fnorv01l)
014900010406     c                   leave
015000010406     c                   endif
015100010406     c                   delete    fnorv000
015200010406     c                   ENDDO
015300010406      * FNORP
015400010406     c     korm          setll     fnorp01l
015500010406     c                   DO        *hival
015600010406     c     korm          reade     fnorp01l
015700010406     c                   if        %eof(fnorp01l)
015800010406     c                   leave
015900010406     c                   endif
016000010406     c                   delete    fnorp000
016100010406     c                   ENDDO
016200010406
016300010406     c                   delete    fnorm000
016400010406     c                   endif
016500010411
016600040818     c  n02              movel     watb          ORMatb
016700040818     c  n02              Z-Add     *Date         OrmDtt
016800010411     c  n02              update    fnorm000
016900980721     c                   endif
017000980721
017100980721     c                   ENDIF
017200980721
017300980721     c                   EVAL      *INLR = *ON
017400980721
017500980721      *-------------------------------------------------------------------------*
017600980721      * Subroutine iniziale
017700980721      *-------------------------------------------------------------------------*
017800980721     c     *inzsr        BEGSR
017900980721      *
018000980721     C     *ENTRY        PLIST
018100980721     C                   PARM                    KPJBA
018200010406     c                   parm                    fior30ds
018300980721      *
018400010406     c                   IF        i30MOD = 'A'
018500980721     c                   movel     'A'           Watb
018600980721     c                   ENDIF
018700980721
018800010406     c                   IF        i30MOD = 'R'
018900980721     c                   movel     ' '           Watb
019000980721     c                   ENDIF
019100990820      *
019200010406     c                   IF        i30MOD = 'D'
019300990820     c                   seton                                        02
019400990820     c                   ENDIF
019500980721      *
019600010406     c                   movel     i30poe        kpoe
019700010406     c                   movel     i30nsr        knsr
019800010406     c                   movel     i30nor        knor
019900010406     c                   movel     i30nrv        knrv
020000980721      *
020100010406     C     Korm          KLIST
020200010406     C                   KFLD                    Kpoe
020300010406     C                   KFLD                    Knsr
020400010406     C                   KFLD                    Knor
020500010406     C                   KFLD                    Knrv
020600980721      *
020700980721     c                   endsr
020800980721      *-------------------------------------------------------------------------*
020900980721**  MSG lung. 78                                                             *
021000980721Modalità richiamo programma errata: telefonare in Sede !!!
