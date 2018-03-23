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
002600170710     D fior3Ads      E DS
002700170710
002800170710      // Se Tipo lancio 'C' --> solo chiusura file
002900170710       IF  I30tla = 'C';
003000170710         *inLR = *on;
003100170710         return;
003200170710       ENDIF;
003300980721
003400010406     c                   IF        i30mod <> 'A'  and  i30mod <> 'R'
003500010406     c                                            and  i30mod <> 'D'
003600010406     c                   movel     'E'           o30err
003700010406     c                   movel     MSG(1)        o30msg
003800980721     C                   ELSE
003900170724     c                   exsr      routinz
004000010411
004100010411      * FNORN
004200010411     c     korm          setll     fnorn01l
004300010411     c                   DO        *hival
004400010411     c     korm          reade     fnorn01l
004500010411     c                   if        %eof(fnorn01l)
004600010411     c                   leave
004700010411     c                   endif
004800040818     c  n02              movel     watb          ORNatb
004900040818     c  n02              Z-Add     *Date         OrnDtr
005000010411     c  n02              update    fnorn000
005100010411     c   02              delete    fnorn000
005200010411     c                   ENDDO
005300010411
005400010411      * FNORF
005500010411     c     korm          setll     fnorf01l
005600010411     c                   DO        *hival
005700010411     c     korm          reade     fnorf01l
005800010411     c                   if        %eof(fnorf01l)
005900010411     c                   leave
006000010411     c                   endif
006100040818     c  n02              movel     watb          ORFatb
006200010411     c  n02              update    fnorf000
006300010411     c   02              delete    fnorf000
006400010411     c                   ENDDO
006500070705
006600070705      * FNORG
006700070705     c     korm          setll     fnorg01l
006800070705     c                   DO        *hival
006900070705     c     korm          reade     fnorg01l
007000070705     c                   if        %eof(fnorg01l)
007100070705     c                   leave
007200070705     c                   endif
007300070705     c  n02              movel     watb          ORgatb
007400070705     c  n02              update    fnorg000
007500070705     c   02              delete    fnorg000
007600070705     c                   ENDDO
007700100218
007800100218      * FNORA
007900100218     c     korm          setll     fnora01l
008000100218     c                   DO        *hival
008100100218     c     korm          reade     fnora01l
008200100218     c                   if        %eof(fnora01l)
008300100218     c                   leave
008400100218     c                   endif
008500100218     c   02              delete    fnora000
008600100218     c                   ENDDO
008700100218
008800100218      * FNORE
008900100218     c     korm          setll     fnore01l
009000100218     c                   DO        *hival
009100100218     c     korm          reade     fnore01l
009200100218     c                   if        %eof(fnore01l)
009300100218     c                   leave
009400100218     c                   endif
009500100218     c   02              delete    fnore000
009600100218     c                   ENDDO
009700150702
009800150702      * FNORT
009900150702     c     korm          setll     fnort01l
010000150702     c                   DO        *hival
010100150702     c     korm          reade     fnort01l
010200150702     c                   if        %eof(fnort01l)
010300150702     c                   leave
010400150702     c                   endif
010500150702     c   02              delete    fnort000
010600150702     c                   ENDDO
010700131030
010800131030      * FNORPE
010900131030     c     korm          setll     fnorpe1l
011000131030     c                   DO        *hival
011100131030     c     korm          reade     fnorpe1l
011200131030     c                   if        %eof(fnorpe1l)
011300131030     c                   leave
011400131030     c                   endif
011500131030     c   02              delete    fnorpe00
011600131030     c                   ENDDO
011700131030
011800131030      * FNORVE
011900131030     c     korm          setll     fnorve1l
012000131030     c                   DO        *hival
012100131030     c     korm          reade     fnorve1l
012200131030     c                   if        %eof(fnorve1l)
012300131030     c                   leave
012400131030     c                   endif
012500131030     c   02              delete    fnorve00
012600131030     c                   ENDDO
012700100811
012800100811      * FIDPO
012900100811     c     korm          chain     fidpo01l
013000100811     c                   if        %found(fidpo01l)
013100100811     c  n02              eval      DPOann = watb
013200100811     c  n02              update    fidpo000
013300100811     c   02              delete    fidpo000
013400100811     c                   ENDIF
013500141110
013600141110      * FISAO
013700141110     c     korm          setll     fisao01l
013800141110     c                   DO        *hival
013900141110     c     korm          reade     fisao01l
014000141110     c                   IF        %eof(fisao01l)
014100141110     c                   leave
014200141110     c                   ENDIF
014300141110     c                   delete    fisao000
014400141110     c                   ENDDO
014500980721
014600010406      * FNORM
014700010406     c     korm          chain     fnorm01l
014800010406     c                   if        %found(fnorm01l)
014900010406
015000010406     C                   if        *in02 = *on
015100010406      * FNORV
015200010406     c     korm          setll     fnorv01l
015300010406     c                   DO        *hival
015400010406     c     korm          reade     fnorv01l
015500010406     c                   if        %eof(fnorv01l)
015600010406     c                   leave
015700010406     c                   endif
015800010406     c                   delete    fnorv000
015900010406     c                   ENDDO
016000010406      * FNORP
016100010406     c     korm          setll     fnorp01l
016200010406     c                   DO        *hival
016300010406     c     korm          reade     fnorp01l
016400010406     c                   if        %eof(fnorp01l)
016500010406     c                   leave
016600010406     c                   endif
016700010406     c                   delete    fnorp000
016800010406     c                   ENDDO
016900010406
017000010406     c                   delete    fnorm000
017100010406     c                   endif
017200010411
017300040818     c  n02              movel     watb          ORMatb
017400040818     c  n02              Z-Add     *Date         OrmDtt
017500010411     c  n02              update    fnorm000
017600980721     c                   endif
017700980721
017800980721     c                   ENDIF
017900980721
018000170710       IF  I30tla <> *blanks;
018100980721     c                   EVAL      *INLR = *ON
018200170710       ELSE;
018300170710         *inRT = *on;
018400170710       ENDIF;
018500980721
018600980721      *-------------------------------------------------------------------------*
018700980721      * Subroutine iniziale
018800980721      *-------------------------------------------------------------------------*
018900980721     c     *inzsr        BEGSR
019000980721      *
019100980721     C     *ENTRY        PLIST
019200980721     C                   PARM                    KPJBA
019300170710     c                   parm                    fior3Ads
019400170724      *
019500170724     C     Korm          KLIST
019600170724     C                   KFLD                    Kpoe
019700170724     C                   KFLD                    Knsr
019800170724     C                   KFLD                    Knor
019900170724     C                   KFLD                    Knrv
020000170724      *
020100170724     c                   endsr
020200170724
020300170724      *-------------------------------------------------------------------------*
020400170724      * routine per impostazione dati
020500170724      *-------------------------------------------------------------------------*
020600170724     c     routinz       BEGSR
020700980721      *
020800010406     c                   IF        i30MOD = 'A'
020900980721     c                   movel     'A'           Watb
021000980721     c                   ENDIF
021100980721
021200010406     c                   IF        i30MOD = 'R'
021300980721     c                   movel     ' '           Watb
021400980721     c                   ENDIF
021500990820      *
021600010406     c                   IF        i30MOD = 'D'
021700990820     c                   seton                                        02
021800990820     c                   ENDIF
021900980721      *
022000010406     c                   movel     i30poe        kpoe
022100010406     c                   movel     i30nsr        knsr
022200010406     c                   movel     i30nor        knor
022300010406     c                   movel     i30nrv        knrv
022400980721      *
022500980721     c                   endsr
022600980721      *-------------------------------------------------------------------------*
022700980721**  MSG lung. 78                                                             *
022800980721Modalità richiamo programma errata: telefonare in Sede !!!
