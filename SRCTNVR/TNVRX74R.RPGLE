000100140814     Fwftis77f  o  a e             disk
000200040330     D NSpediz         S             12
000300040330     D esito           S              1
000400140818     D indx            S              2  0
000500140818     D w015a           S             15
000600040330     D tis174dsi     E DS                  INZ
000700040330     D tis174dso     E DS                  INZ
000800140814     d rifero74_30            28     57
000900040330
001000040330     C     *ENTRY        PLIST
001100040330     C     NSPEDIZI74    PARM                    NSPEDIZ
001200040330
001300040330     c                   CALL      'TIS774R'
001400040330     c                   PARM      *BLANK        esito
001500040330     c                   PARM                    tis174dsi
001600040330     c                   PARM                    tis174dso
001700140814
001800140814     c                   if        rifero74_30<>*blanks
001900140814
002000140814     c                   clear                   dec2              2 0
002100140814     c                   clear                   wias              9 2
002200140814     c                   clear                   num1              1 0
002300140814     c                   clear                   num2              2 0
002400140814     c                   clear                   num3              3 0
002500140814     c                   if        %subst(iaso74:11:2)>=*zeros
002600140814     c                   move      iaso74        dec2
002700140814     c                   eval      wias=dec2*0,00
002800140814     c
002900140814     c                   if        %subst(iaso74:1:1)>*zeros
003000140814     c                   eval      num1=%int(%subst(iaso74:1:1))
003100140814     c                   eval      wias=wias + (num1*10000000)
003200140814     c                   endif
003300140814
003400140814     c                   select
003500140814     c                   when      %subst(iaso74:3:1)>*zeros
003600140814     c                   eval      num3=%int(%subst(iaso74:3:3))
003700140814     c                   eval      wias=wias + (num3*1000)
003800140814     c                   when      %subst(iaso74:4:1)>*zeros
003900140814     c                   eval      num2=%int(%subst(iaso74:4:2))
004000140814     c                   eval      wias=wias + (num2*1000)
004100140814     c                   when      %subst(iaso74:5:1)>*zeros
004200140814     c                   eval      num1=%int(%subst(iaso74:5:1))
004300140814     c                   eval      wias=wias + (num1*1000)
004400140814     c                   endsl
004500140814     c                   select
004600140814     c                   when      %subst(iaso74:7:1)>*zeros
004700140814     c                   eval      num3=%int(%subst(iaso74:7:3))
004800140819     c                   eval      wias=wias + (num3*1)
004900140814     c                   when      %subst(iaso74:8:1)>*zeros
005000140814     c                   eval      num2=%int(%subst(iaso74:8:2))
005100140819     c                   eval      wias=wias + (num2*1)
005200140814     c                   when      %subst(iaso74:9:1)>*zeros
005300140814     c                   eval      num1=%int(%subst(iaso74:9:1))
005400140819     c                   eval      wias=wias + (num1*1)
005500140814     c                   endsl
005600140814     c                   endif
005700140814     c
005800140814     c
005900140814      /free
006000140814            wfpgm='74' ;
006100140814           portoo74_1=portoo74 ;
006200140818           w015a    = %subst(rifero74:1:15)   ;
006300140818           indx=%checkr(' ':w015a)  ;
006400140818           clear rifero74n ;
006500140818           rifero74n= %int(%subst(w015a:1:indx)) ;
006600140818           //  rifero74n= %int(%xlate(' ':'0':(%subst(rifero74:1:15))));
006700140818
006800140814           rifero74a= %subst(rifero74:16:15)  ;
006900140814           iaso74_n=wias    ;
007000140814           DATO74_10=DATO74;
007100140814           ORAO74_10=ORAO74;
007200140814           OPEO74_10=OPEO74;
007300140814           EVEO74_10=EVEO74;
007400140814           NOTAO74_10=NOTAO74;
007500140814           write wftis7700  ;
007600140814           endif ;
007700140814      /end-free
007800040330
007900040330     C                   EVAL      *INLR = *ON
