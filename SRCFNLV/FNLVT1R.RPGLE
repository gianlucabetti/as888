000101081104     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081104     F*--------------------------------------------------------------------
000300090112     F*   Calcolo Km totali di un traino
000900081104     F*--------------------------------------------------------------------
000901090112     Ftntlr01l  IF   E           K DISK
001100081104     F*--------------------------------------------------------------------
001101090113     d w_totkm         s                   like(ot1km)
001200920217     D*
001400081104     F*--------------------------------------------------------------------
001401090112     D fnlvt1ds      E DS
005000920217     C*----------------------------------------------------*
005100880202     C     *ENTRY        PLIST
005200090112     C                   PARM                    fnlvt1ds
005300940913     C*
005302081104
005313081104      /free
005314090113       clear ot1km;
005315090113       clear ot1err;
005316090113       clear ot1msg;
005317090113       // Tipo lancio, I95tla = "C" -           chiudo con LR
005318090113       // Tipo lancio, I95tla = "L" - elaboro e chiudo con LR
005319090113       // Tipo lancio, I95tla = " " - elaboro e chiudo con return
005320090113
005321090113       if it1tla<>'C';
005322090113          test(DE) *iso it1dde;
005323090113          if %error;
005324090113             eval ot1err='1';
005325090113             eval ot1msg='Data errata';
005326090113          else;
005327090114             if it1trn=0;
005328090114                eval ot1err='2';
005329090114                eval ot1msg='Numero traino mancante';
005330090114             else;
005331090114                setll (it1trn: it1dde) tntlr01l;
005332090114                reade (it1trn: it1dde) tntlr01l;
005333090114                dow not %eof(TNTLR01L);
005334090114                   if tlrkmt>0;
005335090114                      w_totkm+=tlrkmt;
005336090114                   endif;
005337090114                   reade (it1trn: it1dde) tntlr01l;
005338090114                enddo;
005339090114                ot1km=w_totkm;
005340090114             endif;
005341090114          endif;
005342090113       endif;
005343090113
005344090113       if it1tla<>' ';
005345090113          *INLR = *ON;
005346090113       else;
005347090113          return;
005348090113       endif;
005354090112
