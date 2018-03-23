000101081104     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081104     F*--------------------------------------------------------------------
000300090112     F*   Calcolo Km totali di un traino
000900081104     F*--------------------------------------------------------------------
000901090112     Ftntlr01l  IF   E           K DISK
001100081104     F*--------------------------------------------------------------------
001200920217     D*
001400081104     F*--------------------------------------------------------------------
001401090112     D fnlvt1ds      E DS
005000920217     C*----------------------------------------------------*
005100880202     C     *ENTRY        PLIST
005200090112     C                   PARM                    fnlvt1ds
005300940913     C*
005302081104
005313081104      /free
005314090112       setll *loval TNTLT01L;
005315090112       read  TNTLT01L;
005316090112       dow not %eof(TNTLT01L);
005318090112          if %lookup(tltttr:tvn)=0;
005319090112             exsr sr_elabora;
005320090112          endif;
005321090112          read  TNTLT01L;
005322090112       enddo;
005323090112       *INLR = *ON;
005324090112       //-----------------------------------------------------------------
005325090112       // Aggiornamento totale kilometri per tratta
005326090112       //-----------------------------------------------------------------
005327090112       begsr sr_elabora;
005328090112          // Ciclo di lettura delle fermate del traino per traino/data/numero fe
005329090112          setll (tlttrn: tltdde) TNTLR01L;
005330090112          reade(N) (tlttrn: tltdde) TNTLR01L;
005331090112          // Prima fermata: memorizzo semplicemente la filiale
005332090112          wfilp=tlrfil;
005333090112          reade (tlttrn: tltdde) TNTLR01L;
005334090112          dow not %eof(TNTLR01L);
005335090112             clear ficna2ds;
005336090112             ia2lnp=wfilp;
005337090112             ia2lna=tlrfil;
005338090112             callp ficna2r1(ficna2ds);
005339090112             if oa2error<>'1';
005340090112                if tlrkmt=0;
005341090112                   tlrkmt=oa2km;
005342090112                   update tntlr000;
005343090112                else;
005344090112                   unlock tntlr000;
005345090112                endif;
005346090112             else;
005347090112                unlock tntlr000;
005348090112             endif;
005349090112             wfilp=tlrfil;
005350090112             reade (tlttrn: tltdde) TNTLR01L;
005351090112          enddo;
005352090112       ENDSR;
005353090112
005354090112       begsr *inzsr;
005355090112       x=0;
005356090112       clear tvn;
005357090112       setll (1: 'TV') tabel00f;
005358090112       reade (1: 'TV') tabel00f;
005359090112       dow not %eof(tabel00f);
005360090112          dstv=tbluni;
005361090112          if §tvutc='S';
005362090112             x=x+1;
005363090112             tvn(x)=tblkey;
005364090112          endif;
005365090112          reade (1: 'TV') tabel00f;
005366090112       enddo;
005367090112       ENDSR;
