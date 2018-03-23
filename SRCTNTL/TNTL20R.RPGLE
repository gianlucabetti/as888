000101081104     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081104     F*--------------------------------------------------------------------
000300090112     F*   Aggiornamento kilometri su anagrafica traini (fermate)
000900081104     F*--------------------------------------------------------------------
000901090112     Ftntlt01l  IF   E           K DISK
001000090112     Ftntlr01l  UF   E           K DISK
001001090112     Ftabel00f  IF   E           K DISK
001002090113     Fazorg01l  IF   E           K DISK
001003090113     Ftntl20p   O    E             PRINTER OFLIND(*IN80)
001100081104     F*--------------------------------------------------------------------
001200920217     D*
001205090112     d wfilp           s                   like(tlrfil)
001206090112     d x               s              4  0
001207090113     d w_errore        s              1
001208090113     d dataiso         s               d   datfmt(*iso)
001210090112     d*tipi traino non pagati dalla sede
001211090112     d tvn             S                   like(tltttr) dim(30)
001400081104     F*--------------------------------------------------------------------
001401090113     D kpjba         E DS
001402090113     D ficna2ds      E DS
001403090112     D dstv          E DS
001404090113     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
001405090113     d DDatiUte      e ds                  dtaara(§datiute)
001406090113     d tibs34ds      e ds                  inz
001407090113      *------------------------------------------------------------------------*
001408090113      *   P R O T O T I P I
001409090113      *------------------------------------------------------------------------*
001410090113     d ficna2r1        pr                  extpgm('FICNA2R1')
001411090113     d  ficna2ds                           likeds(FICNA2DS)
001412090113      /copy gaitrasrc/srcprotopr,tibs34r
001413090113     D
005000920217     C*----------------------------------------------------*
005100880202     C     *ENTRY        PLIST
005200090112     C                   PARM                    kpjba
005300940913     C*
005302081104
005313081104      /free
005314090112       setll *loval TNTLT01L;
005315090112       read  TNTLT01L;
005316090112       dow not %eof(TNTLT01L);
005317090113       // scarto se traino scaduto prima del 01/01/2009
005321090113       // scarto se tipo traino non pagato da sede
005322090116          if tltdsc>=20080101 and %lookup(tltttr:tvn)=0;
005324090112             exsr sr_elabora;
005325090113             if w_errore='1';
005326090113                if *in80;
005327090113                   write tl20te;
005328090113                   write tl20ri;
005329090113                   *in80='0';
005330090113                endif;
005331090113                exsr riedet;
005342090113                write tl20de;
005343090113             endif;
005344090112          endif;
005345090112          read  TNTLT01L;
005346090112       enddo;
005347090113       write tl20fi;
005348090112       *INLR = *ON;
005349090112       //-----------------------------------------------------------------
005350090112       // Aggiornamento totale kilometri per tratta
005351090112       //-----------------------------------------------------------------
005352090112       begsr sr_elabora;
005353090113          w_errore='0';
005354090112          // Ciclo di lettura delle fermate del traino per traino/data/numero fermata
005355090112          setll (tlttrn: tltdde) TNTLR01L;
005356090112          reade(N) (tlttrn: tltdde) TNTLR01L;
005357090112          // Prima fermata: memorizzo semplicemente la filiale
005358090112          wfilp=tlrfil;
005359090112          reade (tlttrn: tltdde) TNTLR01L;
005360090112          dow not %eof(TNTLR01L);
005361090112             clear ficna2ds;
005362090112             ia2lnp=wfilp;
005363090112             ia2lna=tlrfil;
005364090112             callp ficna2r1(ficna2ds);
005365090112             if oa2error<>'1';
005366090112                   tlrkmt=oa2km;
005367090113                   clear tlrftr;
005368090112                   update tntlr000;
005369090112             else;
005370090113                unlock tntlr01L;
005371090113                w_errore='1';
005372090112             endif;
005373090112             wfilp=tlrfil;
005374090112             reade (tlttrn: tltdde) TNTLR01L;
005375090112          enddo;
005376090112       ENDSR;
005377090113       //-----------------------------------------------------------------
005378090113       // Riempimento campi di stampa
005379090113       //-----------------------------------------------------------------
005380090113       begsr riedet;
005381090113       pdetrn=tlttrn;
005382090113       chain (tlttfp) azorg01l;
005383090113       if %found(azorg01l);
005384090113         pdefpd=orgdes;
005385090113       endif;
005386090113       chain (tlttfa) azorg01l;
005387090113       if %found(azorg01l);
005388090113         pdefad=orgdes;
005389090113       endif;
005390090113       dataiso=%date(tltdde:*iso);
005391090113       pdedde=%dec(dataiso:*eur);
005392090113       dataiso=%date(tltdsc:*iso);
005394090113       pdedsc=%dec(dataiso:*eur);
005395090113       ENDSR;
005396090112
005397090113       //-----------------------------------------------------------------
005398090113       // Routine iniziale
005399090113       //-----------------------------------------------------------------
005400090112       begsr *inzsr;
005401090113
005402090113       in(E) *dtaara   ;
005403090113       if    %error or rsut  =*blanks  ;
005404090113       callp TIBS34R (TIBS34DS)        ;
005405090113       in    *dtaara   ;
005406090113       endif                           ;
005407090113
005408090113       // Caricamento tabell tipi traino non pagati da sede
005409090112       x=0;
005410090112       clear tvn;
005411090112       setll (1: 'TV') tabel00f;
005412090112       reade (1: 'TV') tabel00f;
005413090112       dow not %eof(tabel00f);
005414090112          dstv=tbluni;
005415090113          if §tvutc<>'S';
005416090112             x=x+1;
005417090112             tvn(x)=tblkey;
005418090112          endif;
005419090112          reade (1: 'TV') tabel00f;
005420090112       enddo;
005421090113       // Stampa intestazione
005422090113       write tl20te;
005423090113       write tl20ri;
005424090113       *in80='0';
005425090112       ENDSR;
