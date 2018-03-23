000100121218      //---------------------------------------------------------------
000200121218      //
000300150903      //?        Controllo codice tassazione
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100140613     fAZCPC01L  if   e           k disk
001101150903     ffnblp31l  uf   e           k disk
001102150903     ffnarb01l  uf   e           k disk
001103150903     ftitas30c  uf   e           k disk    extfile(wtitas) usropn
001104150903     f                                     ignore(titas010)
001105150903     f                                     ignore(titasp00)
001106150903     fwfcts00f  o  a e             disk
001500040119
001600040119      *---------------------------------------------------------------*
001700121218
001800121218       // - Campi di comodo
001900150903     d k_ver           s                   like(CPCver)  inz(46)
001901150903     d Wtitas          s             21    inz('GAITRAGRU /TITAS30C')
002300140613
002900140613
003000140613      // - Campi di comodo data
003100140613     d wData_EUR       s               d   datfmt(*eur)
003200140613     d wData_ISO       s               d   datfmt(*iso)
003300140613     d wOggi           s              8  0 inz
003400121218
003500121218       // - flag booleani
003900140613
004000140613       //--------------------------------------------------------------
004100140613       //?Definizione strutture dati.                                  ?
004200140613       //--------------------------------------------------------------
004300140613     d KPJBA         e ds
004400140616
004600040119
004700140613       //--------------------------------------------------------------
004800140613       //?M A I N - L I N E
004900140613       //---------------------------------------------------------------
005000140613
005100140613     c     *entry        plist
005200140613     c                   parm                    KPJBA
005300040119
005301150903     c                   Open(e)   titas30c
005302150903     c                   if        not %open(titas30c)
005303150903     c                   eval      %subst(Wtitas:7:4)='GRPS'
005304150903     c                   Open      titas30c
005305150903     c                   endif
005400121218      /free
005500140613
005800140613
005900140613       // Imposto data e ora sistema
006000140613         wOggi     = %dec(%date());
006100140613         wData_ISO = %date(wOggi:*ISO);
006200140613         wData_EUR = wData_ISO;
006300150903       //DataEla   = %dec(wData_EUR);
006400150903       //OraEla    = %dec(%time());
006500140613
006900140616
006901150903       // Leggo FNBLP31L anno 2015 e lnp 028 e mgs>0808
006902150903         setll (028:2015:0827) fnblp31l;
006903150903         if %subst(kpjbu:1:1)='A';
006904150903            reade (028:2015)      fnblp31l;
006905150903         else;
006906150903            reade(N) (028:2015)      fnblp31l;
006907150903         endif;
006908150903         dow not %eof(fnblp31l);
006909150903       //   non elaboro se bolla per estero
006910150903            if blpnzd=*blanks;
006911150903               chain (k_ver:blpnzd:blpcad) azcpc01l;
006912150903               if %found(azcpc01l);
006913150903       // Codice tassazione <> : aggiorno se vuoto su bolla
006914150903       // in ogni caso stampo la differenza
006915150903                  if blpcts<>cpccts;
006916150903                     ANNO   = blpaas;
006917150903                     LNP    = blplnp;
006918150903                     SERIE  = blpnrs;
006919150903                     NSP    = blpnsp;
006920150903                     CTSERR = blpcts;
006921150903                     CTSOK  = cpccts;
006922150903                     NOTE   = 'BLP ' ;
006923150903                     write wfcts000;
006924150903       // AGGIORNAMENTO ARCHIVI BOLLE
006925150903                     if blpcts=*blanks and %subst(kpjbu:1:1)='A';
006926150903       // Aggiorno BLP
006927150903                        blpcts=cpccts;
006928150903                        update fnblp000;
006929150903       // Aggiorno ARB
006930150903                        chain (blpaas:blplnp:blpnrs:blpnsp) fnarb01l;
006931150903                        if %found(fnarb01l) and arbcts=*blanks;
006932150903                           ANNO   = arbaas;
006933150903                           LNP    = arblnp;
006934150903                           SERIE  = arbnrs;
006935150903                           NSP    = arbnsp;
006936150903                           CTSERR = arbcts;
006937150903                           CTSOK  = cpccts;
006938150903                           NOTE   = 'ARB ' ;
006939150903                           write wfcts000;
006940150903                           arbcts=cpccts;
006941150903                           update fnarb000;
006942150903                        endif;
006943150903       // Aggiorno TAS
006944150903                        setll (blpaas:blplnp:blpnrs:blpnsp) titas30c;
006945150903                        reade (blpaas:blplnp:blpnrs:blpnsp) titas30c;
006946150903                        dow not %eof(titas30c);
006947150903                           if tascts=*blanks;
006948150903                              ANNO   = tasaas;
006949150903                              LNP    = taslnp;
006950150903                              SERIE  = tasnrs;
006951150903                              NSP    = tasnsp;
006952150903                              CTSERR = tascts;
006953150903                              CTSOK  = cpccts;
006954150903                              NOTE   = 'TAS ' ;
006955150903                              write wfcts000;
006956150903                              tascts=cpccts;
006957150903                              update titas000;
006958150903                           endif;
006959150903                        reade (blpaas:blplnp:blpnrs:blpnsp) titas30c;
006960150903                        enddo;
006961150903                     endif;
006962150903                  endif;
006963150903               else;
006964150903                     ANNO   = blpaas;
006965150903                     LNP    = blplnp;
006966150903                     SERIE  = blpnrs;
006967150903                     NSP    = blpnsp;
006968150903                     CTSERR = blpcts;
006969150903                     CTSOK  = *blanks;
006970150903                     NOTE   = 'Non trovato cts su cappario' ;
006971150903                     write wfcts000;
006972150903               endif;
006973150903            endif;
006974150903            if %subst(kpjbu:1:1)='A';
006975150903            reade (028:2015)      fnblp31l;
006976150903            else;
006977150903            reade(n) (028:2015)      fnblp31l;
006978150903            endif;
006979150903         enddo;
007300140616
007301150903           if %open(titas30c);
007302150903              close titas30c;
007303150903           endif;
007900140613
008200121218
008300140613         *inLR = *ON;
008400140616
029800121218      /end-free
029900140331
