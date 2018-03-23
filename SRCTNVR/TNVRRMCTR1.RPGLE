000100121218      //---------------------------------------------------------------
000200121218      //
000300150903      //?        Controllo Flag inoltro
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100150903     fAZCPc01L  if   e           k disk
001101150903     f*fiar401l  if   e           k disk
001102150903     ffnblp31l  uf   e           k disk
001103150903     ffnarb01l  uf   e           k disk
001104150903     ftitas30c  uf   e           k disk    extfile(wtitas) usropn
001105150903     f                                     ignore(titas010)
001106150903     f                                     ignore(titasp00)
001107150903     fwfcts10f  o  a e             disk
001500040119
001600040119      *---------------------------------------------------------------*
001700121218
001800121218       // - Campi di comodo
001900150903     d k_ver           s                   like(CPcver)  inz(46)
001901150903     d*wlod            s                   like(CPlloc)
001902150903     d Wtitas          s             21    inz('GAITRAGRU /TITAS30C')
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
006911150903       //      wlod=blplod;
006912150903       //      chain (blpaas:blplnp:blpnrs:blpnsp:'X') fiar401l;
006913150903       //      if %found(fiar401l);
006914150903       //         wlod=ar4not;
006915150903       //      endif;
006916150903       //      chain (k_ver:blpnzd:blpcad:wlod) azcpl01l;
006917150903               chain (k_ver:blpnzd:blpcad) azcpc01l;
006918150903               if %found(azcpc01l);
006919150903       // Codice tassazione <> : aggiorno se vuoto su bolla
006920150903       // in ogni caso stampo la differenza
006921150903                  if blpfin<>cpciso;
006922150903                     ANNO   = blpaas;
006923150903                     LNP    = blplnp;
006924150903                     SERIE  = blpnrs;
006925150903                     NSP    = blpnsp;
006926150903                     finERR = blpfin;
006927150903                     finOK  = cpciso;
006928150903                     NOTE   = 'BLP ' ;
006929150903                     write wfcts000;
006930150903       // AGGIORNAMENTO ARCHIVI BOLLE
006931150903                     if blpfin=*blanks and %subst(kpjbu:1:1)='A';
006932150903       // Aggiorno BLP
006933150903                        blpfin=cpciso;
006934150903                        update fnblp000;
006935150903       // Aggiorno ARB
006936150903                        chain (blpaas:blplnp:blpnrs:blpnsp) fnarb01l;
006937150903                        if %found(fnarb01l) and arbfin=*blanks;
006938150903                           ANNO   = arbaas;
006939150903                           LNP    = arblnp;
006940150903                           SERIE  = arbnrs;
006941150903                           NSP    = arbnsp;
006942150903                           finERR = arbfin;
006943150903                           finOK  = cpciso;
006944150903                           NOTE   = 'ARB ' ;
006945150903                           write wfcts000;
006946150903                           arbfin=cpciso;
006947150903                           update fnarb000;
006948150903                        endif;
006949150903       // Aggiorno TAS
006950150903                        setll (blpaas:blplnp:blpnrs:blpnsp) titas30c;
006951150903                        reade (blpaas:blplnp:blpnrs:blpnsp) titas30c;
006952150903                        dow not %eof(titas30c);
006953150903                           if tasfin<>cpciso;
006954150903                              ANNO   = tasaas;
006955150903                              LNP    = taslnp;
006956150903                              SERIE  = tasnrs;
006957150903                              NSP    = tasnsp;
006958150903                              finERR = tasfin;
006959150903                              finOK  = cpciso;
006960150903                              NOTE   = 'TAS ' ;
006961150903                              write wfcts000;
006962150903                              tasfin=cpciso;
006963150903                              update titas000;
006964150903                           endif;
006965150903                        reade (blpaas:blplnp:blpnrs:blpnsp) titas30c;
006966150903                        enddo;
006967150903                     endif;
006968150903                  endif;
006969150903               else;
006970150903                     ANNO   = blpaas;
006971150903                     LNP    = blplnp;
006972150903                     SERIE  = blpnrs;
006973150903                     NSP    = blpnsp;
006974150903                     finERR = blpfin;
006975150903                     finOK  = *blanks;
006976150903                     NOTE   = 'Non trovato cappario' ;
006977150903                     write wfcts000;
006978150903               endif;
006979150903            endif;
006980150903            if %subst(kpjbu:1:1)='A';
006981150903            reade (028:2015)      fnblp31l;
006982150903            else;
006983150903            reade(n) (028:2015)      fnblp31l;
006984150903            endif;
006985150903         enddo;
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
