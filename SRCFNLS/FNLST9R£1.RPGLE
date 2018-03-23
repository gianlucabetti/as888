000100110620     /*PRM dbgview(*source)
001000040107      *---------------------------------------------------------------*
001100111202      * Interrogazione spedizioni consegnate per linea di arrivo      *
001200040107      *---------------------------------------------------------------*
001300040107
001400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500090528     h dftactgrp(*no) actgrp(*caller)
001600040107
001700040107      *---------------------------------------------------------------*
001800040107
001900111202     ftabel00f  if   e           k disk
002000090922     fazorg01l  if   e           k disk
002001111206     ffnlrc2d   cf   E             workstn SFILE(LRC2S01:s01NRR)
002002111207     ffnlrc2p   o    e             printer oflind(*in90) usropn
002700040112      *
002800040107      *---------------------------------------------------------------*
003400040107      *
003500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040107      *
003700111202     d rec             S              2    DIM(100) inz
003702111202     d est             S              3  0 DIM(300) inz
003703111205     d err             s             79    dim(3) ctdata perrcd(1)
004700040107      *
004800040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004900040107      *
005000090922     d Tibs34ds      e ds                  inz
005100090922     d §azute        e ds                  Extname(Azute00f)
005200090922     d                                     dtaara
005300090922     d §Datiute      e ds                  EXTname(ddatiute)
005400090922     d                                     dtaara
005600111202     d ds3a          e ds
005601111205     d ds5a          e ds
005602111206     d og143         e ds
006100090828
006200040107     d KPJBA         e ds
008200040123      *
008900040109      *
009200090409
009300040107      *
009400040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009500040107      *
009600090409     D WrkStringaSql   S           4500
009700090409     D                                     VARYING
009701111205     D WrkFilIn        S           4000
009702111205     D                                     VARYING
009703111205      *
009704111206     D WrkcboIn        S            150
009705111205     D                                     VARYING
010700040213      * - Indici di schiera / Contatori
010800090827     d i               s              3  0 inz
011000040109      *
011100040109      * - Campi di comodo
011200111206     d wprimo          s              1
011202111205     d viddcminv       s                   like(viddcm)
011203111206     d conta           s              6  0
011204111206     d contatot        s              7  0
011205111206     d savlna          s                   like(orgfil)
011206111205     d dataiso         s               d   datfmt(*iso)
011207111205     d dataeur         s               d   datfmt(*eur)
011208111205     d dataisopul      s               d   datfmt(*iso)
011209111205     d datacur         s               d   datfmt(*iso)
011210111206     d datapul         s              8
011211111205     d flerr           s              1
011212111202      // - Campi associati al video
011213111206     d $Video          s              2    inz('D1')
011214111206     d s01nrr          s              4  0 inz
018500090828      *
018600090828      * - Flags booleani
018700090828     d $Finerec        s              1    inz(*off)
018701111202     d $Fine           s               n   inz(*off)
018702111207     d $End            s               n   inz(*off)
018703111202     d $InzD01         s               n   inz(*on)
018704111202     d $InzS01         s               n   inz(*on)
018705111205
018800081023
018900090402      // ? PROTOTIPI ?
019000090402      /copy gaitrasrc/srcprotopr,tibs34r
019001111206
019002111206     d TRUL49C         PR                  extpgm('TRUL49C')
019003111206     d  ppgm                         10    const
019004111206     d  pdat                          8
019005111206     d  perr                          1
019500040107
019600040107      *===============================================================*
019700040107      *  RIEPILOGO INDICATORI                                         *
019800040107      *---------------------------------------------------------------*
019900090827      *  XX    -
020000040107      *===============================================================*
020100040107
020200040107     c     *Entry        plist
020300040107     c                   parm                    KPJBA
020400090827     c
020600040123      *
020700090331      /free
020800090403         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
020801111202       //?Gestione video
020802111202       DOW $Fine = *off;
020803111202         SELECT;
020804111202           WHEN $Video = 'D1';
020805111202             exsr GesD01;
020806111202           WHEN $Video = 'S1';
020807111202             exsr GesS01;
020808111202           WHEN $Video = 'ST';
020809111202             exsr Stampa;
020810111202           OTHER;
020811111202             $Fine = *on;
020812111202         ENDSL;
020813111202       ENDDO;
020900090610
022300090922
026000090922
026100090827       eval *inlr=*on;
026101111206       //-------------------------------------------------------------*
026102111206       //                                                             *
026103111206       //-------------------------------------------------------------*
026104111206       Begsr Gesd01;
026105111206       // Inizializzazione videata
026106111206          if $inzd01;
026107111206              clear vidmsg;
026108111206              clear viddcm;
026109111206              *in02=*off;
026110111206              *in28=*off;
026111111206              $InzD01  = *off;
026112111206          endif;
026113111206             write lrc2z01;
026114111206             exfmt lrc2d01;
026115111206             clear vidmsg;
026116111206             *in28=*off;
026117111206             *in90=*off;
026118111206             select;
026119111206             // F3=Fine
026120111206                when *inkc;
026121111206                   $fine=*on;
026122111206                   leavesr;
026123111206                other;
026124111206                   exsr ctrd01;
026125111206                   if *in90;
026126111206                      leavesr;
026127111206                   endif;
026128111206                   if *inkf or *inkh;
026129111206                      $Video  = 'S1';
026130111206                      $InzS01 = *on;
026131111206                   endif;
026132111206             endsl;
026133111206       endsr;
026134111206       //-------------------------------------------------------------*
026135111206       // Controllo prima videata                                     *
026136111206       //-------------------------------------------------------------*
026137111206       Begsr ctrd01  ;
026138111206       *in40=*off;
026139120102       if %subst(%editc(viddcm:'X'):1:2)='00';
026140120102          test(DE) *dmy viddcm;
026141120102          if %error;
026142120102             *in40=*on;
026143120102             *in90=*on;
026144120102             *in28=*on;
026145120102             vidmsg=err(1);
026146120102             leavesr;
026147120102          else;
026148120102             dataiso=%date(viddcm:*dmy);
026149120102          endif;
026150120102       else;
026151120102          test(DE) *eur viddcm;
026152120102          if %error;
026153120102             *in40=*on;
026154120102             *in90=*on;
026155120102             *in28=*on;
026156120102             vidmsg=err(1);
026157120102             leavesr;
026158120102          else;
026159120102             dataiso=%date(viddcm:*eur);
026160120102          endif;
026161120102       endif;
026174111206          dataeur=dataiso;
026175111206          viddcm=%dec(dataeur);
026176111206          viddcminv=%dec(dataiso);
026177111206          clear datapul;
026178111206          clear flerr;
026179111206          trul49c('FNLR48R   ':datapul:flerr);
026180111206          if flerr='1' or datapul='00000000';
026181111206             dataisopul=datacur;
026182111206          else;
026183111206             dataisopul=%date(%dec(datapul:8:0):*iso);
026184111206          endif;
026185111206          dataiso=dataisopul-%days(§5aarb);
026186111206          if viddcminv<=%dec(dataiso);
026187111206             *in40=*on;
026188111206             *in90=*on;
026189111206             *in28=*on;
026190111206             vidmsg=err(2);
026191111206             leavesr;
026192111206          endif;
026193111206
026194111206       endsr;
026195111206       //--------------------------------------------------------------
026196111206       //?Gestione videata S01.
026197111206       //--------------------------------------------------------------
026198111206       BEGSR GesS01;
026199111206
026200111206       //?Inizializzazione videata
026201111206         IF  $InzS01;
026202111206           exsr InzS01;
026203111206           $InzS01  = *off;
026204111206         ENDIF;
026205111206
026206111206       //?Se richiamato per stampa non emetto il subfile
026207111206       //?ma vado nella routine di stampa
026208111206         IF  *inkh;
026209111206           $Video  = 'ST';
026210111206           leavesr;
026211111206         ENDIF;
026212111206
026213111206       //?Visualizzazione del SFL (se ci sono dati)
026214111207         *in20 = (S01nrr > *zeros);
026215111206         *in02 = *on;
026216111206
026217111206       //?Emissione Testata e Piede con tasti funzionali abilitati
026218111206         write  lrc2z02;
026219111206         write  lrc2d01;
026220111206
026221111206       //?Emissione videata
026222111206         exfmt  lrc2C01;
026223111206
026224111206         SELECT;
026225111206
026226111206       //?- F03=Fine
026227111206           WHEN  *inkc;
026228111206             $fine=*on;
026229111206
026230111206       //?- F08=Stampa
026231111206           WHEN  *inkh;
026232111206             exsr Stampa;
026233111206
026234111206       //?- F12=Ritorno
026235111206           WHEN  *inkl;
026236111206            $Video  = 'D1';
026237111206            $InzD01 = *on;
026238111206            leavesr;
026240111206
026241111206         ENDSL;
026242111206
026243111206       ENDSR;
026244111206       //--------------------------------------------------------------
026245111206       BEGSR InzS01;
026246111206
026247111206         *in21   = *off;
026248111206         *in23   = *off;
026249111206         write  LRC2C01;
026251111206         *in21   = *on;
026253111206
026254111206         clear V1Ctot;
026255111206         clear S01nrr;
026256111206
026257111206        // preparo stringa sql per estrazione codici clienti da elaborare
026258111206         exsr sr_prepSql;
026259111206       //?Caricamento subfile
026260111206         exsr Ries01;
026261111206
026262111206         *in23 = *on;
026263111206
026264111206       ENDSR;
026265111206
026266111206       //--------------------------------------------------------------
026267111206       //?Riempimento S01.
026268111206       //--------------------------------------------------------------
026269111206       BEGSR RieS01;
026270111206        $finerec=*off;
026271111206        clear  conta;
026272111206        clear  contatot;
026273111206        exec sql prepare STRINGASQL from :WrkStringaSql;
026274111206        exec sql declare ARBCsr cursor for StringaSql;
026275111206        exec sql open ARBcsr;
026276111206        dow $finerec=*off;
026277111206           exec sql Fetch ARBcsr into :savlna, :conta ;
026278111206           if sqlcod=100 or sqlcod<0;
026279111206              $finerec = *on;
026280111206              leave;
026281111206           endif;
026282111206           S01nrr += 1;
026283111206           v1slna=savlna;
026284111207           clear v1dlna;
026285111207           chain  v1slna azorg01l  ;
026286111207           if %found(azorg01l);
026287111207              v1dlna=orgdes;
026288111207           endif;
026289111206           v1stos=conta;
026290111206           contatot+=conta;
026291111206           write  lrc2s01;
026292111206        enddo;
026293111207        exec sql close ARBcsr;
026294111206        v1ctot=contatot;
026295111206
026296111206       ENDSR ;
026297111206       //-------------------------------------------------------------*
026298111206       //Preparazione stringa sql per Estrazione clienti da elaborare *
026299111206       //-------------------------------------------------------------*
026300111206       Begsr SR_prepSql;
026301111206
026302111214       if %subst(knsif:7:3)='201';
026303111214         WrkStringaSql='select arblna, count(*) from filtra201/fnarb00f +
026304111214                         where arbdcm=';
026305111214       else;
026306111214         WrkStringaSql='select arblna, count(*) from filtraprd/fnarb00f +
026307111214                         where arbdcm=';
026308111214       endif;
026309111206       WrkStringaSql=WrkStringaSql+%editc(viddcminv:'X');
026310111214       WrkStringaSql=WrkStringaSql+' and arbcca=''  ''';
026311111206       exsr sr_whrfil;
026312111206       WrkStringaSql=WrkStringaSql+WrkFilIn;
026313111206
026314111206       exsr sr_whrcbo;
026315111206       WrkStringaSql=WrkStringaSql+WrkCboIn;
026316111206       WrkStringaSql=WrkStringaSql+
026317111206       ' group by arblna order by arblna for read only';
026318111206
026319111206       endsr;
026320111206       //-------------------------------------------------------------*
026321111206       //Schiera filiali di appartenenza all'area del comm.le unific. *
026322111206       //-------------------------------------------------------------*
026323111206       Begsr SR_whrfil;
026324111206          wprimo='S';
026325111206
026326111206          reset WrkFilIn;
026327111206          WrkFilIn= ' And arblna not in (';
026328111206          for I=1 to %elem(est);
026329111206             if est(i)=0;
026330111206                leave;
026331111206             endif;
026332111206               if wprimo<>'S';
026333111206                 WrkFilIn=WrkFilIn + ' , ';
026334111206               endif;
026335111206             WrkFilIn=WrkFilIn + %char(est(i));
026336111206             Wprimo=' '    ;
026337111206          endfor;
026338111206          WrkFilIn=WrkFilIn + ')';
026339111206       endsr;
026340111206       //-------------------------------------------------------------*
026341111206       //Schiera codici bolla da scartare                             *
026342111206       //-------------------------------------------------------------*
026343111206       Begsr SR_whrcbo;
026344111206       wprimo='S';
026345111206       reset WrkCboIn;
026346111206       WrkCboIn= ' And arbcbo not in (';
026347111206          for I=1 to %elem(rec);
026348111206             if rec(i)<>*blanks;
026349111206                if wprimo<>'S';
026350111206                   WrkcboIn=WrkcboIn + ', ';
026351111206                endif;
026352111206                WrkCboIn=WrkCboIn + '''' + rec(i) + '''';
026353111206                wprimo=' ';
026354111206             endif;
026355111206          endfor;
026356111206       WrkCboIn=WrkCboIn + ')';
026357111206       endsr;
026358111206       //-------------------------------------------------------------*
026359111206       //Stampa                                                       *
026360111206       //-------------------------------------------------------------*
026361111206       Begsr Stampa;
026362111207
026363111207       //?Apro il file di stampa
026364111207         open fnlrc2p;
026365111207
026366111207         clear S01nrr;
026367111207         $End = *off;
026368111207       //?Stampo la testata
026369111207         write LRC2T00;
026370111207         *in90=*off;
026371111207
026372111207       //?Leggo il subfile caricato
026373111207         DOW not $End;
026374111207           S01nrr += 1;
026375111207           chain S01nrr lrc2S01;
026376111207           IF  not %found;
026377111207             $End = *on;
026378111207             leave;
026379111207           ENDIF;
026390111207
026391111207           if *in90;
026392111207              write LRC2T00;
026393111207              *in90=*off;
026394111207           endif;
026395111207           write lrc2r01;
026396111207
026397111207         ENDDO;
026398111207
026399111207       //Stampo il totale generale
026400111207          if v1ctot>0;
026401111207             if *in90;
026402111207                write LRC2T00;
026403111207                *in90=*off;
026404111207             endif;
026405111207             write lrc2tot;
026406111207          endif;
026407111207
026408111207         write Fines;
026409111207
026410111207         close fnlrc2p;
026411111207         $Fine = *on;
026412111206       endsr;
026413040108
026414090402       //-------------------------------------------------------------*
026415090402       //Routine Iniziale                                             *
026500090402       //-------------------------------------------------------------*
026600090414       begsr *inzsr;
026700090402
026800090402       //Reperisco dati job
026900090917         exsr datijob;
028300111202       // Leggo tabella "TB" per caricare schiera dei codici bolla da scartare
028301111202       // (recuperi e conto servizi)
028500090917       i=0;
028600111202       setll (1:'3A') tabel00f  ;
028700111202       reade (1:'3A') tabel00f  ;
028800090917
028900090917  1    dow    not %eof(tabel00f)  ;
029000111202          ds3a=tbluni             ;
029100111202          if §3arbl='R' or §3arbl='C';
029200090917             i=i+1;
029300111202             rec(i)=tblkey;
029400090917          endif;
029500111202          reade (1:'3A') tabel00f  ;
029600090917  1    enddo     ;
029601111202       // Carico schiera delle filiali estere
029602111202       i=0;
029603111202       setll *loval azorg01l  ;
029604111202       read         azorg01l  ;
029605111202
029606111202  1    dow    not %eof(azorg01l)  ;
029609111202             og143=orgde3;
029610111202             if §ogntw='DPD' or §ogntw='FED' or §ogntw='EEX';
029611111202                i=i+1;
029612111202                est(i)=orgfil;
029613111202             endif;
029615111202          read      azorg01l  ;
029616111202  1    enddo     ;
029617111205       // Rperisco giorni di pulizia bolle arrivi
029618111205       clear ds5a;
029619111205       chain (1:'5A':'1       ') tabel00f  ;
029620111205       if %found(tabel00f);
029621111205          ds5a=tbluni;
029622111205       endif;
030300090402
030400090402       EndSr;
030500040108
034400090402       //-------------------------------------------------------------*
034500090402       //Reperimento Dati del job (Utente/Operativi)                  *
034600090402       //-------------------------------------------------------------*
034700090918       Begsr DatiJob;
034800090403
034900090917          in(e) §azute;
035000090917          if not %error;
035100090917           in(e) §datiute;
035200090917          endif;
035300090917          if %error or rsut = *blanks;
035400090917           tibs34r(tibs34ds);
035500090917           in §azute;
035600090917           in §datiute;
035700090917          endif;
035701111205
035702111205       // Reperisco la data corrente
035703111205          datacur=(%date());
035705111207          wdate=%dec(datacur :*eur);
035706111207          utime= %dec(%time());
036000090917         endsr;
036100040108
155300111205**
155500111205Data errata o mancante                                                        01
155600111205Data non elaborabile per mancanza di dati                                     02
155700111205
