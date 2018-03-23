000100131227     H*------------------------------------------------------------------------*
000200151027     H* Creazione/Aggiornamento Orario Massimo al TFA per linea di arrivo :
000300150911     h*   spalmatura per i secondi livello
000400970910     H*------------------------------------------------------------------------*
000500970910     H DECEDIT('0,') DATEDIT(*DMY.)
000600131227     h dftactgrp(*no) actgrp(*caller)
000700970910     F*------------------------------------------------------------------------*
000800970910     F* DATA BASE
000900970910     F*------------------------------------------------------------------------*
001000151027     fwaoma11l  uf a e           k disk
001001151028     fwaoma1Xl  If   e           k disk    extfile(*extdesc)
001002151028     f                                     extdesc('WAOMA11L')
001003151028     f                                     rename(waoma010:waoma01x)
001004151028     f                                     prefix(x_)
001005151028     fazcae05l  if   e           k disk
001100130906     d*
001101151028     d kpjba         e ds
001200140103     d Tibs34ds      e ds                  inz
001300140103     d §azute        e ds                  Extname(Azute00f)
001400140103     d                                     dtaara
001500140103     d §Datiute      e ds                  EXTname(ddatiute)
001600140103     d                                     dtaara
001601151028     D fnlv55ds      E DS
001900151027     d
002100141126      * Parametri per richiamo a TRUL09R
002200141126     d Trul09ds      e ds
002300141126     d  mec                   19    108  0 dim(30)
002400131227
002401151027     d mec_a           s              3  0 dim(60)
002500130905     d datacur         s               d   datfmt(*iso)
002800151028     d S_omahmx        s                   like(omahmx)
003400140103     d dateu           s              8  0
003600141126     d ix              s              3  0
003601151028     d ixtfae          s              3  0
003800140103
003900140103     d $Finerec        s              1    inz(*off)
004000131227
004100131227     D WrkStringaSql   S           4500
004200131227     D                                     VARYING
004300130906     D
004400140103      // ? PROTOTIPI ?
004500141126     D TRUL09R         pr                  extpgm('TRUL09R')
004600141126     D  TRUL09DS                           likeds(TRUL09DS)
004601151028      // - Reperimento terminal di arrivo/partenza
004602151028     d fnlv55r         pr                  extpgm('FNLV55R')
004603151028     d  fnlv55ds                           likeds(FNLV55ds)
004700140103      /copy gaitrasrc/srcprotopr,tibs34r
004800970910     C*------------------------------------------------------------------------*
004900970910     C* MAIN LINES
005000970910     C*------------------------------------------------------------------------*
005100151027     c     *entry        plist
005200151027     c                   parm                    kpjba
005300151027     c                   movel     kpjbu         parelab           1
005400130905      /free
005500131227         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005504151028
005505151028          exsr routinz;
005506151028          // Cancello da waoma i record di eccezione (TFP>0) riferiti ai
005507151028          // secondi livello (da azcae o tfae>0)
005508151103             exsr sr_pulecc;
005600130905
005700151027          setll 'A' azcae05l;
005701151027          reade 'A' azcae05l;
005900151027
0060001510273            dow not %eof(azcae05l);
006100151027          // elaboro solo se decorrente e se tfp=0 o = al tfp che sto elaborando
0062001510274               if dateu >= caedde and dateu<=caedsc and caeatb=' '
006300151027                and caetfp=0 and caetfe=caetfa;
006301151028       // Verifico che il terminal non risulti secondo livello su OMA: in questo caso
006302151028       // non lo tratto come ter-arr
006303151028                   chain(n) (caetfe:000) waoma11l;
006304151028                   if %found(waoma11l) and omatfae=0;
006305151028       // memorizzo l'orario da spalmare sui secondi liv
006306151028       //             s_omahmx=omahmx;
006900151027
007100151027       // reperisco tutti i secondi livello del terminal di arrivo
007300151028                      clear trul09ds;
007400151028                      i09mod='TFU';
007500151028                      i09tfa=caetfe;
007600151028                      I09dta = dateu;
007700151028                      trul09r(trul09ds);
007701151027
007702151027       // aggiungo evetuali filiali che hanno il terminal come eccezione su waoma
007703151028                      exsr sr_tfae;
007704151028                      setll (caetfe) waoma1xl;
007705151028                      reade (caetfe) waoma1xl;
007706151028                      dow not %eof(waoma1xl);
007707151028
007708151028       // scrivo/updato waoma per ogni secondo livello caricato
007709151028                      for ix=1 to %elem(mec_a);
007710151028                         if mec_a(ix)=0;
007711151028                            leave;
007712151028                         endif;
007713151028                         if mec_a(ix)<> caetfe;
007714151028                            chain (mec_a(ix):x_omatfp) waoma11l;
007715151028                            if not %found(waoma11l);
007716151028                               clear waoma010;
007717151028                               omaduv=dateu;
007718151028                               OMALNA=mec_a(ix);
007719151028                               OMAtfp=x_omatfp ;
007720151028                               OMAHMX=x_omahmx;
007721151028                               if ix>=ixtfae;
007722151028                                   omatfae=caetfe  ;
007723151028                               endif;
007724151028                               write waoma010 ;
007725151028                            else;
007726151028                               if parelab='T' or parelab=*blanks or
007727151028                                 (parelab='A' and omahmx=0);
007728151028                                  OMAHMX=x_omahmx;
007729151028                                  omaduv=dateu;
007730151028                                  update waoma010 ;
007731151028                               else;
007732151028                                  unlock waoma11l;
007733151028                               endif;
007734151028                            endif;
007735151028                         endif;
007736151028                      endfor;
007737151028                         reade (caetfe) waoma1xl;
007738151028                      enddo;
008901151027          endif;
008902151028          endif;
008903151027         reade 'A' azcae05l;
009100141126       enddo;
009200130905
009300130905       // Operazioni finali
009400130905       exsr RoutEnd;
009500130905       //--------------------------------------------------------------
009600130905       //?Operazioni iniziali.
009700130905       //--------------------------------------------------------------
009800130905       BEGSR RoutInz;
009900130905
010000130905         // Reperimento dati job
010100130905         exsr DatiJob;
010200130905       // Reperisco la data corrente
010300130905          datacur=(%date());
010400131220          dateu=%dec(datacur);
010500130905
010600130905       endsr;
010700130905       //--------------------------------------------------------------
010800130905       //?Reperimento Dati del job (Utente/Operativi).
010900130905       //--------------------------------------------------------------
011000130905       BEGSR DatiJob;
011100130905
011200130905         in(E) §AzUte;
011300130905         if NOT %error;
011400130905           in(E) §DatiUte;
011500130905         endif;
011600130905         if %error or RSut = *blanks;
011700130905           clear TIBS34ds;
011800130905           tibs34r(tibs34ds);
011900130905           in §AzUte;
012000130905           in §DatiUte;
012100130905         endif;
012200130905
012300130905       ENDSR;
012400130905       //--------------------------------------------------------------
012500130905       //?Operazioni finali.
012600130905       //--------------------------------------------------------------
012700130905       BEGSR RoutEnd;
012800130905
012900130905         // Chiusura pgm   ;
013000130905
013100130905         *inLR = *on;
013200130905
013300130905         return;
013400130905
013500130905       ENDSR;
013501151028       //--------------------------------------------------------------
013502151028       //?Cancellazione record WAoma dei record con tfp>0 per i secondi liv
013503151028       //--------------------------------------------------------------
013504151028       BEGSR sr_pulecc;
013505151028          setll *loval waoma11l;
013506151028          read         waoma11l;
013507151028          dow not %eof(waoma11l);
013508151028             if omatfp>0;
013509151028       // Verifico se si tratta di secondo livello
013510151028                if omatfae=0;
013511151029                   clear fnlv55ds;
013512151028                   D55tpt = 'A'    ;
013513151028                   D55lin = omalna ;
013514151028                   d55drf = dateu;
013515151028                   fnlv55r(fnlv55ds);
013520151028                endif;
013521151028                if omatfae>0 or d55tfa<>omalna;
013522151028                   delete waoma010;
013523151028                endif;
013524151028             endif;
013525151028             read         waoma11l;
013526151028          enddo;
013527151028          unlock waoma11l;
013528151028       endsr;
013529151027       //--------------------------------------------------------------
013530151028       //?Aggiunta filiali con eccezione terminal su waoma
013531151027       //--------------------------------------------------------------
013532151027       BEGSR sr_tfae  ;
013533151027       mec_a=mec ;
013534151027       ix=%lookup(000:mec_a);
013535151028       // Memorizzo indice a partire dal quale vado a memorizzare eventuali
013536151028       // secondi livello presi da tfae
013537151028       ixtfae=ix;
013538151027       // aggiungo eventuale filiale con eccezione terminal su waoma
013539151028       WrkStringaSql='select omalna from waoma10f'         +
013540151028                     ' where omatfae=' + %editc(caetfe:'X') + ' and omatfp=0';
013541151027        $finerec=*off;
013542151028        exec sql prepare STRINGASQL from :WrkStringaSql;
013543151027        exec sql declare omaCsr cursor for StringaSql;
013544151027        exec sql open omacsr;
013545151027        dow $finerec=*off;
013546151028           exec sql Fetch OMAcsr into :omalna ;
013547151027           if sqlcod=100 or sqlcod<0;
013548151027              $finerec = *on;
013549151027              leave;
013550151027           endif;
013551151027       // aggiungo alla schiera mec_a
013552151027           mec_a(ix)=omalna;
013553151027           ix+=1;
013554151027        enddo;
013555151027        exec sql close omacsr;
013600130905
013700151028       ENDSR;
