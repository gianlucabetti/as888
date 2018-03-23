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
001100151028     fwaoma1Xl  If   e           k disk    extfile(*extdesc)
001200151028     f                                     extdesc('WAOMA11L')
001300151028     f                                     rename(waoma010:waoma01x)
001400151028     f                                     prefix(x_)
001500151028     fazcae05l  if   e           k disk
001600130906     d*
001700151028     d kpjba         e ds
001800140103     d Tibs34ds      e ds                  inz
001900140103     d §azute        e ds                  Extname(Azute00f)
002000140103     d                                     dtaara
002100140103     d §Datiute      e ds                  EXTname(ddatiute)
002200140103     d                                     dtaara
002300151028     D fnlv55ds      E DS
002400151027     d
002500141126      * Parametri per richiamo a TRUL09R
002600141126     d Trul09ds      e ds
002700141126     d  mec                   19    108  0 dim(30)
002800131227
002900151027     d mec_a           s              3  0 dim(60)
003000130905     d datacur         s               d   datfmt(*iso)
003100151028     d S_omahmx        s                   like(omahmx)
003200140103     d dateu           s              8  0
003300141126     d ix              s              3  0
003400151028     d ixtfae          s              3  0
003500140103
003600140103     d $Finerec        s              1    inz(*off)
003700131227
003800131227     D WrkStringaSql   S           4500
003900131227     D                                     VARYING
004000130906     D
004100140103      // ? PROTOTIPI ?
004200141126     D TRUL09R         pr                  extpgm('TRUL09R')
004300141126     D  TRUL09DS                           likeds(TRUL09DS)
004400151028      // - Reperimento terminal di arrivo/partenza
004500151028     d fnlv55r         pr                  extpgm('FNLV55R')
004600151028     d  fnlv55ds                           likeds(FNLV55ds)
004700140103      /copy gaitrasrc/srcprotopr,tibs34r
004800970910     C*------------------------------------------------------------------------*
004900970910     C* MAIN LINES
005000970910     C*------------------------------------------------------------------------*
005100151027     c     *entry        plist
005200151027     c                   parm                    kpjba
005300151027     c                   movel     kpjbu         parelab           1
005400130905      /free
005500131227         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005600151028
005700151028          exsr routinz;
005800151028          // Cancello da waoma i record di eccezione (TFP>0) riferiti ai
005900151028          // secondi livello (da azcae o tfae>0)
006000151103             exsr sr_pulecc;
006100130905
006200151027          setll 'A' azcae05l;
006300151027          reade 'A' azcae05l;
006400151027
0065001510273            dow not %eof(azcae05l);
006600151027          // elaboro solo se decorrente e se tfp=0 o = al tfp che sto elaborando
0067001510274               if dateu >= caedde and dateu<=caedsc and caeatb=' '
006800151027                and caetfp=0 and caetfe=caetfa;
006900151028       // Verifico che il terminal non risulti secondo livello su OMA: in questo caso
007000151028       // non lo tratto come ter-arr
007100151028                   chain(n) (caetfe:000) waoma11l;
007200151028                   if %found(waoma11l) and omatfae=0;
007300151028       // memorizzo l'orario da spalmare sui secondi liv
007400151028       //             s_omahmx=omahmx;
007500151027
007600151027       // reperisco tutti i secondi livello del terminal di arrivo
007700151028                      clear trul09ds;
007800151028                      i09mod='TFU';
007900151028                      i09tfa=caetfe;
008000151028                      I09dta = dateu;
008100151028                      trul09r(trul09ds);
008200151027
008300151027       // aggiungo evetuali filiali che hanno il terminal come eccezione su waoma
008400151028                      exsr sr_tfae;
008500151028                      setll (caetfe) waoma1xl;
008600151028                      reade (caetfe) waoma1xl;
008700151028                      dow not %eof(waoma1xl);
008800151028
008900151028       // scrivo/updato waoma per ogni secondo livello caricato
009000151028                      for ix=1 to %elem(mec_a);
009100151028                         if mec_a(ix)=0;
009200151028                            leave;
009300151028                         endif;
009400151028                         if mec_a(ix)<> caetfe;
009500151028                            chain (mec_a(ix):x_omatfp) waoma11l;
009600151028                            if not %found(waoma11l);
009700151028                               clear waoma010;
009800151028                               omaduv=dateu;
009900151028                               OMALNA=mec_a(ix);
010000151028                               OMAtfp=x_omatfp ;
010100151028                               OMAHMX=x_omahmx;
010200151104                               OMAHMp=x_omahmp;
010300151028                               if ix>=ixtfae;
010400151028                                   omatfae=caetfe  ;
010500151028                               endif;
010600151028                               write waoma010 ;
010700151028                            else;
010800151028                               if parelab='T' or parelab=*blanks or
010900151028                                 (parelab='A' and omahmx=0);
011000151028                                  OMAHMX=x_omahmx;
011100151104                                  OMAHMp=x_omahmp;
011200151028                                  omaduv=dateu;
011300151028                                  update waoma010 ;
011400151028                               else;
011500151028                                  unlock waoma11l;
011600151028                               endif;
011700151028                            endif;
011800151028                         endif;
011900151028                      endfor;
012000151028                         reade (caetfe) waoma1xl;
012100151028                      enddo;
012200151027          endif;
012300151028          endif;
012400151027         reade 'A' azcae05l;
012500141126       enddo;
012600130905
012700130905       // Operazioni finali
012800130905       exsr RoutEnd;
012900130905       //--------------------------------------------------------------
013000130905       //?Operazioni iniziali.
013100130905       //--------------------------------------------------------------
013200130905       BEGSR RoutInz;
013300130905
013400130905         // Reperimento dati job
013500130905         exsr DatiJob;
013600130905       // Reperisco la data corrente
013700130905          datacur=(%date());
013800131220          dateu=%dec(datacur);
013900130905
014000130905       endsr;
014100130905       //--------------------------------------------------------------
014200130905       //?Reperimento Dati del job (Utente/Operativi).
014300130905       //--------------------------------------------------------------
014400130905       BEGSR DatiJob;
014500130905
014600130905         in(E) §AzUte;
014700130905         if NOT %error;
014800130905           in(E) §DatiUte;
014900130905         endif;
015000130905         if %error or RSut = *blanks;
015100130905           clear TIBS34ds;
015200130905           tibs34r(tibs34ds);
015300130905           in §AzUte;
015400130905           in §DatiUte;
015500130905         endif;
015600130905
015700130905       ENDSR;
015800130905       //--------------------------------------------------------------
015900130905       //?Operazioni finali.
016000130905       //--------------------------------------------------------------
016100130905       BEGSR RoutEnd;
016200130905
016300130905         // Chiusura pgm   ;
016400130905
016500130905         *inLR = *on;
016600130905
016700130905         return;
016800130905
016900130905       ENDSR;
017000151028       //--------------------------------------------------------------
017100151028       //?Cancellazione record WAoma dei record con tfp>0 per i secondi liv
017200151028       //--------------------------------------------------------------
017300151028       BEGSR sr_pulecc;
017400151028          setll *loval waoma11l;
017500151028          read         waoma11l;
017600151028          dow not %eof(waoma11l);
017700151028             if omatfp>0;
017800151028       // Verifico se si tratta di secondo livello
017900151028                if omatfae=0;
018000151029                   clear fnlv55ds;
018100151028                   D55tpt = 'A'    ;
018200151028                   D55lin = omalna ;
018300151028                   d55drf = dateu;
018400151028                   fnlv55r(fnlv55ds);
018500151028                endif;
018600151028                if omatfae>0 or d55tfa<>omalna;
018700151028                   delete waoma010;
018800151028                endif;
018900151028             endif;
019000151028             read         waoma11l;
019100151028          enddo;
019200151028          unlock waoma11l;
019300151028       endsr;
019400151027       //--------------------------------------------------------------
019500151028       //?Aggiunta filiali con eccezione terminal su waoma
019600151027       //--------------------------------------------------------------
019700151027       BEGSR sr_tfae  ;
019800151027       mec_a=mec ;
019900151027       ix=%lookup(000:mec_a);
020000151028       // Memorizzo indice a partire dal quale vado a memorizzare eventuali
020100151028       // secondi livello presi da tfae
020200151028       ixtfae=ix;
020300151027       // aggiungo eventuale filiale con eccezione terminal su waoma
020400151028       WrkStringaSql='select omalna from waoma10f'         +
020500151028                     ' where omatfae=' + %editc(caetfe:'X') + ' and omatfp=0';
020600151027        $finerec=*off;
020700151028        exec sql prepare STRINGASQL from :WrkStringaSql;
020800151027        exec sql declare omaCsr cursor for StringaSql;
020900151027        exec sql open omacsr;
021000151027        dow $finerec=*off;
021100151028           exec sql Fetch OMAcsr into :omalna ;
021200151027           if sqlcod=100 or sqlcod<0;
021300151027              $finerec = *on;
021400151027              leave;
021500151027           endif;
021600151027       // aggiungo alla schiera mec_a
021700151027           mec_a(ix)=omalna;
021800151027           ix+=1;
021900151027        enddo;
022000151027        exec sql close omacsr;
022100130905
022200151028       ENDSR;
