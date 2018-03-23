000100131227     H*------------------------------------------------------------------------*
000200141121     H* Creazione File filiali WFTWF
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500131227     h dftactgrp(*no) actgrp(*caller)
000600970910     F*------------------------------------------------------------------------*
000700970910     F* DATA BASE
000800970910     F*------------------------------------------------------------------------*
000900141121     fazorg01l  if   e           K disk
001000151026     fwaoma11l  if   e           K disk
001100141121     fwftwf00f  o    e             disk
001200130906
001300130906     d*
001400141121     d kpjba         e ds
001500141121     d Tntls5ds      e ds
001600141121     d fnlv55ds      e ds
001700140103     d Tibs34ds      e ds                  inz
001800140103     d §azute        e ds                  Extname(Azute00f)
001900140103     d                                     dtaara
002000140103     d §Datiute      e ds                  EXTname(ddatiute)
002100140103     d                                     dtaara
002200131227
002300130905     d datacur         s               d   datfmt(*iso)
002400130905     d dataiso         s               d   datfmt(*iso)
002500130905     d dataeur         s               d   datfmt(*eur)
002600140103     d dateu           s              8  0
002700140108     d ii              s              2  0
002800140103
002900130906     D
003000140103      // ? PROTOTIPI ?
003100140103      /copy gaitrasrc/srcprotopr,tibs34r
003200141121      /copy gaitrasrc/srcprotopr,fnlv55r
003300130905      //---------------------------------------------------------------
003400130905      //?Definizione procedure usate.
003500130905      //---------------------------------------------------------------
003600970910     C*------------------------------------------------------------------------*
003700970910     C* MAIN LINES
003800970910     C*------------------------------------------------------------------------*
003900141121     C     *entry        plist
004000141121     C                   parm                    kpjba
004100141121     c                   eval      tntls5ds=kpjbu
004200130905      /free
004300141121         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
004400141126
004500141126       EXEC sql  delete from WFTWF00F          ;
004600130905
004700130905       // Operazioni iniziali
004800130905       exsr RoutInz;
004900141121       // lettura delle filiali su azorg
005000141121       setll *loval azorg01l;
005100141121       read  azorg01l;
005200141121       dow not %eof(azorg01l);
005300141121          // elaboro solo se record non annullato e filiale o agenzia
005400141121          if orgfva=*blanks and (orgfag='F' or orgfag='A');
005500141121          //   Determino il terminal di partenza e di arrivo standard alla data richiesta
005600141121             clear fnlv55ds;
005700141121             d55lin = orgfil;
005800141121             d55drf=tls5drf;
005900141121             fnlv55r(fnlv55ds);
006000141121
006100141121          // Impostazione campi del wf e scrittura record
006200141121             clear wftwf000;
006300141121             %subst(TWFAFIL:1:3)=%editc(orgfil:'X') ;
006400141121             TWFDESF=orgdes           ;
006500141128          // Se errore da reperimento terminal non scrivo il record
006600141128          // In questo modo escludo 180-Poste. Per quanto riguarda
006700141128          // Le nuove filiali bisognerà appurare se al momento della
006800141128          // creazione dovranno essere scritte anche in mancanza
006900141128          // del ter-arr perchè in questo caso il pgm così fatto non va bene
007000141121             if d55err=*blanks ;
007100141121                if d55tfp=orgfil;
007200141121                   TWFFTFP=1                ;
007300141121                endif;
007400141121                TWFATFP=%editc(d55tfp:'X') ;
007500141121                TWFATFA=%editc(d55tfa:'X');
007600141121                if TWFFTFP=1;
007700141121                   TWFFFLP=1                 ;
007800141121                   TWFFPSC=1                 ;
007900141121                endif;
008000151026
008100151026                chain (orgfil:000) waoma11l;
008200151026                if %found(waoma11l);
008300151026                  TWFAHHX=%subst(%editc(OMAHMX:'X'):1:2) ;
008400151026                  TWFAmmX=%subst(%editc(OMAHMX:'X'):3:2) ;
008500151026
008600151026                  // se c'è eccezione terminal arrivo la imposto
008700151026                  if omatfae>0  ;
008800151026                   TWFATFA=%editc(omatfae:'X');
008900151026                  endif  ;
009000151026
009100151029                twffaut=omafaut  ;
009200151029                twffpos=omafpos  ;
009300151029                twfnbap=omanbap  ;
009400151026                twfnbaa=omanbaa  ;
009500151026                twfncas=omancas  ;
009600151029                twfnsmr=omansmr  ;
009700151026                  TWFAfaihh=%subst(%editc(OMAafai:'X'):1:2) ;
009800151026                  TWFAfaimm=%subst(%editc(OMAafai:'X'):3:2) ;
009900151026                  TWFAfafhh=%subst(%editc(OMAafaf:'X'):1:2) ;
010000151026                  TWFAfafmm=%subst(%editc(OMAafaf:'X'):3:2) ;
010100151026                  TWFAfpihh=%subst(%editc(OMAafpi:'X'):1:2) ;
010200151026                  TWFAfpimm=%subst(%editc(OMAafpi:'X'):3:2) ;
010300151026                  TWFAfpfhh=%subst(%editc(OMAafpf:'X'):1:2) ;
010400151026                  TWFAfpfmm=%subst(%editc(OMAafpf:'X'):3:2) ;
010500151026                twftfic=omatfic  ;
010600151026                twftfis=omatfis  ;
010700151026                twftvvc=omatvvc  ;
010800151026                twftvvs=omatvvs  ;
010900151026                endif;
011000151026
011100141128                write wftwf000;
011200141128
011300141128             endif;
011400141121
011500131220          endif                                   ;
011600141121          read azorg01l;
011700131220       enddo;
011800130905
011900130905       // Operazioni finali
012000130905       exsr RoutEnd;
012100130905       //--------------------------------------------------------------
012200130905       //?Operazioni iniziali.
012300130905       //--------------------------------------------------------------
012400130905       BEGSR RoutInz;
012500130905
012600130905         // Reperimento dati job
012700130905         exsr DatiJob;
012800130905       // Reperisco la data corrente
012900130905          datacur=(%date());
013000131220          dateu=%dec(datacur);
013100130905
013200130905       endsr;
013300130905       //--------------------------------------------------------------
013400130905       //?Reperimento Dati del job (Utente/Operativi).
013500130905       //--------------------------------------------------------------
013600130905       BEGSR DatiJob;
013700130905
013800130905         in(E) §AzUte;
013900130905         if NOT %error;
014000130905           in(E) §DatiUte;
014100130905         endif;
014200130905         if %error or RSut = *blanks;
014300130905           clear TIBS34ds;
014400130905           tibs34r(tibs34ds);
014500130905           in §AzUte;
014600130905           in §DatiUte;
014700130905         endif;
014800130905
014900130905       ENDSR;
015000130905       //--------------------------------------------------------------
015100130905       //?Operazioni finali.
015200130905       //--------------------------------------------------------------
015300130905       BEGSR RoutEnd;
015400130905
015500130905         // Chiusura pgm   ;
015600141128
015700141128             clear fnlv55ds;
015800141128             d55tla='C'     ;
015900141128             fnlv55r(fnlv55ds);
016000130905
016100130905         *inLR = *on;
016200130905
016300130905         return;
016400130905
016500130905       ENDSR;
016600130905
