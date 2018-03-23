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
007100151117             // se terminal di partenza e arrivo imposto i flag di scambio e transito
007200151117                if d55tfp=orgfil and orgfil=d55tfa  ;
007300151117               //    TWFFTFP=1                ;
007400151117                   TWFFFLP=1                 ;
007500151117                   TWFFPSC=1                 ;
007600141121                endif;
007700151117
007800151117                // solo se terminal di arrivo imposto falg terminal
007900151117                if d55tfa=orgfil;
008000151117                   TWFFTFP=1                ;
008100151117                endif  ;
008200151117
008300141121                TWFATFP=%editc(d55tfp:'X') ;
008400141121                TWFATFA=%editc(d55tfa:'X');
008500151117
008600151117                //if TWFFTFP=1;
008700151117                //   TWFFFLP=1                 ;
008800151117                //   TWFFPSC=1                 ;
008900151117                //endif;
009000151026
009100151103                clear omatfae ;
009200151103                clear omahmx  ;
009300151026                chain (orgfil:000) waoma11l;
009400151026                if %found(waoma11l);
009500151026                  TWFAHHX=%subst(%editc(OMAHMX:'X'):1:2) ;
009600151026                  TWFAmmX=%subst(%editc(OMAHMX:'X'):3:2) ;
009700151026
009800151026                  // se c'è eccezione terminal arrivo la imposto
009900151026                  if omatfae>0  ;
010000151026                   TWFATFA=%editc(omatfae:'X');
010100151118                   TWFATFP=%editc(omatfae:'X');
010200151117                   // se presente eccezione non è un terminal
010300151117                   TWFFTFP=0                ;
010400151117                   TWFFFLP=0                 ;
010500151117                   TWFFPSC=0                 ;
010600151026                  endif  ;
010700151026
010800151029                twffaut=omafaut  ;
010900151029                twffpos=omafpos  ;
011000151029                twfnbap=omanbap  ;
011100151026                twfnbaa=omanbaa  ;
011200151026                twfncas=omancas  ;
011300151029                twfnsmr=omansmr  ;
011400151026                  TWFAfaihh=%subst(%editc(OMAafai:'X'):1:2) ;
011500151026                  TWFAfaimm=%subst(%editc(OMAafai:'X'):3:2) ;
011600151026                  TWFAfafhh=%subst(%editc(OMAafaf:'X'):1:2) ;
011700151026                  TWFAfafmm=%subst(%editc(OMAafaf:'X'):3:2) ;
011800151026                  TWFAfpihh=%subst(%editc(OMAafpi:'X'):1:2) ;
011900151026                  TWFAfpimm=%subst(%editc(OMAafpi:'X'):3:2) ;
012000151026                  TWFAfpfhh=%subst(%editc(OMAafpf:'X'):1:2) ;
012100151026                  TWFAfpfmm=%subst(%editc(OMAafpf:'X'):3:2) ;
012200151026                twftfic=omatfic  ;
012300151026                twftfis=omatfis  ;
012400151026                twftvvc=omatvvc  ;
012500151026                twftvvs=omatvvs  ;
012600151026                endif;
012700151103                if not %found(waoma11l) or (omatfae>0 and omahmx=0)
012800151102                                        or (omahmx=0 and d55tfa<>orgfil);
012900151102                 if omatfae>0;
013000151102                   chain (omatfae:000) waoma11l;
013100151102                 else;
013200151102                   chain (d55tfa:000) waoma11l;
013300151102                 endif;
013400151102                 if %found(waoma11l);
013500151102                  TWFAHHX=%subst(%editc(OMAHMX:'X'):1:2) ;
013600151102                  TWFAmmX=%subst(%editc(OMAHMX:'X'):3:2) ;
013700151102                 endif;
013800151102
013900151102                endif;
014000151026
014100141128                write wftwf000;
014200141128
014300141128             endif;
014400141121
014500131220          endif                                   ;
014600141121          read azorg01l;
014700131220       enddo;
014800130905
014900130905       // Operazioni finali
015000130905       exsr RoutEnd;
015100130905       //--------------------------------------------------------------
015200130905       //?Operazioni iniziali.
015300130905       //--------------------------------------------------------------
015400130905       BEGSR RoutInz;
015500130905
015600130905         // Reperimento dati job
015700130905         exsr DatiJob;
015800130905       // Reperisco la data corrente
015900130905          datacur=(%date());
016000131220          dateu=%dec(datacur);
016100130905
016200130905       endsr;
016300130905       //--------------------------------------------------------------
016400130905       //?Reperimento Dati del job (Utente/Operativi).
016500130905       //--------------------------------------------------------------
016600130905       BEGSR DatiJob;
016700130905
016800130905         in(E) §AzUte;
016900130905         if NOT %error;
017000130905           in(E) §DatiUte;
017100130905         endif;
017200130905         if %error or RSut = *blanks;
017300130905           clear TIBS34ds;
017400130905           tibs34r(tibs34ds);
017500130905           in §AzUte;
017600130905           in §DatiUte;
017700130905         endif;
017800130905
017900130905       ENDSR;
018000130905       //--------------------------------------------------------------
018100130905       //?Operazioni finali.
018200130905       //--------------------------------------------------------------
018300130905       BEGSR RoutEnd;
018400130905
018500130905         // Chiusura pgm   ;
018600141128
018700141128             clear fnlv55ds;
018800141128             d55tla='C'     ;
018900141128             fnlv55r(fnlv55ds);
019000130905
019100130905         *inLR = *on;
019200130905
019300130905         return;
019400130905
019500130905       ENDSR;
019600130905
