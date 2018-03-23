000100131227     H*------------------------------------------------------------------------*
000200141121     H* Creazione File filiali WFTWF
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000401131227     h dftactgrp(*no) actgrp(*caller)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000705141121     fazorg01l  if   e           K disk
000706141128     fwaoma01l  if   e           K disk
000707141121     fwftwf00f  o    e             disk
001901130906
001902130906     d*
001903141121     d kpjba         e ds
001904141121     d Tntls5ds      e ds
001905141121     d fnlv55ds      e ds
001906140103     d Tibs34ds      e ds                  inz
001907140103     d §azute        e ds                  Extname(Azute00f)
001908140103     d                                     dtaara
001909140103     d §Datiute      e ds                  EXTname(ddatiute)
001910140103     d                                     dtaara
001911131227
001912130905     d datacur         s               d   datfmt(*iso)
001913130905     d dataiso         s               d   datfmt(*iso)
001914130905     d dataeur         s               d   datfmt(*eur)
001922140103     d dateu           s              8  0
001923140108     d ii              s              2  0
001924140103
010500130906     D
010501140103      // ? PROTOTIPI ?
010502140103      /copy gaitrasrc/srcprotopr,tibs34r
010503141121      /copy gaitrasrc/srcprotopr,fnlv55r
011219130905      //---------------------------------------------------------------
011220130905      //?Definizione procedure usate.
011221130905      //---------------------------------------------------------------
011300970910     C*------------------------------------------------------------------------*
011400970910     C* MAIN LINES
011500970910     C*------------------------------------------------------------------------*
011501141121     C     *entry        plist
011502141121     C                   parm                    kpjba
011503141121     c                   eval      tntls5ds=kpjbu
011504130905      /free
011505141121         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011506141126
011507141126       EXEC sql  delete from WFTWF00F          ;
011508130905
011509130905       // Operazioni iniziali
011510130905       exsr RoutInz;
011511141121       // lettura delle filiali su azorg
011512141121       setll *loval azorg01l;
011513141121       read  azorg01l;
011514141121       dow not %eof(azorg01l);
011515141121          // elaboro solo se record non annullato e filiale o agenzia
011516141121          if orgfva=*blanks and (orgfag='F' or orgfag='A');
011517141121          //   Determino il terminal di partenza e di arrivo standard alla data richiesta
011518141121             clear fnlv55ds;
011519141121             d55lin = orgfil;
011520141121             d55drf=tls5drf;
011521141121             fnlv55r(fnlv55ds);
011522141121
011523141121          // Impostazione campi del wf e scrittura record
011524141121             clear wftwf000;
011525141121             %subst(TWFAFIL:1:3)=%editc(orgfil:'X') ;
011526141121             TWFDESF=orgdes           ;
011527141128          // Se errore da reperimento terminal non scrivo il record
011528141128          // In questo modo escludo 180-Poste. Per quanto riguarda
011529141128          // Le nuove filiali bisognerà appurare se al momento della
011530141128          // creazione dovranno essere scritte anche in mancanza
011531141128          // del ter-arr perchè in questo caso il pgm così fatto non va bene
011532141121             if d55err=*blanks ;
011533141121                if d55tfp=orgfil;
011534141121                   TWFFTFP=1                ;
011535141121                endif;
011536141121                TWFATFP=%editc(d55tfp:'X') ;
011537141121                TWFATFA=%editc(d55tfa:'X');
011538141121                if TWFFTFP=1;
011539141121                   TWFFFLP=1                 ;
011540141121                   TWFFPSC=1                 ;
011541141121                endif;
011542141128                chain (orgfil:000) waoma01l;
011543141128                if %found(waoma01l);
011544150514                  TWFAHMX=%trim(%editw(OMAHMX:'0  :  '));
011545141128                endif;
011546141128                write wftwf000;
011547141128
011548141128             endif;
011549141121
011558131220          endif                                   ;
011559141121          read azorg01l;
011560131220       enddo;
011561130905
011562130905       // Operazioni finali
011563130905       exsr RoutEnd;
011884130905       //--------------------------------------------------------------
011885130905       //?Operazioni iniziali.
011886130905       //--------------------------------------------------------------
011887130905       BEGSR RoutInz;
011888130905
011890130905         // Reperimento dati job
011891130905         exsr DatiJob;
011892130905       // Reperisco la data corrente
011893130905          datacur=(%date());
011894131220          dateu=%dec(datacur);
011897130905
011898130905       endsr;
011899130905       //--------------------------------------------------------------
011900130905       //?Reperimento Dati del job (Utente/Operativi).
011901130905       //--------------------------------------------------------------
011902130905       BEGSR DatiJob;
011903130905
011904130905         in(E) §AzUte;
011905130905         if NOT %error;
011906130905           in(E) §DatiUte;
011907130905         endif;
011908130905         if %error or RSut = *blanks;
011909130905           clear TIBS34ds;
011910130905           tibs34r(tibs34ds);
011911130905           in §AzUte;
011912130905           in §DatiUte;
011913130905         endif;
011914130905
011915130905       ENDSR;
011916130905       //--------------------------------------------------------------
011917130905       //?Operazioni finali.
011918130905       //--------------------------------------------------------------
011919130905       BEGSR RoutEnd;
011920130905
011921130905         // Chiusura pgm   ;
011922141128
011923141128             clear fnlv55ds;
011924141128             d55tla='C'     ;
011925141128             fnlv55r(fnlv55ds);
011926130905
011927130905         *inLR = *on;
011928130905
011929130905         return;
011930130905
011931130905       ENDSR;
011932130905
