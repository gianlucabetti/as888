000100170314      //---------------------------------------------------------------
000200170314      //?FIOR85R - Controllo Ritiri assegnati in distinta MAN-AUTO
000300170314      //---------------------------------------------------------------
000400170314     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170314
000600170314      //---------------------------------------------------------------
000700170314      //?Dichiarazione file.
000800170314      //---------------------------------------------------------------
000900170314     fFIOR85D   cf   e             workstn
001000170314     f                                     indds(IndDspF)
001100170314     f                                     infds(InfDspF)
001200170314
001300170314      // - Organigramma
001400170314     fAZORG01L  if   e           k disk
001500170314
001600170314      //---------------------------------------------------------------
001700170314      //?Definizione costanti.
001800170314      //---------------------------------------------------------------
001900170314      // - Tasti funzionali a video
002000170314     d c_F01           c                   const(x'31')
002100170314     d c_F02           c                   const(x'32')
002200170314     d c_F03           c                   const(x'33')
002300170314     d c_F04           c                   const(x'34')
002400170314     d c_F05           c                   const(x'35')
002500170314     d c_F06           c                   const(x'36')
002600170314     d c_F07           c                   const(x'37')
002700170314     d c_F08           c                   const(x'38')
002800170314     d c_F09           c                   const(x'39')
002900170314     d c_F10           c                   const(x'3A')
003000170314     d c_F11           c                   const(x'3B')
003100170314     d c_F12           c                   const(x'3C')
003200170314     d c_F13           c                   const(x'B1')
003300170314     d c_F14           c                   const(x'B2')
003400170314     d c_F15           c                   const(x'B3')
003500170314     d c_F16           c                   const(x'B4')
003600170314     d c_F17           c                   const(x'B5')
003700170314     d c_F18           c                   const(x'B6')
003800170314     d c_F19           c                   const(x'B7')
003900170314     d c_F20           c                   const(x'B8')
004000170314     d c_F21           c                   const(x'B9')
004100170314     d c_F22           c                   const(x'BA')
004200170314     d c_F23           c                   const(x'BB')
004300170314     d c_F24           c                   const(x'BC')
004400170314     d c_Enter         c                   const(x'F1')
004500170314     d c_RollDown      c                   const(x'F4')
004600170314     d c_RollUp        c                   const(x'F5')
004700170314
004800170314     d Digits          c                   const('0123456789')
004900170314
005000170314      //---------------------------------------------------------------
005100170314      //?Definizione schiere.
005200170314      //---------------------------------------------------------------
005300170314      // - Messaggi di errore
005400170314     d Msg             s             78    dim(30) ctdata perrcd(1)
005500170314
005600170314      //---------------------------------------------------------------
005700170314      //?Definizione aree dati.
005800170314      //---------------------------------------------------------------
005900170314      // - Dati utente
006000170314     d §AzUte        e ds                  extname(AZUTE00F)
006100170314     d                                     dtaara
006200170314     d §DatiUte      e ds                  extname(dDatiUte)
006300170314     d                                     dtaara
006400170314
006500170314      //---------------------------------------------------------------
006600170314      //?Definizione strutture dati.
006700170314      //---------------------------------------------------------------
006800170314      // - Status
006900170314     d Psds           sds
007000170314     d   SDSpgm          *proc
007100170314
007200170314      // - InfDS
007300170314     d InfDspF         ds
007400170314     d  dsp_aid              369    369a
007500170314
007600170314      // - Indicatori su DspF
007700170314     d IndDspF         ds
007800170314        // - Indicatori di errore in videata
007900170314     d  ErrMessage                    1n   overlay(IndDspF : 28)
008000170314        // - Indicatori di errore
008100170314     d  PosCurFIL                     1n   overlay(IndDspF : 50)
008200170314     d  PosCurDATADAL                 1n   overlay(IndDspF : 51)
008300170314     d  PosCurDATAAL                  1n   overlay(IndDspF : 52)
008400170314        // - Indicatori di errore generico
008500170314     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008600170314
008700170314     d WindDspF        ds                  inz
008800170314     d  WindDspFa              1     49    inz(*zero)
008900170314     d  WindDspFb             50     99    inz(*zero)
009000170314
009100170314      // - Parametri per pgm. controllo/inversione data?
009200170314     d wlbdat          ds                  inz
009300170314     d  G08dat                 1      8  0
009400170314     d  G08inv                 9     16  0
009500170314     d  G08err                17     17
009600170314     d  G08tgi                18     22  0
009700170314
009800170314      // - Parametri ricevuti
009900170314     d KPJBA         e ds
010000170314
010100170314      // - Reperimento dati utente
010200170314     d TIBS34DS      e ds
010300170314
010400170314      //---------------------------------------------------------------
010500170314      //?Definizione variabili globali.
010600170314      //---------------------------------------------------------------
010700170314      // - Flags booleani
010800170314     d Fine            s               n   inz(*off)
010900170314     d wInzD01         s               n   inz(*off)
011000170314
011100170314      // - Indici di schiera
011200170314     d xx              s              4s 0 inz
011300170314
011400170314      // - Campi associati al video
011500170314     d Video           s              2a   inz('D1')
011600170314
011700170314       // - Stringa SQL da eseguire
011800170314     d wSQL            s           2048    Varying        inz
011900170314
012000170314      // - Campi di comodo data
012100170314     d Data_EUR        s               d   datfmt(*eur)
012200170314     d Data_ISO        s               d   datfmt(*iso)
012300170314     d wdatadal        s              8s 0 inz
012400170314     d wdataal         s              8s 0 inz
012500170314
012600170314      // - Campi di comodo
012700170314     d Oggi            s              8s 0 inz
012800170314
012900170314      //---------------------------------------------------------------
013000170314      //?Definizione procedure esterne.
013100170314      //---------------------------------------------------------------
013200170314
013300170314      //---------------------------------------------------------------
013400170314      //?Definizione prototipi.
013500170314      //---------------------------------------------------------------
013600170314      /copy gaitrasrc/srcprotopr,TIBS34R
013700170314      /copy gaitrasrc/srcprotopr,XSRDA8
013800170314
013900170314      //---------------------------------------------------------------
014000170314      //?Definizione key-list.
014100170314      //---------------------------------------------------------------
014200170314
014300170314      //---------------------------------------------------------------
014400170314      //?M A I N - L I N E
014500170314      //---------------------------------------------------------------
014600170314     c     *Entry        plist
014700170314     c                   parm                    kpjba
014800170314
014900170314      /free
015000170314
015100170314       //?Operazioni iniziali
015200170314       exsr RoutInz;
015300170314
015400170314       //?Gestione video
015500170314       DOW  Fine = *off;
015600170314         SELECT;
015700170314           WHEN  Video = 'D1';
015800170314             exsr GesD01;
015900170314           OTHER;
016000170314             Fine = *on;
016100170314         ENDSL;
016200170314       ENDDO;
016300170314
016400170314       //?Operazioni finali
016500170314       exsr RoutEnd;
016600170314
016700170314       //--------------------------------------------------------------
016800170314       //?Operazioni iniziali.
016900170314       //--------------------------------------------------------------
017000170314       BEGSR RoutInz;
017100170314
017200170314         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017300170314
017400170314       //?Impostazione campi "fissi"
017500170314         Video = 'D1';
017600170314         wInzD01 = *on;
017700170314
017800170314       //?Imposto oggi
017900170314         Oggi = %dec(%date());
018000170314
018100170314       //?Reperimento dati job
018200170314         exsr DatiJob;
018300170314
018400170314       ENDSR;
018500170314
018600170314       //--------------------------------------------------------------
018700170314       //?Reperimento Dati del job (Utente/Operativi).
018800170314       //--------------------------------------------------------------
018900170314       BEGSR DatiJob;
019000170314
019100170314         in(E) §AzUte;
019200170314         IF  NOT %error;
019300170314           in(E) §DatiUte;
019400170314         ENDIF;
019500170314         IF  %error or RSut = *blanks;
019600170314           clear TIBS34ds;
019700170314           tibs34r(tibs34ds);
019800170314           in §AzUte;
019900170314           in §DatiUte;
020000170314         ENDIF;
020100170314
020200170314       ENDSR;
020300170314
020400170314       //--------------------------------------------------------------
020500170314       //?Gestione videata D01.
020600170314       //--------------------------------------------------------------
020700170314       BEGSR GesD01;
020800170314
020900170314       //?Inizializzazione videata
021000170314         IF  wInzD01;
021100170314           exsr InzD01;
021200170314           wInzD01 = *off;
021300170314         ENDIF;
021400170314
021500170314       //?Emissione videata
021600170314         exfmt OR85D01;
021700170314         reset ErrMessage;
021800170314         reset ErrGenerico;
021900170314         clear V01msg;
022000170314
022100170314         SELECT;
022200170314
022300170314       //?- F03=Fine
022400170314           WHEN  dsp_aid = c_F03;
022500170314             exsr F03D01;
022600170314
022700170314       //?- F06=Conferma
022800170314           WHEN  dsp_aid = c_F06;
022900170314             exsr ContrD01;
023000170314             IF  ErrGenerico;
023100170314               leavesr;
023200170314             ENDIF;
023300170314             exsr F06D01;
023400170314             Fine = *on;
023500170314
023600170314       //?Invio
023700170314           OTHER;
023800170314             exsr ContrD01;
023900170314             IF  ErrGenerico;
024000170314               leavesr;
024100170314             ENDIF;
024200170314
024300170314         ENDSL;
024400170314
024500170314       ENDSR;
024600170314
024700170314       //--------------------------------------------------------------
024800170314       //?Inizializzazione Videata D01.
024900170314       //--------------------------------------------------------------
025000170314       BEGSR InzD01;
025100170314
025200170314       //?Pulizia videata
025300170314         clear V01fil;
025400170314         clear V01dfil;
025500170314         clear V01dtdal;
025600170314         clear V01dtal;
025700170314
025800170314       //?Imposto di default tutte le filiali
025900170314         V01fil = 999;
026000170314         V01dfil = 'Tutte le Filiali';
026100170314
026200170314       ENDSR;
026300170314
026400170314       //--------------------------------------------------------------
026500170314       //?Gestione tasto funzionale F03 da videata D01
026600170314       //?F03=Fine
026700170314       //--------------------------------------------------------------
026800170314       BEGSR F03D01;
026900170314
027000170314       //?Chiusura del programma
027100170314         Fine = *on;
027200170314
027300170314       ENDSR;
027400170314
027500170314       //--------------------------------------------------------------
027600170314       //?Gestione tasto funzionale F06 da videata D01
027700170314       //?F06=Conferma
027800170314       //--------------------------------------------------------------
027900170314       BEGSR F06D01;
028000170314
028100170315         exec sql DELETE from GAITRAAZM/WFASR00F;
028200170314
028300170314       //?Elaboro tutte le filiali
028400170314         IF  V01fil = 999;
028500170314           exec sql
028600170315           INSERT into GAITRAAZM/WFASR00F
028700170315          (ASRfgs, ASRdes, ASRddc, ASRaman, ASRaaut, ASRgtman, ASRgtaut,
028800170316           ASRgaman, ASRgaaut, ASRgmman, ASRgmaut, ASRgnman, ASRgnaut)
028900170314           SELECT ORGfgs, ORGdes, ORGddc,
029000170314                  SUM(A) manuale, SUM(B) automatico,
029100170315                  SUM(C) tellusm, SUM(C1) tellusa,
029200170315                  SUM(D) anagrm, SUM(D1) anagra,
029300170316                  SUM(E) manm, SUM(E1) mana,
029400170316                  SUM(F) nogm, SUM(F1) noga
029500170314           FROM
029600170314            (SELECT ORGfgs, ORGddc,
029700170314             case when ORGnftl = 99999 then 1 else 0 end as A,
029800170314             case when ORGnftl between 1 and 99998 then 1 else 0 end as B,
029900170315             case when ORGtgi = 'T' and ORGnftl = 99999
030000170315                                    then 1 else 0 end as C,
030100170315             case when ORGtgi = 'T' and ORGnftl between 1 and 99998
030200170315                                    then 1 else 0 end as C1,
030300170315             case when ORGtgi = 'A' and ORGnftl = 99999
030400170315                                    then 1 else 0 end as D,
030500170315             case when ORGtgi = 'A' and ORGnftl between 1 and 99998
030600170315                                    then 1 else 0 end as D1,
030700170315             case when ORGtgi = 'M' and ORGnftl = 99999
030800170315                                    then 1 else 0 end as E,
030900170315             case when ORGtgi = 'M' and ORGnftl between 1 and 99998
031000170316                                    then 1 else 0 end as E1,
031100170316             case when ORGtgi = ' ' and ORGnftl = 99999
031200170316                                    then 1 else 0 end as F,
031300170316             case when ORGtgi = ' ' and ORGnftl between 1 and 99998
031400170316                                    then 1 else 0 end as F1
031500170314             FROM FNORG00F
031600170316             WHERE ORGnsr = 0 and ORGndc <> 0 and
031700170314                   ORGddc between :wdatadal and :wdataal) as tabella
031800170314             JOIN AZORG00F on ORGfgs = ORGfil
031900170314             GROUP BY ORGfgs, ORGdes, ORGddc;
032000170314
032100170314       //?Elaboro solo una filiale
032200170314         ELSE;
032300170315           exec sql
032400170315           INSERT into GAITRAAZM/WFASR00F
032500170315          (ASRfgs, ASRdes, ASRddc, ASRaman, ASRaaut, ASRgtman, ASRgtaut,
032600170316           ASRgaman, ASRgaaut, ASRgmman, ASRgmaut, ASRgnman, ASRgnaut)
032700170315           SELECT ORGfgs, ORGdes, ORGddc,
032800170315                  SUM(A) manuale, SUM(B) automatico,
032900170315                  SUM(C) tellusm, SUM(C1) tellusa,
033000170315                  SUM(D) anagrm, SUM(D1) anagra,
033100170316                  SUM(E) manm, SUM(E1) mana,
033200170316                  SUM(F) nogm, SUM(F1) noga
033300170315           FROM
033400170315            (SELECT ORGfgs, ORGddc,
033500170315             case when ORGnftl = 99999 then 1 else 0 end as A,
033600170315             case when ORGnftl between 1 and 99998 then 1 else 0 end as B,
033700170315             case when ORGtgi = 'T' and ORGnftl = 99999
033800170315                                    then 1 else 0 end as C,
033900170315             case when ORGtgi = 'T' and ORGnftl between 1 and 99998
034000170315                                    then 1 else 0 end as C1,
034100170315             case when ORGtgi = 'A' and ORGnftl = 99999
034200170315                                    then 1 else 0 end as D,
034300170315             case when ORGtgi = 'A' and ORGnftl between 1 and 99998
034400170315                                    then 1 else 0 end as D1,
034500170315             case when ORGtgi = 'M' and ORGnftl = 99999
034600170315                                    then 1 else 0 end as E,
034700170315             case when ORGtgi = 'M' and ORGnftl between 1 and 99998
034800170316                                    then 1 else 0 end as E1,
034900170316             case when ORGtgi = ' ' and ORGnftl = 99999
035000170316                                    then 1 else 0 end as F,
035100170316             case when ORGtgi = ' ' and ORGnftl between 1 and 99998
035200170316                                    then 1 else 0 end as F1
035300170315             FROM FNORG00F
035400170316             WHERE ORGnsr = 0 and ORGndc <> 0 and
035500170314                   ORGfgs = :V01fil and
035600170314                   ORGddc between :wdatadal and :wdataal) as tabella
035700170314             JOIN AZORG00F on ORGfgs = ORGfil
035800170314             GROUP BY ORGfgs, ORGdes, ORGddc;
035900170314         ENDIF;
036000170314
036100170314       ENDSR;
036200170314
036300170314       //--------------------------------------------------------------
036400170314       //?Controlla Videata D01.
036500170314       //--------------------------------------------------------------
036600170314       BEGSR ContrD01;
036700170314
036800170314         WindDspF = IndDspF;
036900170314         reset WindDspFb;
037000170314         IndDspF  = WindDspF;
037100170314
037200170314       //?Filiale gestione
037300170314         IF  V01fil = 999;
037400170314           V01dfil = 'Tutte le Filiali';
037500170314         ENDIF;
037600170314
037700170314         IF  V01fil = 0;
037800170314           clear V01dfil;
037900170314           ErrMessage  = *on;
038000170314           ErrGenerico = *on;
038100170314           PosCurFIL = *on;
038200170314           V01msg = Msg(01);
038300170314           leavesr;
038400170314         ENDIF;
038500170314
038600170314         IF  V01fil <> 999;
038700170314           clear V01dfil;
038800170314           chain V01fil AZORG01L;
038900170314           IF  not %found(AZORG01L) or
039000170314               ORGfva <> *blanks or
039100170314               ORGfag <> 'F';
039200170314             ErrMessage  = *on;
039300170314             ErrGenerico = *on;
039400170314             PosCurFIL = *on;
039500170314             V01msg = Msg(01);
039600170314             leavesr;
039700170314           ENDIF;
039800170314           V01dfil = ORGdes;
039900170314         ENDIF;
040000170314
040100170314       //?Data Dal
040200170314         IF  V01dtdal = *zeros;
040300170314           ErrMessage  = *on;
040400170314           ErrGenerico = *on;
040500170314           PosCurDATADAL = *on;
040600170314           V01msg = Msg(02);
040700170314           leavesr;
040800170314         ENDIF;
040900170314         clear wlbdat;
041000170314         G08dat = V01dtdal;
041100170314         xsrda8(wlbdat);
041200170314         IF  G08err = '1';
041300170314           ErrMessage  = *on;
041400170314           ErrGenerico = *on;
041500170314           PosCurDATADAL = *on;
041600170314           V01msg = Msg(02);
041700170314           leavesr;
041800170314         ENDIF;
041900170314         V01dtdal = G08dat;
042000170314         wdatadal = G08inv;
042100170314
042200170314       //?Data Al
042300170314         IF  V01dtal = *zeros;
042400170314           ErrMessage  = *on;
042500170314           ErrGenerico = *on;
042600170314           PosCurDATAAL = *on;
042700170314           V01msg = Msg(02);
042800170314           leavesr;
042900170314         ENDIF;
043000170314         clear wlbdat;
043100170314         G08dat = V01dtal;
043200170314         xsrda8(wlbdat);
043300170314         IF  G08err = '1';
043400170314           ErrMessage  = *on;
043500170314           ErrGenerico = *on;
043600170314           PosCurDATAAL = *on;
043700170314           V01msg = Msg(02);
043800170314           leavesr;
043900170314         ENDIF;
044000170314         V01dtal = G08dat;
044100170314         wdataal = G08inv;
044200170314
044300170314       //?Data DAL congruente con AL
044400170314         IF  wdatadal > 0 and wdataal = 0;
044500170314           ErrMessage  = *on;
044600170314           ErrGenerico = *on;
044700170314           PosCurDATADAL = *on;
044800170314           V01msg = Msg(03);
044900170314           leavesr;
045000170314         ENDIF;
045100170314         IF  wdatadal > 0 and wdataal > 0 and wdatadal > wdataal;
045200170314           ErrMessage  = *on;
045300170314           ErrGenerico = *on;
045400170314           PosCurDATADAL = *on;
045500170314           V01msg = Msg(03);
045600170314           leavesr;
045700170314         ENDIF;
045800170314
045900170314       ENDSR;
046000170314
046100170314
046200170314       //--------------------------------------------------------------
046300170314       //?Operazioni finali.
046400170314       //--------------------------------------------------------------
046500170314       BEGSR RoutEnd;
046600170314
046700170314         *inLR = *on;
046800170314         return;
046900170314
047000170314       ENDSR;
047100170314
047200170314      /end-free
047300170314** -- MSG -------------------------------------------------------------------*
047400170314Filiale errata                                                                 1
047500170314Data errata                                                                    2
047600170314Data DAL incongruente con data AL                                              3
