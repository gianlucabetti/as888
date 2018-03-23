000100111128       //==============================================================
000200120417       // TNVRA03R - Stampa le 30 occorrenze ricevute con la statistica parziale ottenuta dal A02
000300120405       //==============================================================
000400111128
000500120404       // per compilare si deve avere in linea la lib. FILTRAPRD
000600111128
000700111128     /*PRM  dbgview(*source)
000800111128     /*END
000900111128
001000111128       //--------------------------------------------------------------
001100120207       // Specifiche di controllo.                                     ?
001200111128       //--------------------------------------------------------------
001300111128
001400111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500120416     h dftactgrp(*no) bnddir('UBBNDDIR') actgrp(*caller)
001600111128
001700111128       //--------------------------------------------------------------
001800120207       // Dichiarazione file.                                          ?
001900111128       //--------------------------------------------------------------
002000120416     fTNVRA03P  o    e             printer usropn
002100120416     f                                     oflind(*in25)
002200120413
002300120413
002400111128       //--------------------------------------------------------------
002500120207       // Definizione costanti.                                        ?
002600111128       //--------------------------------------------------------------
002700111128
002800120413
002900111128       //--------------------------------------------------------------
003000120207       // Definizione schiere.                                         ?
003100111128       //--------------------------------------------------------------
003200111128
003300111128
003400111128       //--------------------------------------------------------------
003500120207       // Definizione aree dati.                                       ?
003600111128       //--------------------------------------------------------------
003700120416       // - Dati utente?
003800120416     d §AzUte        e ds                  extname(AZUTE00F)
003900120416     d                                     dtaara
004000120416     d §DatiUte      e ds                  extname(dDatiUte)
004100120416     d                                     dtaara
004200111128
004300111128
004400111128       //--------------------------------------------------------------
004500120207       // Definizione strutture dati.                                  ?
004600111128       //--------------------------------------------------------------
004700120416       // - Status ds?
004800120416     d Status         sds
004900120416     d  SDSpgm           *proc
005000120416
005100120416     d Kpjba         e ds
005200120416
005300120413     d FIARG00F      e ds
005400120413     d FNARB00F      e ds
005500120417     d DatiXStampa     ds                  qualified dim(30)
005600120413     d  AAS_non                            like(ARBAAS)
005700120413     d  LNP_non                            like(ARBLNP)
005800120413     d  NRS_non                            like(ARBNRS)
005900120413     d  NSP_non                            like(ARBNSP)
006000120413     d  AAS                                like(ARBAAS)
006100120413     d  LNP                                like(ARBLNP)
006200120413     d  NRS                                like(ARBNRS)
006300120413     d  NSP                                like(ARBNSP)
006400120413     d  TGI                                like(ARGTGI)
006500120413     d  CGI                                like(ARGCGI)
006600120413     d  IND                                like(ARBIND)
006700120413     d  CAD                                like(ARBCAD)
006800120413     d  LOD                                like(ARBLOD)
006900120413     d  PRD                                like(ARBPRD)
007000120413     d  Dt_Sped                       8s 0
007100120416     d  TotNonAtt                     5s 0
007200111128
007300111128       //--------------------------------------------------------------
007400120207       // Definizione variabili globali.                               ?
007500111128       //--------------------------------------------------------------
007600120207
007700120207       // - Flags booleani
007800120208     d ErrBlock        s               n   inz(*off)
007900120207
008000120207       // - Variabili di appoggio
008100120416     d Qcmd            s            500    inz
008200120405     d wX              s              5s 0
008300120413     d wDataDa         s              8s 0
008400120413     d wDataA          s              8s 0
008500120417     d Dati_NrRows     s              3s 0
008600120416     d wDate_Eur       s               d   inz  datfmt(*eur)
008700120416     d wDate_Iso       s               d   inz  datfmt(*iso)
008800111128
008900111128       //--------------------------------------------------------------
009000120210       // Definizione procedure usate
009100111128       //--------------------------------------------------------------
009200120416
009300120416       // Reperimento dati utente?
009400120416     d TIBS34ds      e ds                  inz
009500120416      /copy gaitrasrc/srcProtoPr,TIBS34R
009600120416
009700120416      /COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009800120416      /COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009900120404
010000120416     d TNVRA03R        pr
010100120417     d  in_Dati                            likeds(DatiXStampa) dim(30)
010200120417     d  in_NrRows                          like(Dati_NrRows)
010300120417     d  in_DtRif                      8s 0
010400120416     d  in_kpjba                           like(kpjba)
010500120413     d  out_Errore                    1
010600120404
010700120416     d TNVRA03R        pi
010800120417     d  in_Dati                            likeds(DatiXStampa) dim(30)
010900120417     d  in_NrRows                          like(Dati_NrRows)
011000120417     d  in_DtRif                      8s 0
011100120416     d  in_kpjba                           like(kpjba)
011200120413     d  out_Errore                    1
011300111128
011400111128       //--------------------------------------------------------------
011500120208       // Definizione key-list.                                        ?
011600111128       //--------------------------------------------------------------
011700111128
011800111128
011900111128       //--------------------------------------------------------------
012000120208       // M A I N - L I N E                                            ?
012100111128       //--------------------------------------------------------------
012200111128
012300111128      /free
012400111128
012500120208       // - Operazioni iniziali
012600111128       exsr  sr_RoutInz;
012700111128
012800120416       // - Gestione stampa
012900120416       exsr  GesStampa;
013000120207
013100120208       // - Operazioni finali
013200111128       exsr  sr_RoutEnd;
013300111128
013400111128       //--------------------------------------------------------------
013500120208       // Operazioni iniziali
013600111128       //--------------------------------------------------------------
013700111128       BEGSR sr_RoutInz;
013800111128
013900111128         *inLR = *on;
014000120417
014100120417         DatiXStampa=in_Dati;
014200120417         Dati_NrRows=in_NrRows;
014300120417         kpjba=in_kpjba;
014400120417         DtRif_A = %char(%date(in_DtRif:*iso):*eur);
014500120417         out_Errore=*blank;
014600120416
014700120416         // Apertura archivi in gestione?
014800120416         open  TNVRA03P;
014900120416
015000120416         T1Cpgm = SDSpgm;
015100120416         wDate_Eur = %date();
015200120416         T1Cdta = %dec( wDate_Eur );
015300120416         T1Cora = %int( %subst( %char( %dec( %timestamp() ) ) :9 :6 ) );
015400120416
015500120416         exsr sr_DatiJob;
015600120416
015700120417         *in25 = *on;
015800120417         exsr StampaTestata;
015900120413
016000111128       ENDSR;
016100120416
016200120416       //--------------------------------------------------------------
016300120417       // Stampa testata e intestazioni
016400120416       //--------------------------------------------------------------
016500120417       BEGSR  StampaTestata;
016600120416
016700120417         write VRA03TXT;
016800120417         write VRA03INT;
016900120417         write VRA03IN1;
017000120417         write VRA03IN2;
017100120417         *in25 = *off;
017200120417
017300120417       ENDSR;
017400120417
017500120417       //--------------------------------------------------------------
017600120417       // Reperimento Dati del job (Utente/Operativi).                 ?
017700120417       //--------------------------------------------------------------
017800120417       BEGSR  sr_DatiJob;
017900120417
018000120416         in(E) §AzUte;
018100120416         if NOT %error;
018200120416           in(E) §DatiUte;
018300120416         endif;
018400120416         if %error or RSut = *blanks;
018500120416           clear TIBS34ds;
018600120416           tibs34r ( tibs34ds );
018700120416           in §AzUte;
018800120416           in §DatiUte;
018900120416         endif;
019000120416
019100120416       ENDSR;
019200120405
019300120405       //--------------------------------------------------------------
019400120416       // Stampo le righe estratte
019500120405       //--------------------------------------------------------------
019600120416       BEGSR  GesStampa;
019700120405
019800120417         for wX = 1 to Dati_NrRows;
019900120416           // punto all'occorrenza da valorizzare
020000120417        // DatiXStampa(wX);
020100120417           AAS_non = DatiXStampa(wX).AAS_non;
020200120417           LNP_non = DatiXStampa(wX).LNP_non;
020300120417           NRS_non = DatiXStampa(wX).NRS_non;
020400120417           NSP_non = DatiXStampa(wX).NSP_non;
020500120417           AAS     = DatiXStampa(wX).AAS;
020600120417           LNP     = DatiXStampa(wX).LNP;
020700120417           NRS     = DatiXStampa(wX).NRS;
020800120417           NSP     = DatiXStampa(wX).NSP;
020900120417           TGI     = DatiXStampa(wX).TGI;
021000120417           CGI     = DatiXStampa(wX).CGI;
021100120417           IND     = DatiXStampa(wX).IND;
021200120417           CAD     = DatiXStampa(wX).CAD;
021300120417           LOD     = DatiXStampa(wX).LOD;
021400120417           PRD     = DatiXStampa(wX).PRD;
021500120417           Dt_Sped = DatiXStampa(wX).Dt_Sped;
021600120417           TotNonAtt = DatiXStampa(wX).TotNonAtt;
021700120417
021800120417           // valorizzo il numero spedizione
021900120417           Sped = wX;
022000120417
022100120417           if *in25 = *on;
022200120417             exsr StampaTestata;
022300120417           endif;
022400120416           write VRA03DET;
022500120416         endfor;
022600120207
022700111128       ENDSR;
022800120215
022900111128       //--------------------------------------------------------------
023000120208       // Operazioni finali
023100111128       //--------------------------------------------------------------
023200111128       BEGSR  sr_RoutEnd;
023300111128
023400120208         // - Uscita
023500120413         return;
023600111128
023700111128       ENDSR;
023800111128
023900111128      /end-free
