000100160219      //************************************************************************
000101160219      //
000102160219      // Questo programma, in base ai parametri ricevuti, decide se è possibile
000103160219      // stampare la frase che avvisa il cliente che può pagare anche con carta
000104160219      // di credito.
000105160219      //
000106160219      //************************************************************************
000107160219
000108160219       CTL-OPT ACTGRP(*CALLER) OPTION(*NOUNREF:*NODEBUGIO:*SRCSTMT);
000109160219
000110160219      //************************************************************************
000111160219      //
000112160219      // Costanti.
000113160219      //
000114160219      //************************************************************************
000115160219
000116160222      /COPY GAITRASRC/SRCCONST,TIBS02R
000117160219      /COPY GAITRASRC/SRCCONST,YCOCCSOLR
000118160219
000119160219      //************************************************************************
000120160219      //
000121160219      // Strutture.
000122160219      //
000123160219      //************************************************************************
000127160222
000128160222       DCL-DS kpjba EXT QUALIFIED INZ(*EXTDFT) END-DS;
000130160222       DCL-DS tibs02ds EXT TEMPLATE END-DS;
000137160219
000138160219      //************************************************************************
000139160219      //
000140160219      // Prototipi.
000141160219      //
000142160219      //************************************************************************
000143160219
000144160222      /COPY GAITRASRC/SRCPROTOPR,TIBS02R
000145160222      /COPY GAITRASRC/SRCPROTOPR,YCOCCSOLR
000146160219
000147160219      //************************************************************************
000148160219      //
000149160219      // Parametri.
000150160219      //
000151160219      //************************************************************************
000152160219
000153160219       DCL-PI Ycoccsolr;
000154160219         rqsOpCode INT(10) CONST;
000155160222         rpyOpCode INT(10) OPTIONS(*NOPASS);
000156160219         rqsFormato CHAR(10) CONST OPTIONS(*NOPASS);
000157160222         rqsData CHAR(502) CONST OPTIONS(*NOPASS:*VARSIZE);
000158160219         rqsDataSize INT(10) CONST OPTIONS(*NOPASS);
000159160219         rpyFormato CHAR(10) CONST OPTIONS(*NOPASS);
000160160222         rpyData CHAR(502) OPTIONS(*NOPASS:*VARSIZE);
000161160219         rpyDataSize INT(10) CONST OPTIONS(*NOPASS);
000162160219       END-PI ;
000163160219
000164160219      //************************************************************************
000165160219      //
000166160219      // Main.
000167160219      //
000168160219      //************************************************************************
000189160222
000190160219       SELECT;
000191160219         WHEN rqsOpCode = YCOCCSOL_OPCODE_GETFRASE;
000192160222           rpyOpCode = GetFrase();
000193160222         WHEN rqsOpCode = YCOCCSOL_OPCODE_KPJBA;
000194160222           kpjba = %SUBST(rqsData : 1 : rqsDataSize);
000195160219         WHEN rqsOpCode = YCOCCSOL_OPCODE_SETONLR;
000196160219           *INLR = *ON;
000197160223         OTHER;
000198160223           rpyOpCode = YCOCCSOL_ERRORE_GENERICO;
000199160219       ENDSL;
000200160222
000201160222       RETURN;
000202160219
000203160219       // --------------------------------------------------
000204160219       // Procedure name: GetFrase
000205160219       // Purpose:
000206160219       // Returns:        Esito.
000207160219       // --------------------------------------------------
000208160219
000209160219       DCL-PROC GetFrase ;
000210160219
000211160219         DCL-PI *N INT(10);
000212160219         END-PI ;
000213160222
000214160222         DCL-DS tibs02ds EXT QUALIFIED STATIC;
000215160222           t02mod EXTFLD INZ(TNTBE_MODALITA_CONTROLLO);
000216160222           t02cod EXTFLD INZ('017');
000217160222         END-DS;
000218160222
000219160222         DCL-DS ycoccsoli EXT QUALIFIED INZ(*EXTDFT) STATIC END-DS;
000220160222         DCL-DS ycoccsolo EXT QUALIFIED INZ(*EXTDFT) STATIC END-DS;
000221160222         DCL-S retField INT(10) STATIC;
000222160219
000223160222         RESET retField;
000230160226
000231160222         // Ricezione dei parametri.
000232160222
000233160222         IF rqsFormato = ycoccsoli.formato;
000234160222           ycoccsoli = %SUBST(rqsData : 1 : rqsDataSize);
000235160222         ELSE;
000236160222           RETURN YCOCCSOL_ERRORE_GENERICO;
000237160222         ENDIF;
000238160222
000239160222         IF rpyFormato = ycoccsolo.formato;
000240160222           RESET ycoccsolo;
000241160222         ELSE;
000242160222           RETURN YCOCCSOL_ERRORE_GENERICO;
000243160222         ENDIF;
000244160226
000245160226         IF IsStop();
000246160226           ycoccsolo.stampare = *OFF;
000247160226           %SUBST(rpyData : 1 : rpyDataSize) = ycoccsolo;
000248160226           RETURN retField;
000249160226         ENDIF;
000250160222
000251160222         // Controllo i parametri ricevuti.
000252160222
000253160222         IF ycoccsoli.idStampa = *BLANK;
000254160222           RETURN YCOCCSOL_ERRORE_GENERICO;
000255160222         ENDIF;
000256160222
000257160222         // Devo stampare la frase per questa filiale?
000258160222
000259160223         IF ycoccsoli.idFiliale > *ZERO AND NOT IsTutteLeFiliali();
000260160222
000261160222           RESET tibs02ds;
000262160222           tibs02ds.t02ke1 = '*FILIALE';
000263160223           tibs02ds.t02ke2 = %EDITC(ycoccsoli.idFiliale : 'X');
000264160223
000265160223           TNTBE_RicercaControllo(kpjba : tibs02ds);
000266160222
000267160222           IF tibs02ds.t02err = TNTBE_ESITO_ERRORE
000268160222           OR tibs02ds.t02err = TNTBE_ESITO_ANNULLATO;
000269160222             ycoccsolo.stampare = *OFF;
000270160222             %SUBST(rpyData : 1 : rpyDataSize) = ycoccsolo;
000271160222             RETURN YCOCCSOL_INFO_FILIALE_ESCLUSA;
000272160222           ENDIF;
000273160222
000274160222         ENDIF;
000275160222
000276160222         // Devo stampare la frase per questo importo?
000277160222
000278160222         IF ycoccsoli.impIncassa > *ZERO;
000292160222
000293160223           IF ycoccsoli.impIncassa < GetImportoMinimo()
000294160223           OR ycoccsoli.impIncassa > GetImportoMassimo();
000295160222             ycoccsolo.stampare = *OFF;
000296160222             %SUBST(rpyData : 1 : rpyDataSize) = ycoccsolo;
000297160222             RETURN YCOCCSOL_INFO_IMPORTO_ESCLUSO;
000298160222           ENDIF;
000299160222
000300160222         ENDIF;
000301160222
000302160222         // Devo stampare la frase in questa stampa? Se sì, quale?
000303160222
000304160222         RESET tibs02ds;
000305160222         tibs02ds.t02lin = ycoccsoli.idLingua;
000306160222         tibs02ds.t02ke1 = '*FRASE';
000307160222         tibs02ds.t02ke2 = ycoccsoli.idStampa;
000308160222
000309160222         TNTBE_RicercaControllo(kpjba : tibs02ds);
000310160222
000311160222         IF tibs02ds.t02err = TNTBE_ESITO_ERRORE
000312160222         OR tibs02ds.t02err = TNTBE_ESITO_ANNULLATO;
000313160222           ycoccsolo.stampare = *OFF;
000314160222           %SUBST(rpyData : 1 : rpyDataSize) = ycoccsolo;
000315160222           RETURN YCOCCSOL_INFO_STAMPA_ESCLUSA;
000316160222         ENDIF;
000317160222
000318160222         // Se arrivo qui significa che la frase è da stampare.
000319160222
000320160222         ycoccsolo.stampare = *ON;
000321160222         ycoccsolo.frase = tibs02ds.t02uni;
000322160222         %SUBST(rpyData : 1 : rpyDataSize) = ycoccsolo;
000323160222
000324160222         RETURN retField ;
000325160219
000326160219       END-PROC ;
000327160219
000329160223       // --------------------------------------------------
000330160223       // Procedure name: GetImportoMinimo
000331160223       // Purpose:        Restituisce l'importo minimo.
000332160223       // Returns:        Importo minimo.
000333160223       // --------------------------------------------------
000334160223
000335160223       DCL-PROC GetImportoMinimo ;
000336160223
000337160223         DCL-PI *N PACKED(15:2);
000338160223         END-PI ;
000339160223
000341160223         DCL-S retField PACKED(15:2) INZ(*LOVAL) STATIC;
000342160223         DCL-S isFirstTime IND INZ(*ON) STATIC;
000343160223
000344160223         DCL-DS tibs02ds EXT QUALIFIED STATIC;
000345160223           t02mod EXTFLD INZ(TNTBE_MODALITA_CONTROLLO);
000346160223           t02cod EXTFLD INZ('017');
000347160223           t02ke1 EXTFLD INZ('*IMPORTO');
000348160223           t02ke2 EXTFLD INZ('*MINIMO');
000349160223         END-DS;
000350160223
000351160223         IF NOT isFirstTime;
000352160223           RETURN retField ;
000353160223         ENDIF;
000354160223
000355160223         TNTBE_RicercaControllo(kpjba : tibs02ds);
000356160223
000357160223         IF tibs02ds.t02err = TNTBE_ESITO_OK
000358160223         OR tibs02ds.t02err = TNTBE_ESITO_AVVISO;
000359160223           retField = %DEC(tibs02ds.t02uni : 15 : 2);
000362160223         ENDIF;
000363160223
000364160223         isFirstTime = *OFF;
000365160223
000366160223         RETURN retField ;
000367160223
000368160223       END-PROC ;
000369160223
000371160223       // --------------------------------------------------
000372160223       // Procedure name: GetImportoMassimo
000373160223       // Purpose:        Restituisce l'importo massimo.
000374160223       // Returns:        Importo massimo.
000375160223       // --------------------------------------------------
000376160223
000377160223       DCL-PROC GetImportoMassimo ;
000378160223
000379160223         DCL-PI *N PACKED(15:2);
000380160223         END-PI ;
000381160223
000382160223         DCL-S retField PACKED(15:2) INZ(*HIVAL) STATIC;
000383160223         DCL-S isFirstTime IND INZ(*ON) STATIC;
000384160223
000385160223         DCL-DS tibs02ds EXT QUALIFIED STATIC;
000386160223           t02mod EXTFLD INZ(TNTBE_MODALITA_CONTROLLO);
000387160223           t02cod EXTFLD INZ('017');
000388160223           t02ke1 EXTFLD INZ('*IMPORTO');
000389160223           t02ke2 EXTFLD INZ('*MASSIMO');
000390160223         END-DS;
000391160223
000392160223         IF NOT isFirstTime;
000393160223           RETURN retField ;
000394160223         ENDIF;
000395160223
000396160223         TNTBE_RicercaControllo(kpjba : tibs02ds);
000397160223
000398160223         IF tibs02ds.t02err = TNTBE_ESITO_OK
000399160223         OR tibs02ds.t02err = TNTBE_ESITO_AVVISO;
000400160223           retField = %DEC(tibs02ds.t02uni : 15 : 2);
000401160223         ENDIF;
000402160223
000403160223         isFirstTime = *OFF;
000404160223
000405160223         RETURN retField ;
000406160223
000407160223       END-PROC ;
000408160223
000409160223       // --------------------------------------------------
000410160223       // Procedure name: IsTutteLeFiliali
000411160223       // Purpose:        Restituisce *ON se tutte le filiali sono valide.
000412160223       // Returns:        *ON = tutte le filiali.
000413160223       // --------------------------------------------------
000414160223
000415160223       DCL-PROC IsTutteLeFiliali ;
000416160223
000417160223         DCL-PI *N IND;
000418160223         END-PI ;
000420160223
000421160223         DCL-S retField IND STATIC;
000422160223         DCL-S isFirstTime IND INZ(*ON) STATIC;
000423160223
000424160223         DCL-DS tibs02ds EXT QUALIFIED STATIC;
000425160223           t02mod EXTFLD INZ(TNTBE_MODALITA_CONTROLLO);
000426160223           t02cod EXTFLD INZ('017');
000427160223           t02ke1 EXTFLD INZ('*FILIALE');
000428160223           t02ke2 EXTFLD INZ('*ALL');
000429160223         END-DS;
000430160223
000431160223         IF NOT isFirstTime;
000432160223           RETURN retField ;
000433160223         ENDIF;
000434160223
000435160223         TNTBE_RicercaControllo(kpjba : tibs02ds);
000436160223
000437160223         IF tibs02ds.t02err = TNTBE_ESITO_OK
000438160223         OR tibs02ds.t02err = TNTBE_ESITO_AVVISO;
000439160223           retField = *ON;
000440160223         ENDIF;
000441160223
000442160223         isFirstTime = *OFF;
000443160223
000444160223         RETURN retField ;
000445160223
000446160223       END-PROC ;
000447160226
000448160226       // --------------------------------------------------
000449160226       // Procedure name: IsStop
000450160226       // Purpose:        Restituisce *ON se è fermato.
000451160226       // Returns:        *ON = fermato.
000452160226       // --------------------------------------------------
000453160226
000454160226       DCL-PROC IsStop ;
000455160226
000456160226         DCL-PI *N IND;
000457160226         END-PI ;
000458160226
000459160226         DCL-S retField IND STATIC;
000460160226         DCL-S isFirstTime IND INZ(*ON) STATIC;
000461160226
000462160226         DCL-DS tibs02ds EXT QUALIFIED STATIC;
000463160226           t02mod EXTFLD INZ(TNTBE_MODALITA_CONTROLLO);
000464160226           t02cod EXTFLD INZ('017');
000465160226           t02ke1 EXTFLD INZ('*STOP');
000467160226         END-DS;
000468160226
000469160226         IF NOT isFirstTime;
000470160226           RETURN retField ;
000471160226         ENDIF;
000472160226
000473160226         TNTBE_RicercaControllo(kpjba : tibs02ds);
000474160226
000475160226         IF tibs02ds.t02err = TNTBE_ESITO_OK
000476160226         OR tibs02ds.t02err = TNTBE_ESITO_AVVISO;
000477160226           retField = (%SUBST(tibs02ds.t02uni : 1 : 4) = '*YES');
000478160226         ENDIF;
000479160226
000480160226         isFirstTime = *OFF;
000481160226
000482160226         RETURN retField ;
000483160226
000484160226       END-PROC ;
000485160223
