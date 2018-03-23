000100160212     H*
000200141127     H*
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500940307      *  21           GENERICO OPERAZIONI I/O
000600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000700940224      *  30           SFLDSP
000800940224      * N31           SFLCLR
000900940224      *  31           SFLDSPCTL
001000940224      *  32           SFLNXTCHG
001100940224      *  33           SFLEND
001200940224      *  39           OF PRTF
001300940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001400940608      *  Specificare l'uso dei singoli indicatori
001500940224      *  50 <---> 98  ERRORI SU VIDEO
001600940608      *  Specificare l'uso dei singoli indicatori
001700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001900940224      *  99           INDIC. GENERALE DI ERRORE
002000940128     F*----------------------------------------------------*
002100160212     FazORG01L  IF   E           K DISK
002200160212      *---------
002300120504     FFNARB01L  IF   E           K DISK
002400101215     Ffieu20D   CF   E             WORKSTN
002500101215     F                                     INFDS(DsFmt)
002600160201     FEDIvaB2L  UF   E           K DISK
002700160201     FEDIvaT3L  Uf A E           K DISK
002800940128     D*----------------------------------------------------*
002900940211     D* Passaggio Parametri
003000940211     D KPJBA         E DS
003100110223     D conta_bolle     S              5I 0
003200110223     D sav_w1parcel    S                   like(w1parcel)
003300120502     D ristampa_etichetta...
003400120502     d                 s              1
003500160212     D*------------
003600160212     D* DS REPERIMENTO DATI UTENTE
003700160212     D*------------
003800160212     D TIBS34DS      E DS                                                       *Profili utente
003900160212     D DDATIUTE      E DS                                                       *Dati utente
004000160212     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004100160217     D*------------
004200160212     d fieuLPDds     e ds
004300160212     d  dLPD         e ds
004400160217     D*------------
004500160217     D TRUL06DS      E DS
004600160217     D  LIN                    1     90  0 DIM(30)
004700160129     D*-------------
004800160129     d  Sosp_AAS       s                   like(VATAAS)
004900160129     d  Sosp_LNP       s                   like(VATLNP)
005000160129     d  Sosp_NRS       s                   like(VATNRS)
005100160129     d  Sosp_NSP       s                   like(VATNSP)
005200160129     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
005300160201     D*-------------
005400160201     D W0140           S             14  0
005500160201     D WDTGIO          S              8  0
005600160201     D DATEU           S              8  0
005700160201     D DATA_eur        S               D   DATFMT(*eur)
005800160201     D DATA_iso        S               D   DATFMT(*iso)
005900120502     D*-------------
006000120502     D PARAM76         DS                  INZ
006100120502     D parmLNP                 1      3  0
006200120502     D parmLNA                 4      6  0
006300120502     D parmDSD                 7     14  0
006400120502     D parmDSA                15     22  0
006500120502     D*
006600120502     D parmLPD                23     25  0
006700120502     D parmSED                26     27  0
006800120502     D parmNSD                28     34  0
006900120502     D*
007000120502     D parmSEA                35     36  0
007100120502     D parmNSA                37     43  0
007200120502     D parmAAS                44     47  0
007300120502     D*
007400120502     D parsSOP                48     48
007500120502     D parsLNA                49     51  0
007600120502     D parsFLS                52     54  0
007700120502     D parsSCD                55     61  0
007800120502     D parsSCA                62     68  0
007900120502     D parsKSC                69     75  0
008000120502     D*
008100120502     D parsCPR                76     82  0
008200120502     D parsFPP                83     83
008300120502     D parsNRT                84     84
008400120502     D parsBAM                85     85
008500120502     D parsSNP                86     86
008600120502     d parsvcli               87     87
008700120502     D*
008800120502     d  wD90psl              157    166
008900120502     d* · stampante per LDV in formato "A4" (laser)
009000120502     d  wD90prb4             167    176
009100120502     d* · stampante per LDV in formato "A5" (laser)
009200120502     d  wD90prb5             177    186
009300120502     d* · modulo per stampa LDV su "A4"     (laser)
009400120502     d  wD90mdb4             187    196
009500120502     d* · modulo per stampa LDV su "A5"     (laser)
009600120502     d  wD90mdb5             197    206
009700120502     d* · pgm di stampa etichette segnacolli
009800120502     d  wD90pss              207    216
009900120502     d* · modulo per stampa etichette
010000120502     d  wD90mde              227    236
010100120502     d* · stampante per etichette
010200120502     d  wD90pre              247    256
010300940211     D*-------------
010400120502     D* DS PER TRUL90R - RICHIESTA STAMPANTI
010500120502     D TRUL90DS      E DS                  INZ
010600120502     D*-------------
010700160212     D OG143         E DS
010800160212     D*-------------
010900940211     D DSFMT           DS           512
011000940506     D  $TASTO               369    369
011100940211     D  NRG                  370    370
011200940211     D  NCL                  371    371
011300940211     D  SFLNRR               378    379B 0
011400940207     D*-------------
011500940207     D* Nome PGM a video
011600940207     D                 DS
011700940207     D  PROGR                  1     10
011800940207     D  ASTER1                 1      1    INZ('*')
011900940207     D  SIGLA                  2      9
012000940207     D  ASTER2                10     10    INZ('*')
012100940127     D*-------------
012200940127     D* Reperimento nome PGM
012300940127     D STATUS         SDS           333
012400940127     D  DSPGM            *PROC
012500060927     D* descrizione tabella
012600030113     D*-------------
012700940211     D* COSTANTI
012800940211     D*-------------
012900940506     D* Tasti di funzione
013000940506     D F01             C                   CONST(X'31')
013100940506     D F02             C                   CONST(X'32')
013200940506     D F03             C                   CONST(X'33')
013300940506     D F04             C                   CONST(X'34')
013400940506     D F05             C                   CONST(X'35')
013500940506     D F06             C                   CONST(X'36')
013600940506     D F07             C                   CONST(X'37')
013700940506     D F08             C                   CONST(X'38')
013800940506     D F09             C                   CONST(X'39')
013900940506     D F10             C                   CONST(X'3A')
014000940506     D F11             C                   CONST(X'3B')
014100940506     D F12             C                   CONST(X'3C')
014200940506     D F13             C                   CONST(X'B1')
014300940506     D F14             C                   CONST(X'B2')
014400940506     D F15             C                   CONST(X'B3')
014500940506     D F16             C                   CONST(X'B4')
014600940506     D F17             C                   CONST(X'B5')
014700940506     D F18             C                   CONST(X'B6')
014800940506     D F19             C                   CONST(X'B7')
014900940506     D F20             C                   CONST(X'B8')
015000940506     D F21             C                   CONST(X'B9')
015100940506     D F22             C                   CONST(X'BA')
015200940506     D F23             C                   CONST(X'BB')
015300940506     D F24             C                   CONST(X'BC')
015400940506     D ENTER           C                   CONST(X'F1')
015500940506     D ROLDWN          C                   CONST(X'F4')
015600940506     D ROLLUP          C                   CONST(X'F5')
015700940127     C*----------------------------------------------------*
015800940127     C*                MAIN LINE PROGRAM
015900940127     C*----------------------------------------------------*
016000120504     C     KbolW1        KLIST
016100120504     C                   KFLD                    W1AAS
016200120504     C                   KFLD                    W1LNP
016300120504     C                   KFLD                    W1NRS
016400120504     C                   KFLD                    W1NSP
016500160129      *
016600160129     C     KSosp_EDIVA   KLIST
016700160129     C                   KFLD                    Sosp_AAS
016800160129     C                   KFLD                    Sosp_LNP
016900160129     C                   KFLD                    Sosp_NRS
017000160129     C                   KFLD                    Sosp_NSP
017100120504      *
017200940223     C* inizializzazione variabili
017300940223     C                   EXSR      INZVAR
017400940223     C*
017500940223     C     $FINE         DOWEQ     *OFF
017600101215     C     $GEST         CASEQ     'D1'          GESD1
017700940117     C                   END
017800101215     C                   END
017900101215     C* fine programma
018000101215     C                   SETON                                            LR
018100030113     C************************************************************
018200030113     C* INIZIALIZZAZIONE VARIABILI
018300030113     C************************************************************
018400030113     C     INZVAR        BEGSR
018500030113     C*
018600030113     C* Pulizia campi e indicatori
018700030113     C                   MOVE      *ALL'0'       IN4049           10
018800030113     C                   MOVEA     IN4049        *IN(40)
018900030113     C* Variabili per gestione videate
019000030113     C*
019100030113     C                   MOVE      *OFF          $FINE
019200030113     C                   MOVE      *OFF          $EFILE
019300030113     C                   MOVE      *OFF          $ESCI
019400030113     C                   MOVE      *OFF          $RCDOK
019500030113     C*
019600101215     C                   MOVE      *ON           $INZD1
019700101215     C                   MOVE      'D1'          $GEST
019800101215     c*
019900030113     C                   ENDSR
020000940127     C************************************************************
020100940131     C* GESTIONE LISTA
020200940127     C************************************************************
020300101215     C     GESD1         BEGSR
020400030113     C*
020500940223     C* inizializzazione videata
020600101215     C     $INZD1        IFEQ      *ON
020700101215     C                   EXSR      INZD1
020800101215     C                   MOVE      *OFF          $INZD1
020900940127     C                   ENDIF
021000030113     C*
021100030113     C* emissione piede videata
021200120502     c                   setoff                                       88
021300120502     C                   if          Ristampa_etichetta = *on
021400120502     c                   seton                                        88
021500120502     c                   end
021600110223     c                   clear                   w1elabora
021700030113     C                   WRITE     Z1
021800030113     C*              *------------------*
021900101215     C                   EXFMT     D1
022000030113     C*              *------------------*
022100110223      **
022200030113     C* Selezioni
0223009401271    C                   SELECT
022400940127     C* F3=Fine
022500940506     C     $TASTO        WHENEQ    F03
022600101215     C                   MOVE      *ON           $FINE
022700120502      **
022800120502     C                   WHEN      $TASTO = F08 and Ristampa_etichetta = *on
022900120502     c                   exsr      exec_prteti
023000120502      **
0231009401271O   C                   OTHER
023200160129      **
023300160129      ** se digitato il BARCODE tramite la pistola
023400160129     c                   if        w1barcode <> *blank
023500160129     c                   eval      w1parcel = %subst(w1barcode:9:14)
023600160129     c                   end
023700110223      **
023800110223     c                   eval       sav_w1parcel = w1parcel
023900110223     c                   clear                   D1
024000110223     c                   eval       w1parcel  =  sav_w1parcel
024100110223     c                   clear                   w1exec
024200110223     c                   eval       w1elabora = '*-In Elaborazione-*'
024300110223     c                   setoff                                       010203
024400110223     c                   setoff                                       040506
024500110223     C                   write     D1
024600940127     C* CONTROLLO DATI
024700101215     C                   EXSR      CTRD1
0248009401271-   C                   ENDSL
024900940127     C*
025000940127     C                   ENDSR
025100940224     C/EJECT
025200940127     C************************************************************
025300940131     C* INIZIALIZZAZIONE LISTA
025400940127     C************************************************************
025500101215     C     INZD1         BEGSR
025600101215     c*
025700101215     c                   clear                   D1
025800110223     c                   setoff                                       010203
025900110223     c                   setoff                                       040506
026000101215     C*
026100940127     C                   ENDSR
026200940309     C/EJECT
026300940131     C************************************************************
026400940131     C* CONTROLLO OPZIONI LISTA
026500940131     C************************************************************
026600101215     C     CTRD1         BEGSR
026700940131     C*
026800101215
026900101215      /free
027000101215
027100120502                ristampa_etichetta = *OFF;
027200120502                w1rieti = *blank;
027300160215                w1istr1 = *blank;
027400160215                w1istr2 = *blank;
027500101215                $esci = *off;
027600101215                *in99 = *off;
027700110223                *in01 = *off;
027800110223                *in02 = *off;
027900110223                *in03 = *off;
028000110223                *in04 = *off;
028100110223                *in05 = *off;
028200101215
028300110223
028400110223               //  se Non immesso il Parcel
028500110223                   IF W1Parcel = *blank ;
028600110223                             *in05 = *on;
028700110223                             leavesr;
028800110223                   ENDIF;
028900110223
029000110223                    eval  w1exec ='Elaborata';
029100110223
029200101215              // 1° test su file CMR
029300101215
029400160201                 EXEC SQL SELECT VATCMR, VATAAS, VATLNP, VATNRS, VATNSP
029500160201                 INTO :W1CMR, :Sosp_AAS, :Sosp_LNP, :Sosp_NRS, :Sosp_NSP
029600101215                     FROM EDIVAT0F WHERE
029700101215                       VATTRC='E'
029800101215                     AND
029900101215                       SUBSTR(VATNOT, 1, 14)= :W1parcel
030000110223                       FETCH FIRST 1 ROW ONLY
030100101215                  ;
030200101215
030300101215                  IF sqlCode < 0;
030400101215                    DUMP(A);
030500101215                  ENDIF;
030600101215
030700101215                  IF sqlCode = 0;
030800160218                        If  W1CMR <> *blank;
030900160218                            If  W1CMR = GEO_in_SOSPESO or
031000160218                                %subst(W1CMR:1:3) = 'GEO';
031100160129                                exsr  ABILITA_CMR;
031200160218                            endif;
031300110223                            *in01 = *on ;
031400101215                            leavesr;
031500101215                        endif;
031600101215                  ENDIF;
031700101215
031800101215
031900101215              // 2° test su file CLIENTE
032000101215
032100101215                 EXEC SQL SELECT VATCCM
032200101215                            INTO :W1CCM
032300101215                          FROM FIVAT00F WHERE
032400101215                            VATTRC='E'
032500101215                           AND
032600101215                             SUBSTR(VATNOT, 1, 14)= :W1parcel
032700110223                       FETCH FIRST 1 ROW ONLY
032800101215                   ;
032900101215
033000101215                   IF sqlCode < 0;
033100101215                     DUMP(A);
033200101215                   ENDIF;
033300101215
033400101215                   IF sqlCode = 0;
033500101215                         If  W1CCM <> *zeros;
033600110223                             *in02 = *on ;
033700101215                             leavesr;
033800101215                         endif;
033900101215                   ENDIF;
034000101215
034100101215
034200101215               // 3° test su file BOLLE
034300101215
034400171020              //  EXEC SQL SELECT AR4AAS, AR4LNP, AR4NRS, AR4NSP
034500171020              //      INTO :W1AAS, :W1LNP, :W1NRS, :W1NSP
034600171020              //      FROM FIAR400F WHERE
034700171020              //        AR4TRC='I'
034800171020              //      AND
034900171020              //        SUBSTR(AR4NOT, 1, 14)= :W1parcel
035000171020              //       FETCH FIRST 1 ROW ONLY
035100171020              //   ;
035200171020
035300171020                  EXEC SQL SELECT pndAAS, pndLNP, pndNRS, pndNSP
035400171020                      INTO :W1AAS, :W1LNP, :W1NRS, :W1NSP
035500171020                      FROM FIPND00F WHERE PNDIPN = :W1parcel
035600171020                   FETCH FIRST 1 ROW ONLY
035700171020                   ;
035800171020
035900101215                   IF sqlCode < 0;
036000101215                     DUMP(A);
036100101215                   ENDIF;
036200101215
036300101215                   IF sqlCode = 0;
036400101215                             W1bolla =
036500101215                               %editc(W1AAS:'X') + ' ' +
036600101215                               %editc(W1LNP:'X') + ' ' +
036700101215                               %editc(W1NRS:'X') + ' ' +
036800120504                               %editc(W1NSP:'X') + ' '   ;
036900120504
037000120504           // NON DEVE ESSERE GIA' IN ARRIVO
037100120504           // ALTRIMENTI NON DEVE STAMPARE
037200120504
037300120504                       Setll KbolW1 Fnarb000;
037400120504
037500120504           // Abilita solo se non è in arrivo
037600120504                        if not %Equal;
037700120502                           ristampa_etichetta = *ON;
037800120502                           W1rieti = 'F8 -> ristampa Etichetta';
037900120504                        endIF;
038000110223
038100110223               // 4° controlla se ci sono dei Legami con altra bolla
038200110223
038300171020             //   EXEC SQL SELECT count(*)
038400171020             //       INTO :conta_bolle
038500171020             //       FROM FIAR400F WHERE
038600171020             //         AR4TRC='I'
038700171020             //       AND
038800171020             //         SUBSTR(AR4NOT, 1, 14)= :W1parcel
038900171020             //        FETCH FIRST 1 ROW ONLY
039000171020             //    ;
039100110223
039200171020                  EXEC SQL SELECT count(*)
039300171020                      INTO :conta_bolle
039400171020                      FROM FIPND00F WHERE PNDIPN = :W1parcel
039500171020                       FETCH FIRST 1 ROW ONLY
039600171020                   ;
039700171020
039800110223               //  se presenti più bolle con stesso Parcel sono bolle legate
039900110223                      clear W1Legame;
040000110223                   IF sqlCode = 0 and conta_bolle > 1 ;
040100110223                      eval W1Legame = 'con bolla legata';
040200110223                   ENDIF;
040300110223
040400110223                             *in03 = *on ;
040500101215                             leavesr;
040600101215                   ENDIF;
040700110223
040800110223
040900101215
041000101215
041100101215
041200101215              // 4° NON PRESENTE nel SISTEMA
041300101215
041400110223                  *in04 = *on ;
041500101215                  leavesr;
041600101215
041700101215      /end-free
041800101215
041900101215     C*
042000940127     C                   ENDSR
042100940127     C/EJECT
042200940131     C************************************************************
042300120502     C* Riesegue la stampa dell'etichetta
042400940131     C************************************************************
042500120502     C     exec_prteti   BEGSR
042600120502     C*  per identificare la coda x la stampante e il tipo dicumento
042700120502     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
042800120502     C                   reset                   TRUL90DS
042900120502     C* RICHIESTA STAMPANTI
043000120502     C                   MOVEL     'S'           d90RSE
043100120502     C                   MOVEL     'S'           d90RSB
043200120502     C                   CALL      'TRUL90R'
043300120502     C                   PARM                    KPJBA
043400120502     C                   PARM                    trul90Ds
043500120504     C*
043600120504     c                   if        D90F3 = '1'
043700120504     c                   leavesr
043800120504     c                   end
043900120502     C*
044000120502      * Parametri x ovrprtf
044100120502      * · stampante per etichette
044200120502     c                   movel     D90pre        wD90pre
044300120502      * · modulo per stampa etichette
044400120502     c                   movel     D90mde        wD90mde
044500120502      * · pgm. di stampa etichette segnacolli
044600120502     c                   movel     D90pss        wD90pss
044700120502      * · stampante per LDV in formato "A4" - laser
044800120502     c                   movel     D90prb4       wD90prb4
044900120502      * · stampante per LDV in formato "A5" - laser
045000120502     c                   movel     D90prb5       wD90prb5
045100120502      * · modulo per stampa LDV su "A4" ... - laser
045200120502     c                   movel     D90mdb4       wD90mdb4
045300120502      * · modulo per stampa LDV su "A5" ... - laser
045400120502     c                   movel     D90mdb5       wD90mdb5
045500120502      * · pgm.   per stampa LDV ........... - ad aghi o laser
045600120502     c                   movel     D90psl        wD90psl
045700120502     C*
045800120502     c                   movel     w1aas         parmaas
045900120502     c                   movel     w1lnp         parmlpd
046000120502     c                   movel     w1nrs         parmsed
046100120502     c                   movel     w1nsp         parmnsd
046200120502     c                   clear                   kpjbu
046300120502     c                   eval        kpjbu = param76
046400120502     C                   MOVEL     PARAM76       KPJBU
046500120504     C                   call      'FNLS76C'
046600120502     C                   PARM                    KPJBA
046700120502      **
046800120502     C                   ENDSR
046900120502     C/EJECT
047000120502     C************************************************************
047100160211     C* ABILITA x CMR immediatamente la SPEDIZIONE alla CONFERMA
047200120502     C************************************************************
047300160129     C     ABILITA_CMR   BEGSR
047400160129      *
047500160212      *  Deve aggiornare anche la LINEA e la FILIALE di GESTIONE della bolla
047600160212      *  da confermare con la Pistola.
047700160212      *
047800160129     c                   clear                   new_CMR          35
047900160201     C                   TIME                    W0140
048000160201     C                   MOVE      W0140         WDTGIO
048100160201      * UDATE IN AAAAMMGG
048200160201     C     *eur          MOVEL     WDTGIO        DATA_eur
048300160201     c                   move      DATA_eur      DATA_iso
048400160201     c                   move      DATA_iso      data_iso8         8 0
048500160201      *
048600160211     c                   eval      new_CMR ='GEO_' +
048700160211     c                                %editw(data_iso8:'    /  /  ')
048800160211     c                             + '_PST'
048900160129     c                   eval      W1CMR   = new_CMR
049000160129      *
049100160211      *  per chiave bolla aggiorna la bolla in SOSPENZIONE
049200160129     C     KSosp_EDIVA   setll     EDIVAT3L
049300160129     C     KSosp_EDIVA   reade     EDIVAT3L
049400160129     c                   DOw       not %EoF(EDIVAT3L)
049500160212     c                   eval      vatCMR =  new_CMR
049600160217      *-
049700160217      *  cerca la LINEA FRA QUELLE presenti
049800160217     c     vatLNP        lookup    LIN                                    25
049900160219     c                   if        not *in25 and sav_linea <> *zero
050000160212     c                   eval      vatLNP =  sav_linea
050100160212     c                   eval      vatFGS =  sav_linea
050200160212     c                   eval      vatCCM =  sav_cliente
050300160217     c                   end
050400160217      *-
050500160129     c                   Update    EDIVAT00
050600160129     C     KSosp_EDIVA   reade     EDIVAT3L
050700160129     c                   EndDO
050800160129      *
050900160129      *  poi allinea anche la testata bolla del VAB
051000160129     C     KSosp_EDIVA   chain     EDIVAB2L
051100160129     c                   if        %Found(EDIVAB2L)
051200160210      *** Provenienza  x distinguere i records dall'IGATEAVIS
051300160210     c                   movel(P)  'P'           VABscl
051400160212     c                   eval      vabCMR =  new_CMR
051500160217      *-
051600160217      *  cerca la LINEA FRA QUELLE presenti
051700160217     c     vabLNP        lookup    LIN                                    25
051800160219     c                   if        not *in25 and sav_linea <> *zero
051900160212     c                   eval      vabLNP =  sav_linea
052000160212     c                   eval      vabFGS =  sav_linea
052100160212     c                   eval      vabCCM =  sav_cliente
052200160217     c                   end
052300160217      *-
052400160215     c                   eval      w1istr1=  'Accedere nuovamente al programma '
052500160215     c                             + ' di Conferma per CMR'
052600160215     c                   eval      w1istr2=  'e Confermare questa spedizione.'
052700160201     c                   Update    EDIVAB00
052800160129     c                   End
052900160129      *
053000160129     c                   Endsr
053100160129     C************************************************************
053200160129     C* OPERAZIONI INIZIALI
053300160129     C************************************************************
053400160129     C     *INZSR        BEGSR
053500160129     C*
053600030113     C* Reperimento parametri
053700030113     C     *ENTRY        PLIST
053800030113     C                   PARM                    KPJBA
053900030113     C*
054000030113     C* Variabili per gestione videate
054100030113     C                   MOVE      *BLANK        $GEST             2
054200030113     C                   MOVE      *BLANK        $FINE             1
054300101215     C                   MOVE      *BLANK        $INZD1            1
054400030113     C                   MOVE      *BLANK        $EFILE            1
054500030113     C                   MOVE      *BLANK        $ESCI             1
054600030113     C                   MOVE      *BLANK        $RCDOK            1
054700030113     C* Indici
054800030113     C                   Z-ADD     0             X                 3 0
054900030113     C                   Z-ADD     0             Y                 3 0
055000160212     C*
055100160212     C* REPERIMENTO DATI UTENTE
055200160212     C                   EXSR      REPDATIUTE
055300940127     C*
055400160212     c                   clear                   sav_linea         3 0
055500160212     c                   clear                   sav_cliente       7 0
055600160212     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
055700160212     c     DUTPOU        chain     azORG01l
055800160212     c                   if           %Found(azORG01l)
055900160212     c                   movel     orgde3        OG143
056000160219     c                   if           §OGLNPB <> *blank and §OGLNPB <> *zero
056100160212     c                   move      §OGLNPB       sav_linea
056200160212     c                   clear                   FIEULPDDS
056300160212     c                   z-add     sav_linea     ILPDLNP
056400160212     c                   z-add     sav_linea     ILPDLNA
056500160212     c*   il FIEULPDR
056600160212     c                   call      'FIEULPDR'
056700160212     c                   parm                    FIEULPDDS
056800160212     c                   eval      dLPD = oLPDUNI
056900160212     c                   move      §LPDksc       sav_cliente       7 0
057000160217      *--------
057100160217      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
057200160217     C                   CLEAR                   TRUL06DS
057300160217     C                   MOVE      '£1'          D06COD
057400160217     C                   MOVEL     dutPOU        D06KEY
057500160217     C                   MOVEL     'L'           D06TLA
057600160217     C                   MOVEL     TRUL06DS      KPJBU
057700160217     C                   CALL      'TRUL06R'
057800160217     C                   PARM                    KPJBA
057900160217     c                   MOVEL     KPJBU         TRUL06DS
058000160212      **
058100160212     c                   endif
058200160212     c                   endif
058300160212     C*
058400940117     C                   ENDSR
058500160212     C*--------------------------------------------------------------------------------------------*
058600160212     C* REPERISCE I DATI UTENTE
058700160212     C*--------------------------------------------------------------------------------------------*
058800160212     C     REPDATIUTE    BEGSR
058900160212     C*
059000160212     C* INIZIALIZZA VARIABILI DI WRK
059100160212     C                   CLEAR                   TIBS34DS
059200160212     C                   CLEAR                   AZUTEDS
059300160212     C                   CLEAR                   DDATIUTE
059400160212     C*
059500160212     C     *DTAARA       DEFINE    §azute        azuteds
059600160212     C     *DTAARA       DEFINE    §datiute      ddatiute
059700160212     C                   IN(E)     *DTAARA
059800160212     C                   IF        %Error
059900160212     C                   EVAL      I34Tla = 'L'
060000160212     C                   CALL      'TIBS34R'
060100160212     C                   PARM                    Tibs34Ds
060200160212     C                   IN        *DTAARA
060300160212     C                   ENDIF
060400160212     C*
060500160212     C                   ENDSR
060600030113     C************************************************************
