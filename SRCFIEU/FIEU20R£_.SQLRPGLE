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
002001160212     FazORG01L  IF   E           K DISK
002002160212      *---------
002100120504     FFNARB01L  IF   E           K DISK
002200101215     Ffieu20D   CF   E             WORKSTN
002300101215     F                                     INFDS(DsFmt)
002400160201     FEDIvaB2L  UF   E           K DISK
002500160201     FEDIvaT3L  Uf A E           K DISK
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900110223     D conta_bolle     S              5I 0
003000110223     D sav_w1parcel    S                   like(w1parcel)
003100120502     D ristampa_etichetta...
003200120502     d                 s              1
003201160212     D*------------
003202160212     D* DS REPERIMENTO DATI UTENTE
003203160212     D*------------
003204160212     D TIBS34DS      E DS                                                       *Profili utente
003205160212     D DDATIUTE      E DS                                                       *Dati utente
003206160212     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003207160217     D*------------
003208160212     d fieuLPDds     e ds
003209160212     d  dLPD         e ds
003210160217     D*------------
003211160217     D TRUL06DS      E DS
003212160217     D  LIN                    1     90  0 DIM(30)
003300160129     D*-------------
003400160129     d  Sosp_AAS       s                   like(VATAAS)
003500160129     d  Sosp_LNP       s                   like(VATLNP)
003600160129     d  Sosp_NRS       s                   like(VATNRS)
003700160129     d  Sosp_NSP       s                   like(VATNSP)
003800160129     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
003900160201     D*-------------
004000160201     D W0140           S             14  0
004100160201     D WDTGIO          S              8  0
004200160201     D DATEU           S              8  0
004300160201     D DATA_eur        S               D   DATFMT(*eur)
004400160201     D DATA_iso        S               D   DATFMT(*iso)
004500120502     D*-------------
004600120502     D PARAM76         DS                  INZ
004700120502     D parmLNP                 1      3  0
004800120502     D parmLNA                 4      6  0
004900120502     D parmDSD                 7     14  0
005000120502     D parmDSA                15     22  0
005100120502     D*
005200120502     D parmLPD                23     25  0
005300120502     D parmSED                26     27  0
005400120502     D parmNSD                28     34  0
005500120502     D*
005600120502     D parmSEA                35     36  0
005700120502     D parmNSA                37     43  0
005800120502     D parmAAS                44     47  0
005900120502     D*
006000120502     D parsSOP                48     48
006100120502     D parsLNA                49     51  0
006200120502     D parsFLS                52     54  0
006300120502     D parsSCD                55     61  0
006400120502     D parsSCA                62     68  0
006500120502     D parsKSC                69     75  0
006600120502     D*
006700120502     D parsCPR                76     82  0
006800120502     D parsFPP                83     83
006900120502     D parsNRT                84     84
007000120502     D parsBAM                85     85
007100120502     D parsSNP                86     86
007200120502     d parsvcli               87     87
007300120502     D*
007400120502     d  wD90psl              157    166
007500120502     d* · stampante per LDV in formato "A4" (laser)
007600120502     d  wD90prb4             167    176
007700120502     d* · stampante per LDV in formato "A5" (laser)
007800120502     d  wD90prb5             177    186
007900120502     d* · modulo per stampa LDV su "A4"     (laser)
008000120502     d  wD90mdb4             187    196
008100120502     d* · modulo per stampa LDV su "A5"     (laser)
008200120502     d  wD90mdb5             197    206
008300120502     d* · pgm di stampa etichette segnacolli
008400120502     d  wD90pss              207    216
008500120502     d* · modulo per stampa etichette
008600120502     d  wD90mde              227    236
008700120502     d* · stampante per etichette
008800120502     d  wD90pre              247    256
008900940211     D*-------------
009000120502     D* DS PER TRUL90R - RICHIESTA STAMPANTI
009100120502     D TRUL90DS      E DS                  INZ
009300120502     D*-------------
009301160212     D OG143         E DS
009302160212     D*-------------
009400940211     D DSFMT           DS           512
009500940506     D  $TASTO               369    369
009600940211     D  NRG                  370    370
009700940211     D  NCL                  371    371
009800940211     D  SFLNRR               378    379B 0
009900940207     D*-------------
010000940207     D* Nome PGM a video
010100940207     D                 DS
010200940207     D  PROGR                  1     10
010300940207     D  ASTER1                 1      1    INZ('*')
010400940207     D  SIGLA                  2      9
010500940207     D  ASTER2                10     10    INZ('*')
010600940127     D*-------------
010700940127     D* Reperimento nome PGM
010800940127     D STATUS         SDS           333
010900940127     D  DSPGM            *PROC
011000060927     D* descrizione tabella
011100030113     D*-------------
011200940211     D* COSTANTI
011300940211     D*-------------
011400940506     D* Tasti di funzione
011500940506     D F01             C                   CONST(X'31')
011600940506     D F02             C                   CONST(X'32')
011700940506     D F03             C                   CONST(X'33')
011800940506     D F04             C                   CONST(X'34')
011900940506     D F05             C                   CONST(X'35')
012000940506     D F06             C                   CONST(X'36')
012100940506     D F07             C                   CONST(X'37')
012200940506     D F08             C                   CONST(X'38')
012300940506     D F09             C                   CONST(X'39')
012400940506     D F10             C                   CONST(X'3A')
012500940506     D F11             C                   CONST(X'3B')
012600940506     D F12             C                   CONST(X'3C')
012700940506     D F13             C                   CONST(X'B1')
012800940506     D F14             C                   CONST(X'B2')
012900940506     D F15             C                   CONST(X'B3')
013000940506     D F16             C                   CONST(X'B4')
013100940506     D F17             C                   CONST(X'B5')
013200940506     D F18             C                   CONST(X'B6')
013300940506     D F19             C                   CONST(X'B7')
013400940506     D F20             C                   CONST(X'B8')
013500940506     D F21             C                   CONST(X'B9')
013600940506     D F22             C                   CONST(X'BA')
013700940506     D F23             C                   CONST(X'BB')
013800940506     D F24             C                   CONST(X'BC')
013900940506     D ENTER           C                   CONST(X'F1')
014000940506     D ROLDWN          C                   CONST(X'F4')
014100940506     D ROLLUP          C                   CONST(X'F5')
014200940127     C*----------------------------------------------------*
014300940127     C*                MAIN LINE PROGRAM
014400940127     C*----------------------------------------------------*
014500120504     C     KbolW1        KLIST
014600120504     C                   KFLD                    W1AAS
014700120504     C                   KFLD                    W1LNP
014800120504     C                   KFLD                    W1NRS
014900120504     C                   KFLD                    W1NSP
015000160129      *
015100160129     C     KSosp_EDIVA   KLIST
015200160129     C                   KFLD                    Sosp_AAS
015300160129     C                   KFLD                    Sosp_LNP
015400160129     C                   KFLD                    Sosp_NRS
015500160129     C                   KFLD                    Sosp_NSP
015600120504      *
015700940223     C* inizializzazione variabili
015800940223     C                   EXSR      INZVAR
015900940223     C*
016000940223     C     $FINE         DOWEQ     *OFF
016100101215     C     $GEST         CASEQ     'D1'          GESD1
016200940117     C                   END
016300101215     C                   END
016400101215     C* fine programma
016500101215     C                   SETON                                            LR
016600030113     C************************************************************
016700030113     C* INIZIALIZZAZIONE VARIABILI
016800030113     C************************************************************
016900030113     C     INZVAR        BEGSR
017000030113     C*
017100030113     C* Pulizia campi e indicatori
017200030113     C                   MOVE      *ALL'0'       IN4049           10
017300030113     C                   MOVEA     IN4049        *IN(40)
017400030113     C* Variabili per gestione videate
017500030113     C*
017600030113     C                   MOVE      *OFF          $FINE
017700030113     C                   MOVE      *OFF          $EFILE
017800030113     C                   MOVE      *OFF          $ESCI
017900030113     C                   MOVE      *OFF          $RCDOK
018000030113     C*
018100101215     C                   MOVE      *ON           $INZD1
018200101215     C                   MOVE      'D1'          $GEST
018300101215     c*
018400030113     C                   ENDSR
018500940127     C************************************************************
018600940131     C* GESTIONE LISTA
018700940127     C************************************************************
018800101215     C     GESD1         BEGSR
018900030113     C*
019000940223     C* inizializzazione videata
019100101215     C     $INZD1        IFEQ      *ON
019200101215     C                   EXSR      INZD1
019300101215     C                   MOVE      *OFF          $INZD1
019400940127     C                   ENDIF
019500030113     C*
019600030113     C* emissione piede videata
019700120502     c                   setoff                                       88
019800120502     C                   if          Ristampa_etichetta = *on
019900120502     c                   seton                                        88
020000120502     c                   end
020100110223     c                   clear                   w1elabora
020200030113     C                   WRITE     Z1
020300030113     C*              *------------------*
020400101215     C                   EXFMT     D1
020500030113     C*              *------------------*
020600110223      **
020700030113     C* Selezioni
0208009401271    C                   SELECT
020900940127     C* F3=Fine
021000940506     C     $TASTO        WHENEQ    F03
021100101215     C                   MOVE      *ON           $FINE
021200120502      **
021300120502     C                   WHEN      $TASTO = F08 and Ristampa_etichetta = *on
021400120502     c                   exsr      exec_prteti
021500120502      **
0216009401271O   C                   OTHER
021700160129      **
021800160129      ** se digitato il BARCODE tramite la pistola
021900160129     c                   if        w1barcode <> *blank
022000160129     c                   eval      w1parcel = %subst(w1barcode:9:14)
022100160129     c                   end
022200110223      **
022300110223     c                   eval       sav_w1parcel = w1parcel
022400110223     c                   clear                   D1
022500110223     c                   eval       w1parcel  =  sav_w1parcel
022600110223     c                   clear                   w1exec
022700110223     c                   eval       w1elabora = '*-In Elaborazione-*'
022800110223     c                   setoff                                       010203
022900110223     c                   setoff                                       040506
023000110223     C                   write     D1
023100940127     C* CONTROLLO DATI
023200101215     C                   EXSR      CTRD1
0233009401271-   C                   ENDSL
023400940127     C*
023500940127     C                   ENDSR
023600940224     C/EJECT
023700940127     C************************************************************
023800940131     C* INIZIALIZZAZIONE LISTA
023900940127     C************************************************************
024000101215     C     INZD1         BEGSR
024100101215     c*
024200101215     c                   clear                   D1
024300110223     c                   setoff                                       010203
024400110223     c                   setoff                                       040506
024500101215     C*
024600940127     C                   ENDSR
024700940309     C/EJECT
024800940131     C************************************************************
024900940131     C* CONTROLLO OPZIONI LISTA
025000940131     C************************************************************
025100101215     C     CTRD1         BEGSR
025200940131     C*
025300101215
025400101215      /free
025500101215
025600120502                ristampa_etichetta = *OFF;
025700120502                w1rieti = *blank;
025701160215                w1istr1 = *blank;
025702160215                w1istr2 = *blank;
025800101215                $esci = *off;
025900101215                *in99 = *off;
026000110223                *in01 = *off;
026100110223                *in02 = *off;
026200110223                *in03 = *off;
026300110223                *in04 = *off;
026400110223                *in05 = *off;
026500101215
026600110223
026700110223               //  se Non immesso il Parcel
026800110223                   IF W1Parcel = *blank ;
026900110223                             *in05 = *on;
027000110223                             leavesr;
027100110223                   ENDIF;
027200110223
027300110223                    eval  w1exec ='Elaborata';
027400110223
027500101215              // 1° test su file CMR
027600101215
027700160201                 EXEC SQL SELECT VATCMR, VATAAS, VATLNP, VATNRS, VATNSP
027800160201                 INTO :W1CMR, :Sosp_AAS, :Sosp_LNP, :Sosp_NRS, :Sosp_NSP
027900101215                     FROM EDIVAT0F WHERE
028000101215                       VATTRC='E'
028100101215                     AND
028200101215                       SUBSTR(VATNOT, 1, 14)= :W1parcel
028300110223                       FETCH FIRST 1 ROW ONLY
028400101215                  ;
028500101215
028600101215                  IF sqlCode < 0;
028700101215                    DUMP(A);
028800101215                  ENDIF;
028900101215
029000101215                  IF sqlCode = 0;
029100160218                        If  W1CMR <> *blank;
029101160218                            If  W1CMR = GEO_in_SOSPESO or
029102160218                                %subst(W1CMR:1:3) = 'GEO';
029300160129                                exsr  ABILITA_CMR;
029301160218                            endif;
029600110223                            *in01 = *on ;
029700101215                            leavesr;
029800101215                        endif;
029900101215                  ENDIF;
030000101215
030100101215
030200101215              // 2° test su file CLIENTE
030300101215
030400101215                 EXEC SQL SELECT VATCCM
030500101215                            INTO :W1CCM
030600101215                          FROM FIVAT00F WHERE
030700101215                            VATTRC='E'
030800101215                           AND
030900101215                             SUBSTR(VATNOT, 1, 14)= :W1parcel
031000110223                       FETCH FIRST 1 ROW ONLY
031100101215                   ;
031200101215
031300101215                   IF sqlCode < 0;
031400101215                     DUMP(A);
031500101215                   ENDIF;
031600101215
031700101215                   IF sqlCode = 0;
031800101215                         If  W1CCM <> *zeros;
031900110223                             *in02 = *on ;
032000101215                             leavesr;
032100101215                         endif;
032200101215                   ENDIF;
032300101215
032400101215
032500101215               // 3° test su file BOLLE
032600101215
032700101215
032800101215                  EXEC SQL SELECT AR4AAS, AR4LNP, AR4NRS, AR4NSP
032900101215                      INTO :W1AAS, :W1LNP, :W1NRS, :W1NSP
033000101215                      FROM FIAR400F WHERE
033100101215                        AR4TRC='I'
033200101215                      AND
033300101215                        SUBSTR(AR4NOT, 1, 14)= :W1parcel
033400110223                       FETCH FIRST 1 ROW ONLY
033500101215                   ;
033600101215
033700101215                   IF sqlCode < 0;
033800101215                     DUMP(A);
033900101215                   ENDIF;
034000101215
034100101215                   IF sqlCode = 0;
034200101215                             W1bolla =
034300101215                               %editc(W1AAS:'X') + ' ' +
034400101215                               %editc(W1LNP:'X') + ' ' +
034500101215                               %editc(W1NRS:'X') + ' ' +
034600120504                               %editc(W1NSP:'X') + ' '   ;
034700120504
034800120504           // NON DEVE ESSERE GIA' IN ARRIVO
034900120504           // ALTRIMENTI NON DEVE STAMPARE
035000120504
035100120504                       Setll KbolW1 Fnarb000;
035200120504
035300120504           // Abilita solo se non è in arrivo
035400120504                        if not %Equal;
035500120502                           ristampa_etichetta = *ON;
035600120502                           W1rieti = 'F8 -> ristampa Etichetta';
035700120504                        endIF;
035800110223
035900110223               // 4° controlla se ci sono dei Legami con altra bolla
036000110223
036100110223                  EXEC SQL SELECT count(*)
036200110223                      INTO :conta_bolle
036300110223                      FROM FIAR400F WHERE
036400110223                        AR4TRC='I'
036500110223                      AND
036600110223                        SUBSTR(AR4NOT, 1, 14)= :W1parcel
036700110223                       FETCH FIRST 1 ROW ONLY
036800110223                   ;
036900110223
037000110223               //  se presenti più bolle con stesso Parcel sono bolle legate
037100110223                      clear W1Legame;
037200110223                   IF sqlCode = 0 and conta_bolle > 1 ;
037300110223                      eval W1Legame = 'con bolla legata';
037400110223                   ENDIF;
037500110223
037600110223                             *in03 = *on ;
037700101215                             leavesr;
037800101215                   ENDIF;
037900110223
038000110223
038100101215
038200101215
038300101215
038400101215              // 4° NON PRESENTE nel SISTEMA
038500101215
038600110223                  *in04 = *on ;
038700101215                  leavesr;
038800101215
038900101215      /end-free
039000101215
039100101215     C*
039200940127     C                   ENDSR
039300940127     C/EJECT
039400940131     C************************************************************
039500120502     C* Riesegue la stampa dell'etichetta
039600940131     C************************************************************
039700120502     C     exec_prteti   BEGSR
039800120502     C*  per identificare la coda x la stampante e il tipo dicumento
039900120502     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
040000120502     C                   reset                   TRUL90DS
040100120502     C* RICHIESTA STAMPANTI
040200120502     C                   MOVEL     'S'           d90RSE
040300120502     C                   MOVEL     'S'           d90RSB
040400120502     C                   CALL      'TRUL90R'
040500120502     C                   PARM                    KPJBA
040600120502     C                   PARM                    trul90Ds
040700120504     C*
040800120504     c                   if        D90F3 = '1'
040900120504     c                   leavesr
041000120504     c                   end
041100120502     C*
041200120502      * Parametri x ovrprtf
041300120502      * · stampante per etichette
041400120502     c                   movel     D90pre        wD90pre
041500120502      * · modulo per stampa etichette
041600120502     c                   movel     D90mde        wD90mde
041700120502      * · pgm. di stampa etichette segnacolli
041800120502     c                   movel     D90pss        wD90pss
041900120502      * · stampante per LDV in formato "A4" - laser
042000120502     c                   movel     D90prb4       wD90prb4
042100120502      * · stampante per LDV in formato "A5" - laser
042200120502     c                   movel     D90prb5       wD90prb5
042300120502      * · modulo per stampa LDV su "A4" ... - laser
042400120502     c                   movel     D90mdb4       wD90mdb4
042500120502      * · modulo per stampa LDV su "A5" ... - laser
042600120502     c                   movel     D90mdb5       wD90mdb5
042700120502      * · pgm.   per stampa LDV ........... - ad aghi o laser
042800120502     c                   movel     D90psl        wD90psl
042900120502     C*
043000120502     c                   movel     w1aas         parmaas
043100120502     c                   movel     w1lnp         parmlpd
043200120502     c                   movel     w1nrs         parmsed
043300120502     c                   movel     w1nsp         parmnsd
043400120502     c                   clear                   kpjbu
043500120502     c                   eval        kpjbu = param76
043600120502     C                   MOVEL     PARAM76       KPJBU
043700120504     C                   call      'FNLS76C'
043800120502     C                   PARM                    KPJBA
043900120502      **
044000120502     C                   ENDSR
044100120502     C/EJECT
044200120502     C************************************************************
044300160211     C* ABILITA x CMR immediatamente la SPEDIZIONE alla CONFERMA
044400120502     C************************************************************
044500160129     C     ABILITA_CMR   BEGSR
044600160129      *
044601160212      *  Deve aggiornare anche la LINEA e la FILIALE di GESTIONE della bolla
044602160212      *  da confermare con la Pistola.
044603160212      *
044700160129     c                   clear                   new_CMR          35
044900160201     C                   TIME                    W0140
045100160201     C                   MOVE      W0140         WDTGIO
045200160201      * UDATE IN AAAAMMGG
045300160201     C     *eur          MOVEL     WDTGIO        DATA_eur
045400160201     c                   move      DATA_eur      DATA_iso
045500160201     c                   move      DATA_iso      data_iso8         8 0
045600160201      *
045700160211     c                   eval      new_CMR ='GEO_' +
045800160211     c                                %editw(data_iso8:'    /  /  ')
045801160211     c                             + '_PST'
045900160129     c                   eval      W1CMR   = new_CMR
046000160129      *
046100160211      *  per chiave bolla aggiorna la bolla in SOSPENZIONE
046300160129     C     KSosp_EDIVA   setll     EDIVAT3L
046400160129     C     KSosp_EDIVA   reade     EDIVAT3L
046500160129     c                   DOw       not %EoF(EDIVAT3L)
046600160212     c                   eval      vatCMR =  new_CMR
046601160217      *-
046602160217      *  cerca la LINEA FRA QUELLE presenti
046603160217     c     vatLNP        lookup    LIN                                    25
046604160219     c                   if        not *in25 and sav_linea <> *zero
046605160212     c                   eval      vatLNP =  sav_linea
046606160212     c                   eval      vatFGS =  sav_linea
046607160212     c                   eval      vatCCM =  sav_cliente
046608160217     c                   end
046609160217      *-
046700160129     c                   Update    EDIVAT00
046800160129     C     KSosp_EDIVA   reade     EDIVAT3L
046900160129     c                   EndDO
047000160129      *
047100160129      *  poi allinea anche la testata bolla del VAB
047200160129     C     KSosp_EDIVA   chain     EDIVAB2L
047300160129     c                   if        %Found(EDIVAB2L)
047301160210      *** Provenienza  x distinguere i records dall'IGATEAVIS
047302160210     c                   movel(P)  'P'           VABscl
047400160212     c                   eval      vabCMR =  new_CMR
047401160217      *-
047402160217      *  cerca la LINEA FRA QUELLE presenti
047403160217     c     vabLNP        lookup    LIN                                    25
047404160219     c                   if        not *in25 and sav_linea <> *zero
047405160212     c                   eval      vabLNP =  sav_linea
047406160212     c                   eval      vabFGS =  sav_linea
047407160212     c                   eval      vabCCM =  sav_cliente
047408160217     c                   end
047409160217      *-
047410160215     c                   eval      w1istr1=  'Accedere nuovamente al programma '
047411160215     c                             + ' di Conferma per CMR'
047412160215     c                   eval      w1istr2=  'e Confermare questa spedizione.'
047500160201     c                   Update    EDIVAB00
047600160129     c                   End
047700160129      *
047800160129     c                   Endsr
047900160129     C************************************************************
048000160129     C* OPERAZIONI INIZIALI
048100160129     C************************************************************
048200160129     C     *INZSR        BEGSR
048300160129     C*
048400030113     C* Reperimento parametri
048500030113     C     *ENTRY        PLIST
048600030113     C                   PARM                    KPJBA
048700030113     C*
048800030113     C* Variabili per gestione videate
048900030113     C                   MOVE      *BLANK        $GEST             2
049000030113     C                   MOVE      *BLANK        $FINE             1
049100101215     C                   MOVE      *BLANK        $INZD1            1
049200030113     C                   MOVE      *BLANK        $EFILE            1
049300030113     C                   MOVE      *BLANK        $ESCI             1
049400030113     C                   MOVE      *BLANK        $RCDOK            1
049500030113     C* Indici
049600030113     C                   Z-ADD     0             X                 3 0
049700030113     C                   Z-ADD     0             Y                 3 0
049701160212     C*
049702160212     C* REPERIMENTO DATI UTENTE
049703160212     C                   EXSR      REPDATIUTE
049800940127     C*
049801160212     c                   clear                   sav_linea         3 0
049802160212     c                   clear                   sav_cliente       7 0
049803160212     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
049804160212     c     DUTPOU        chain     azORG01l
049805160212     c                   if           %Found(azORG01l)
049806160212     c                   movel     orgde3        OG143
049807160219     c                   if           §OGLNPB <> *blank and §OGLNPB <> *zero
049808160212     c                   move      §OGLNPB       sav_linea
049809160212     c                   clear                   FIEULPDDS
049810160212     c                   z-add     sav_linea     ILPDLNP
049811160212     c                   z-add     sav_linea     ILPDLNA
049813160212     c*   il FIEULPDR
049814160212     c                   call      'FIEULPDR'
049815160212     c                   parm                    FIEULPDDS
049817160212     c                   eval      dLPD = oLPDUNI
049819160212     c                   move      §LPDksc       sav_cliente       7 0
049820160217      *--------
049821160217      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
049822160217     C                   CLEAR                   TRUL06DS
049823160217     C                   MOVE      '£1'          D06COD
049824160217     C                   MOVEL     dutPOU        D06KEY
049825160217     C                   MOVEL     'L'           D06TLA
049826160217     C                   MOVEL     TRUL06DS      KPJBU
049827160217     C                   CALL      'TRUL06R'
049828160217     C                   PARM                    KPJBA
049829160217     c                   MOVEL     KPJBU         TRUL06DS
049831160212      **
049832160212     c                   endif
049833160212     c                   endif
049834160212     C*
049900940117     C                   ENDSR
049901160212     C*--------------------------------------------------------------------------------------------*
049902160212     C* REPERISCE I DATI UTENTE
049903160212     C*--------------------------------------------------------------------------------------------*
049904160212     C     REPDATIUTE    BEGSR
049905160212     C*
049906160212     C* INIZIALIZZA VARIABILI DI WRK
049907160212     C                   CLEAR                   TIBS34DS
049908160212     C                   CLEAR                   AZUTEDS
049909160212     C                   CLEAR                   DDATIUTE
049910160212     C*
049911160212     C     *DTAARA       DEFINE    §azute        azuteds
049912160212     C     *DTAARA       DEFINE    §datiute      ddatiute
049913160212     C                   IN(E)     *DTAARA
049914160212     C                   IF        %Error
049915160212     C                   EVAL      I34Tla = 'L'
049916160212     C                   CALL      'TIBS34R'
049917160212     C                   PARM                    Tibs34Ds
049918160212     C                   IN        *DTAARA
049919160212     C                   ENDIF
049923160212     C*
049924160212     C                   ENDSR
050000030113     C************************************************************
