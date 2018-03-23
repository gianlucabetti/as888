000100130903     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200130903     H BNDDIR('QC2LE')
000300130903     H DECEDIT('0,') DATEDIT(*YMD/)
000400130903      ** ************************************************************************
000500130903      *  Da UPLOAD                                                              *
000600161018      *  TRASCODIFICA : GEODATA / TTEVENTS                                      *
000700161018      *               STATI da DPD su NOSTRO EXPORT                             *
000800130903      ** ************************************************************************
000900150916     Ftivin00r  uF   E             DISK    usropn commit  infds(VINds)
001000161018      *
001100150720      ** -------------------------------------------------------- *
001200150916     d VINds           ds
001300150916     d  nrr_VIN              397    400b 0
001400161019     d NRR_riga        s                   like(nrr_VIN)
001500151103     d Riga_errata     s                   like(nrr_VIN)
001600150916      ** -------------------------------------------------------- *
001700150720      *  VERSIONE ULTIMA da COMPARARE
001800150720      *
001900150720     D Ultima_VERSIONE...
002000150720     D                 C                   '03.00'
002100130904      ** -------------------------------------------------------- *
002200130904      * schiere sull'obbligatorietà e alfanumericità dei campi delle DS
002300130904      ** -------------------------------------------------------- *
002400161018     D SK_HED_O        S              1    DIM(60) CTDATA PERRCD(60)
002500161018     D SK_HED_N        S              1    DIM(60) CTDATA PERRCD(60)
002600161018     D SK_TTE_O        S              1    DIM(60) CTDATA PERRCD(60)
002700161018     D SK_TTE_N        S              1    DIM(60) CTDATA PERRCD(60)
002800150907      ** -------------------------------------------------------- *
002900130904      *     * NOMI INTESTAZIONE CAMPI x segnalare l'errore su VINMSG
003000130904      *  * ------------------------------------------------------ *
003100160210     D Testa_Sk        s            256    DIM(60)
003200161019     D H_HED           s            256    DIM(60)
003300161018     D H_TTE           s            256    DIM(60)
003400130904      ** ------------------------------- *
003500130904      *  obbligatorietà e numericità dei campi
003600130904      ** ------------------------------- *
003700130904     D OBBL            s              1    DIM(60)
003800130904     D NUME            s              1    DIM(60)
003900150716     D UTIL            s              1    DIM(60)
004000150722     D MSGER           s             30    DIM(60)
004100130904      ** ----------------------------------------------- *
004200130904      *  Ds descrittive del dettaglio della spedizione :
004300130904      ** ----------------------------------------------- *
004400161018     D Fieu05RHED    E DS                  prefix(HED_)
004500161124     D Fieu04DS31    E DS                  prefix(TTE_)
004600161019      **?------------------------------------------------------------------ */
004700161019     C*? Ds Decodifica dei campi
004800161019      **?------------------------------------------------------------------ */
004900130904      * Schiere per conversione peso DPD
005000161019      **?------------------------------------------------------------------ */
005100130904     D Tipo_error      S              1  0
005200161019     D Err_Bloccante   S              1
005300130904      *-------------------
005400130904     D W015A           S             15
005500130904     D W0140           S             14  0
005600130904     D WORA            S              6  0
005700130904     D WDTGIO          S              8  0
005800130904     D DATEU           S              8  0
005900130904     D DATA_eur        S               D   DATFMT(*eur)
006000150923     D DATA_iso        S               D   DATFMT(*iso)
006100130904      *-------------------
006200130904     D KPJBA         E DS
006300130904      *  ================================================================== */
006400161024      * ?   * Campi da tradurre con interi
006500161024     D atoll           PR            20I 0
006600161024     D                                     EXTPROC('atoll')
006700161024     D  Char                           *
006800161024     D                                     VALUE
006900161024     D                                     OPTIONS(*STRING)
007000130904      *     * Campi da tradurre con interi
007100130904     D atoi            PR            10I 0
007200130904     D                                     EXTPROC('atoi')
007300130904     D  Char                           *
007400130904     D                                     VALUE
007500130904     D                                     OPTIONS(*STRING)
007600130904      *     *--------------------------------------------------------------*
007700130904      *     ( Descrizione Campi x scomposizione FLAT record )
007800130904      *     *--------------------------------------------------------------*
007900130904      *
008000130904      *     * Campi x decodifica * (INPUT  del Record)
008100130904     D  Dati           s           1500
008200130904     D Separa_campi    s              1
008300130904     D Decimal_Separ   s              1
008400130904      *     * Campi decodificati * (OUTPUT del Record)
008500130904      *       Schiere di Output / Campi di Output
008600130904     d  Sk             s              3u 0
008700130904     D    NR_campi     s              3u 0
008800130904     D  Campi_Record   ds
008900160210     D    Dato_sk                   256    DIM(100)
009000130904     D  Campi_Lunghi   ds
009100130904     D    LunDato_sk                  3u 0 DIM(100)
009200130904     D Campi_Numerici  ds
009300130904     D    DatoNum_sk                  1    DIM(100)
009400130904     D Campi_Decimali  ds
009500130904     D    Decimal_sk                  3u 0 DIM(100)
009600130904      *     *--------------------------------------------------------------*
009700150619      *-----
009800130904     D  position       s              3  0 INZ(0)
009900130904
010000130904     D  se_errore      s              1    inz(' ')
010100130904     D  Msg_Err        s            132    inz(' ')
010200130904     D  Msg_Vin_80     s             80    inz(' ')
010300151102     D  NomeFILE       s            150    inz(' ')
010400130904      *  * ------------------------------------------------------ *
010500151217     d invia_mail_alert...
010600151217     d                 s              1    inz('N')
010700151217      *  * ------------------------------------------------------ *
010800130904      **
010900130904     D Obbligatorio    C                   'M'
011000130904     D Condizionato    C                   'C'
011100130904     D Facoltativo     C                   'O'
011200130904     D  Numerico       C                   'N'
011300150716      * ?------------------------------------------------------------------ */
011400150716      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
011500150716      * ?------------------------------------------------------------------ */
011600150716     D TxtInOut        S           2048
011700150716     D ElencoChar      S            256
011800150716     D TipoElenco      S              1
011900150716     D CharSost        S              1
012000150716     D UpperCase       S              1
012100150716     D ChkNull         S              1
012200150716     D CharNull        S              1
012300150716     D Esito_check     S              1
012400150716      * ?------------------------------------------------------------------ */
012500150716      * ?controllo x CEDINTESA
012600150716     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
012700150916     D                                     3456789 .,-()/=''+:?! %&*;<>#@_°£$')
012800150716     d bart_it         C                   CONST('@Bartolini.it;')
012900150716     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
013000150716     D   Digits        C                   '0123456789'
013100150827
013200150827     D digitsplus      c                   '0123456789 /-'
013300150907     D solosimboli     c                   ' .-_/*+:,§''=\|!?%&$£"ç°#@'
013400150716     D*------------------
013500150716      * ?set caratteri Maiuscoli/Minuscoli
013600150716     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
013700150716     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
013800130904      *  ================================================================== */
013900130904      *   Ciclo principale
014000130904      *  ********************************************************************/
014100160408     c                   SETON                                        LR
014200130904      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
014300130904     C*
014400130904     C                   if        not %open(tivin00r)
014500130904     C                   open      tivin00r
014600130904     C                   endif
014700130904      *  ------------------------------------------------------------------ */
014800130904     C*  Controllo dati arrivati da DPD
014900130904      *  ------------------------------------------------------------------ */
015000130904      *  - Occorre fare un primo test sull'integrità della trasmissione
015100130904      *  - controllando che la trasmissione sia completa.
015200130904      *                /*---------------------- */
015300130904     c                   exsr      check_Trasm
015400130904      *                /*---------------------- */
015500130904      **   --> file in errore
015600130904     c                   if        se_errore = 'S'
015700150720      *                 /* ------------------ */
015800150720     c                   exsr      TUTTO_ERRATO
015900150720      *                 /* ------------------ */
016000130904     c                   else
016100150720      *  --------------------------------------------- */
016200150720     C*  Se TUTTO OK esegue l'importazione delle Bolle */
016300150720      *  --------------------------------------------- */
016400130904      *                /*---------------------- */
016500130904     c                   exsr      Importa_Msg
016600130904      *                /*---------------------- */
016700130904     c                   end
016800150714     c                   COMMIT
016900150714
017000130904     C                   if        %open(tivin00r)
017100130904     C                   close     tivin00r
017200130904     C                   endif
017300130904      *
017400160408     c                   RETURN
017500161018      *  ------------------------------------------------------------------ */
017600161018      *  Controlla la trasmissione   se completa
017700161018      *  ------------------------------------------------------------------ */
017800161018     c     Check_Trasm   Begsr
017900161018
018000161018     C                   clear                   se_errore
018100161018     C                   clear                   Riga_iNIZIo       1
018200161018     C                   clear                   Riga_fINe         1
018300161018      ** primo  record
018400161018     c     *start        setll     tivin00r
018500161018     c     rileggi       tag
018600161018     c                   read      tivin00r
018700161018      *
018800161018     c                   dow       not %eof(tivin00r)
018900161018     c                   movel(p)  VINDTA        dati
019000161018      *
019100161018      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
019200161018      * le leggo e le escludo.
019300161018     c                   if        dati = *blank
019400161018     C                   eval      vinFLG = '1'
019500161018     c                   update    Tivin000
019600161018     c                   goto      rileggi
019700161018     c                   end
019800161018      *
019900161018     c                   if        %subst(dati:1:5) ='#FILE'
020000161018     c                               and
020100161018     c                             %subst(dati:1:1)<>' '
020200161018     C                   eval        Riga_iNIZIo ='X'
020300161018     c                   LEAVE
020400161018     c                   end
020500161018      *
020600161018      *                /*---------------------- */
020700161018     c                   exsr      Decod_Record
020800161018      *                /*---------------------- */
020900161018      *
021000161018     c                   read      tivin00r
021100161018     c                   enddo
021200161018      **
021300161018      ** ultimo record
021400161018     c     *hival        setll     tivin00r
021500161018     c     leggiAncora   tag
021600161018     c                   readp     tivin00r
021700161018     c                   DOW       not %eof(tivin00r)
021800161018      *
021900161018     c                   movel(p)  VINDTA        dati
022000161018     c                   if        dati = *blank
022100161018      * le ultime righe potrebbero essere vuote prima di finire il messaggio
022200161018      * le leggo e le escludo.
022300161018     C                   eval      vinFLG = '1'
022400161018     c                   update    Tivin000
022500161018     c                   goto      leggiAncora
022600161018     c                   end
022700161018      *
022800161018     c                   if        %subst(dati:1:4) ='#END'  AND
022900161018     c                             %subst(dati:1:1)<>' '
023000161018     C                   eval       Riga_fINe ='X'
023100161018     c                   LEAVE
023200161018     c                   end
023300161018      *
023400161018      *                /*---------------------- */
023500161018     c                   exsr      Decod_Record
023600161018      *                /*---------------------- */
023700161018     c                   readp     tivin00r
023800161018     c                   end
023900161018
024000161018      *      Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
024100161018      *      lo stesso numero trasmissione allora si deve segnalare l'errore
024200161018      *      e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
024300161018      *  -----> Errore
024400161018     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
024500161018     C                   eval      se_errore = 'S'
024600161018     c                   end
024700161018
024800161018     c                   endSR
024900161018      *
025000161018      * :+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:  */
025100161018      *  Tutto il Messaggio in ERRORE                                      */
025200161018      * :+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:  */
025300150720     c     TUTTO_errato  Begsr
025400150720      *
025500150720      ** Messaggio da riportare su ogni record x tutta la trasmissione
025600150720     C                   eval      Msg_Err = 'FILE GEODATA DPD ERRATO +
025700150720     C                              >> o VERSIONE SUPERIORE al TRADUTTORE-
025800150720     C                               << CONTROLLARE !!'
025900150720      *
026000150720      *    Scrive su tutti i records il tipo di errore
026100150720     c     *start        setll     tivin00r
026200150720     c                   read      tivin00r
026300150720     c                   dow       not %eof(tivin00r)
026400150720     C                   eval      vinMSG = Msg_Err
026500150720     C                   eval      vinFLG = '2'
026600150720     c                   eval      esito  = '2'
026700150720     c                   update    tivin000
026800150720     c                   read      tivin00r
026900150720     c                   endDO
027000150720      *
027100150720      **    avviso tramite MAIL
027200150720     C*
027300150720     C                   eval      wrkMsg='Il File DPD GEODATA è arrivato inc'+
027400161019     C                             'ompleto. Controllare UPLOAD ($T) '+
027500150720     C                             'oppure VERSIONE SUPERIORE a quella previst-
027600150720     c                             a nel TRADUTTORE.  -  ' +
027700151102     C                             'GEODATA  con stato 2: '+  nomefile +
027800151102     C                             ' TUTTI i records NON SONO STATI TRADOTTI !'
027900150720     C*
028000150720     C                   exSR      eMail_alert
028100150720      *
028200150720     c                   Endsr
028300150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
028400150715      *  Importa i records della trasmissione                               */
028500150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
028600150715     c     Importa_Msg   Begsr
028700150715
028800150715      * per controllare se almeno un record è stato importato sul VAB
028900150715     c                   clear                   Almeno_Uno        1
029000150715
029100150715     c     *start        setll     Tivin00r
029200150715     c                   read      Tivin00r
029300150715
029400150715     c                   dow       not %eof(Tivin00r)
029500150715
029600150715      * solo i record sflaggati da rielaborare
029700150715     c                   IF        vinFLG = *blank and vinDTA <> *blank
029800150715      ** Controlli formali sui campi
029900150715     c                   clear                   se_errore
030000150715     C                   clear                   Msg_Vin_80
030100150715
030200150715      ** Decodifica record a campi variabili
030300150715      *                /*---------------------- */
030400150715     c                   exsr      Decod_Record
030500150715      *                /*---------------------- */
030600150715
030700150715      **  Se presente un errore nel record emette una segnalazione msg
030800150715     c                   if        se_errore ='S'
030900150715     C                   eval      vinMSG = Msg_Vin_80
031000150715     c                   end
031100150715      *  Sempre Record OK
031200150715     C                   eval      vinFLG = '1'
031300150916     c                   endIF
031400150715
031500150715     c                   if        err_bloccante ='S'
031600150715     C                   eval      vinFLG = '2'
031700150715     c                   eval      esito  = '1'
031800150715     c                   end
031900150715
032000150715     c                   update    Tivin000
032100150720      **
032200150720      **  Se la VERSIONE del messaggio è successiva a questo TRADUTTORE
032300150720      **   deve uscire bloccando tutti i records e avvisando con EMAIL
032400150720      **   di controllare SENZA TRADURRE NULLA.
032500150720     c                   if        HED_VERSION > Ultima_VERSIONE
032600150720     c                   exsr      Tutto_ERRATO
032700150720     c                   leave
032800150720     c                   end
032900150720      **
033000150715     c                   read      Tivin00r
033100150715     C                   ENDdo
033200150715      **
033300150715     c                   endSR
033400130904      *  ------------------------------------------------------------------ */
033500130904      *     Decodifica record a campi variabili
033600130904      *  ------------------------------------------------------------------ */
033700130904     c     Decod_Record  Begsr
033800130904
033900130904      *   Sposta il record a tracciato libero su un campo di lavoro       =
034000150716     c                   clear                   dati
034100150716     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
034200150716     c                   exsr      Syntax
034300150716      **
034400130904     c                   EVAL      err_bloccante =' '
034500130904      **
034600130904      *                /*---------------------- */
034700130904     c                   exsr      Split_Rec
034800130904      *                /*---------------------- */
034900130904      **
035000130904      **  Records di Intestazione e di fine Msg
035100130904     c                   if        %subst(dati:1:1) ='#'
035200130904      *                /*---------------------- */
035300130904     c                   exsr      Rec_Testate
035400130904      *                /*---------------------- */
035500130904     c                   Else
035600130904      ** conta le righe
035700130904      *                /*---------------------- */
035800130904     c                   exsr      Rec_Dettaglio
035900130904      *                /*---------------------- */
036000130904     c                   End
036100130904      **
036200130904     c                   Endsr
036300161018      *  ------------------------------------------------------------------ */
036400161018      *       Suddivide i campi della riga con carattere divisorio (;)
036500161018      *  ------------------------------------------------------------------ */
036600161018     c     Split_Rec     Begsr
036700161018      **
036800161020     c                   eval      dati = %trim(dati) + ';'
036900161020      **
037000161018     c                   clear                   Nr_campi
037100161018     c                   clear                   Campi_Record
037200161018     c                   clear                   Campi_Lunghi
037300161018     c                   clear                   Campi_Numerici
037400161018     c                   clear                   Campi_Decimali
037500161018     c                   movel     ';'           Separa_campi
037600161018     c                   movel     '.'           Decimal_Separ
037700161018      *
037800161018      * sostituito lo standard previsto con campi max 40 bytes l'uno
037900161018      * con un pgm identico che prevede campi da 256 bytes
038000161018     c                   call      'FIEU05R0'
038100161018      *       Input
038200161018     c                   parm                    Separa_campi
038300161018     c                   parm                    Decimal_Separ
038400161018     c                   parm                    dati
038500161018      *       Output  su schiere di 100 elementi
038600161018     c                   parm                    Nr_campi
038700161018     c                   parm                    Campi_Record                   * i dati
038800161018     c                   parm                    Campi_Lunghi                   * lunghezza dati i
038900161018     c                   parm                    Campi_Numerici                 * se numerici
039000161018     c                   parm                    Campi_Decimali                 * quanti decimali i
039100161018      **
039200161018     c                   Endsr
039300130904      *  ------------------------------------------------------------------ */
039400130904      *       Record di Testata e di Fine Msg
039500130904      *  ------------------------------------------------------------------ */
039600130904     c     Rec_Testate   Begsr
039700151022      **
039800151022     c                   clear                   vinmsg
039900130904      **
040000130904      ** esempio di record :
040100161018      *     #FILE;GEODATA_STATUS_DE01_IT01_D20161004T075953_000809.dat;
040200161018      *     #ENCODING;ISO 8859-1;
040300161018      *     #VERSION;03.00;
040400161018      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
040500161018      *     #DEF;GEODATA:
040600161018      *   TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;SERVICECODE;DZIPCODE;
040700161018      **  DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;REASONCODE3;DEPOT;AGENTLOCATIONCODE;
040800161018      **  AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;COUNTRY;CITY;STATUSDATETIME;TIMEZONE;
040900161018      **  MEMDATETIME;DDEPOT;DELIVERYTOUR;MEASUREDWEIGHT;DIMENSION;PARCELREF;PODIMAGEREF;
041000161018      **  RNAME;AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;
041100161018      **  CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;RETURNPARCELNUMBER;
041200161018      **  COMPINFO1;;
041300130904      **      .....
041400130904      *     #END;
041500130904      **
041600130904      *    Prima riga
041700130904     c     'FILE'        SCAN      dati:2        position
041800130904     c                   if        %Found and position < 5
041900130904     C                   move      'S'           Riga_iNIZIo
042000151102     c                   eval      NomeFILE = dati
042100150715     C                   move      *blank        Riga_fINe
042200130904     c                   ElSe
042300130904     c                   End
042400130904      **
042500130904      *    Ultima riga
042600130904     c     'END'         SCAN      dati:2        position
042700130904     c                   if        %Found and position < 5
042800130904     C                   move      'S'           Riga_fINe
042900150715      **------
043000130904     c                   End
043100130904      **
043200130904      *    DEFINIZIONI  (testate dei campi)
043300130904     c     'DEF'         SCAN      dati:2        position
043400130904      **
043500130904     c                   if        %Found and position < 5
043600130904      **  IMPOSTA I NOMI DEI CAMPI SU SKIERE
043700150702      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;
043800161018      *     #DEF;GEODATA:TTEVENTS;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MP
043900130904      **
044000130904      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
044100150702      **   eliminando il primo elemento che è sempre "#DEF"
044200150702      *  quindi muove dal 2°campo nella relativa schiera di testata nome campi
044300150702      **
044400161019     c                   IF        %subst(dati:14:6) ='HEADER'                     H_HED
044500161019     c                   movea     Dato_sk(2)    H_HED
044600161019      *
044700161019     c                   elseIF    %subst(dati:14:8) ='TTEVENTS'                   H_TTE
044800161018     c                   movea     Dato_sk(2)    H_TTE
044900130904     c                   End
045000130904      **
045100130904     c                   End
045200130904      **
045300130904     c                   Endsr
045400130904      *  ------------------------------------------------------------------ */
045500130904      *       Riga Bolla Import da controllare e tradurre
045600130904      *  ------------------------------------------------------------------ */
045700130904     c     Rec_Dettaglio Begsr
045800130904      **
045900130904     c                   clear                   Testa_sk
046000130904     c                   clear                   Msg_Err
046100151103      ** ------------
046200151103     c                   eval      Riga_errata = Riga_errata + 1
046300130904      ** ------------
046400160210     c                   IF        %subst(Dato_sk(1):1:+LunDato_sk(1))
046500130904     c                               ='HEADER'
046600130904     c                   exsr      sr_HEADER
046700130904      ** ------------
046800130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
046900161018     c                               ='TTEVENTS'
047000161018     c                   exsr      sr_TTEVENTS
047100130904     c                   end
047200130904      ** ------------
047300130904     c                   Endsr
047400130904      *  ------------------------------------------------------------------ */
047500130904      *      HEADER
047600130904      *  ------------------------------------------------------------------ */
047700130904     c     sr_HEADER     Begsr
047800130904      **
047900150720     c                   clear                   Fieu05RHED
048000130904     c                   movea     H_HED         Testa_sk
048100150702     c                   movea     SK_HED_O      OBBL
048200150702     c                   movea     SK_HED_N      NUME
048300150716     c                   clear                   UTIL
048400150722     c                   clear                   MSGER
048500150703      **
048600150703      **  #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
048700150703      **
048800150703      * cerca in quale elemento si trova il dato
048900160210     c                   clear                   trova_campo     256
049000150702     c                   eval      trova_campo = 'VERSION'
049100150702     c                   z-add     1             sk
049200150702     c     trova_campo   lookup    Testa_sk(sk)                           21
049300150717     c   21              eval      UTIL(sk) ='S'
049400150702     c   21              eval      HED_VERSION    = %subst(Dato_sk(sk):1:+
049500150702     c                                              LunDato_sk(sk))
049600150720      **
049700150720      * Attenzione se il Messaggio ricevuto è successivo all'ultima versione
049800150720      **  da tradurre con questo programma blocca il messaggio e AVVISA.
049900150720     c   21              if        HED_VERSION > Ultima_VERSIONE
050000150720     c                   eval      se_errore ='S'
050100150720     c                   eval      err_bloccante ='S'
050200150720     C                   eval      Msg_Vin_80 = 'VERSIONE SUCCESSIVA al FORMAT-
050300150720     c                                          O previsto in TRADUZIONE !!!'
050400150720     c                   LEAVEsr
050500150720     c                   end
050600130904      **
050700150716     c                   exsr      ctrl_campi
050800150717      **
050900150717     c                   Endsr
051000130904      *  ------------------------------------------------------------------ */
051100161019      *      TTEVENTS
051200130904      *  ------------------------------------------------------------------ */
051300161019     c     sr_TTEVENTS   Begsr
051400130904      **
051500161124     c                   clear                   fieu04ds31
051600161018     c                   movea     H_TTE         Testa_sk
051700161018     c                   movea     SK_TTE_O      OBBL
051800161018     c                   movea     SK_TTE_N      NUME
051900150716     c                   clear                   UTIL
052000150722     c                   clear                   MSGER
052100161019     c                   eval      NRR_riga  = NRR_VIN
052200151103     c                   eval      Riga_errata=NRR_VIN
052300150703      **
052400161019      *     #DEF;GEODATA:
052500161019      *   TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;SERVICECODE;DZIPCODE;
052600161019      **  DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;REASONCODE3;DEPOT;AGENTLOCATIONCODE;
052700161019      **  AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;COUNTRY;CITY;STATUSDATETIME;TIMEZONE;
052800161124      **  MEMDATETIME;DDEPOT;DELIVERYTOUR;MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;
052900161124      **  PODIMAGEREF;RNAME;TRDPRNAME;AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;
053000161124      **  SUPPLIERREF;CONTAINERREF;CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;
053100161124      **  GPSLONG;RETURNPARCELNUMBER;COMPINFO1;;
053200161019      **
053300161019     c                   clear                   trova_campo
053400161019     c                   eval      trova_campo = 'PARCELNUMBER'
053500161019     c                   z-add     1             sk
053600161019     c     trova_campo   lookup    Testa_sk(sk)                           21
053700161019     c   21              eval      TTE_PARCELNR = %subst(Dato_sk(sk):1:+
053800161019     c                                            LunDato_sk(sk))
053900161019     c   21              eval      UTIL(sk) ='S'
054000161019      **
054100161019     c                   clear                   trova_campo
054200161019     c                   eval      trova_campo = 'STATUSTYPE'
054300161019     c                   z-add     1             sk
054400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
054500161019     c   21              eval      TTE_STATUSTYPE = %subst(Dato_sk(sk):1:+
054600161019     c                                            LunDato_sk(sk))
054700161019      **
054800161019     c                   clear                   trova_campo
054900161019     c                   eval      trova_campo = 'ORIGINPARCELNUMBER'
055000161019     c                   z-add     1             sk
055100161019     c     trova_campo   lookup    Testa_sk(sk)                           21
055200161019     c   21              eval      TTE_ORIGPARCEL = %subst(Dato_sk(sk):1:+
055300161019     c                                            LunDato_sk(sk))
055400150921      **
055500160210     c                   clear                   trova_campo
055600161020     c                   eval      trova_campo = 'SERVICECODE'
055700150921     c                   z-add     1             sk
055800150921     c     trova_campo   lookup    Testa_sk(sk)                           21
055900161019     c   21              eval      TTE_SERVICECOD = atoi(%TRIM(Dato_sk(sk)))
056000150921     c   21              eval      UTIL(sk) ='S'
056100150921      **
056200161019      **
056300161019     c                   clear                   trova_campo
056400161019     c                   eval      trova_campo = 'SCANCODE'
056500161019     c                   z-add     1             sk
056600161019     c     trova_campo   lookup    Testa_sk(sk)                           21
056700161019     c   21              eval      TTE_SCANCODE   = %subst(Dato_sk(sk):1:+
056800161019     c                                              LunDato_sk(sk))
056900161019     c   21              eval      UTIL(sk) ='S'
057000161019      **
057100161019     c                   clear                   trova_campo
057200161019     c                   eval      trova_campo = 'REASONCODE'
057300161019     c                   z-add     1             sk
057400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
057500161019     c   21              eval      TTE_REASONCOD1 = %subst(Dato_sk(sk):1:+
057600161019     c                                              LunDato_sk(sk))
057700161019      **
057800161019     c                   clear                   trova_campo
057900161019     c                   eval      trova_campo = 'REASONCODE2'
058000161019     c                   z-add     1             sk
058100161019     c     trova_campo   lookup    Testa_sk(sk)                           21
058200161019     c   21              eval      TTE_REASONCOD2 = %subst(Dato_sk(sk):1:+
058300161019     c                                              LunDato_sk(sk))
058400161019      **
058500161019     c                   clear                   trova_campo
058600161019     c                   eval      trova_campo = 'REASONCODE3'
058700161019     c                   z-add     1             sk
058800161019     c     trova_campo   lookup    Testa_sk(sk)                           21
058900161019     c   21              eval      TTE_REASONCOD3 = %subst(Dato_sk(sk):1:+
059000161019     c                                              LunDato_sk(sk))
059100161019      **
059200160210     c                   clear                   trova_campo
059300161019     c                   eval      trova_campo = 'DEPOT'
059400150703     c                   z-add     1             sk
059500150703     c     trova_campo   lookup    Testa_sk(sk)                           21
059600161019     c   21              eval      TTE_DEPOT    = %subst(Dato_sk(sk):1:+
059700150703     c                                              LunDato_sk(sk))
059800150717     c   21              eval      UTIL(sk) ='S'
059900150703      **
060000161019      **
060100161019     c                   clear                   trova_campo
060200161019     c                   eval      trova_campo = 'STATUSDATETIME'
060300161019     c                   z-add     1             sk
060400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
060500161024     c   21              eval      TTE_STDATETIME = atoLL(%TRIM(Dato_sk(sk)))
060600161019     c   21              eval      UTIL(sk) ='S'
060700161019      **
060800161019     c                   clear                   trova_campo
060900161019     c                   eval      trova_campo = 'TIMEZONE'
061000161019     c                   z-add     1             sk
061100161019     c     trova_campo   lookup    Testa_sk(sk)                           21
061200161019     c   21              eval      TTE_TIMEZONE   = %subst(Dato_sk(sk):1:+
061300161019     c                                              LunDato_sk(sk))
061400161019      **
061500161019      **
061600161019     c                   clear                   trova_campo
061700161019     c                   eval      trova_campo = 'DDEPOT'
061800161019     c                   z-add     1             sk
061900161019     c     trova_campo   lookup    Testa_sk(sk)                           21
062000161019     c   21              eval      TTE_DDEPOT     = atoi(%TRIM(Dato_sk(sk)))
062100161019      **
062200161019      **
062300161019     c                   clear                   trova_campo
062400161019     c                   eval      trova_campo = 'MEASUREDWEIGHT'
062500161019     c                   z-add     1             sk
062600161019     c     trova_campo   lookup    Testa_sk(sk)                           21
062700161019     c   21              eval      TTE_WEIGHT    = atoi(%TRIM(Dato_sk(sk)))
062800161019      **
062900161019      **
063000161019     c                   clear                   trova_campo
063100161019     c                   eval      trova_campo = 'PODIMAGEREF'
063200161019     c                   z-add     1             sk
063300161019     c     trova_campo   lookup    Testa_sk(sk)                           21
063400161019     c   21              eval      TTE_PODIMAGREF = %subst(Dato_sk(sk):1:+
063500161019     c                                              LunDato_sk(sk))
063600161019      **
063700161019     c                   clear                   trova_campo
063800161019     c                   eval      trova_campo = 'RNAME'
063900161019     c                   z-add     1             sk
064000161019     c     trova_campo   lookup    Testa_sk(sk)                           21
064100161019     c   21              eval      TTE_RNAME      = %subst(Dato_sk(sk):1:+
064200161019     c                                              LunDato_sk(sk))
064300161019      **
064400161019      **
064500161019     c                   clear                   trova_campo
064600161019     c                   eval      trova_campo = 'CONTAINERINFO1'
064700161019     c                   z-add     1             sk
064800161019     c     trova_campo   lookup    Testa_sk(sk)                           21
064900161019     c   21              eval      TTE_INFOCONT1  = atoi(%TRIM(Dato_sk(sk)))
065000161019      **
065100161019     c                   clear                   trova_campo
065200161019     c                   eval      trova_campo = 'CONTAINERINFO2'
065300161019     c                   z-add     1             sk
065400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
065500161019     c   21              eval      TTE_INFOCONT2  = %subst(Dato_sk(sk):1:+
065600161019     c                                              LunDato_sk(sk))
065700161019      **
065800161019      **
065900161019     c                   clear                   trova_campo
066000161019     c                   eval      trova_campo = 'COMPINFO1'
066100161019     c                   z-add     1             sk
066200161019     c     trova_campo   lookup    Testa_sk(sk)                           21
066300161019     c   21              eval      TTE_COMPINFO1  = %subst(Dato_sk(sk):1:+
066400161019     c                                              LunDato_sk(sk))
066500161019      **
066600161024      **
066700150716     c                   exsr      ctrl_campi
066800161019      **
066900161019      *     Campi necessari:
067000161019      *   1) PARCELNUMBER;
067100161019      *   2) SERVICECODE;
067200161019      **  3) SCANCODE;
067300161019      **  4) REASONCODE;
067400161019      **  5) REASONCODE2;
067500161019      **  6) REASONCODE3;
067600161019      **  7) DEPOT;
067700161019      **  8) COUNTRY;
067800161019      **  9) CITY;
067900161019      ** 10) STATUSDATETIME;
068000161019      ** 11) TIMEZONE;
068100161019      ** 12) MEASUREDWEIGHT;
068200161019      ** 13) DIMENSION;
068300161019      ** 14) CONTAINERINFO1;
068400161019      ** 15) CONTAINERINFO2;
068500161020      ** 16) RNAME;
068600150716      **
068700161019     C                   if        se_errore   = *blank
068800161020      **
068900161020     C                   call(e)   'FIEU04R1'
069000161124     C                   parm                    FIEU04ds31
069100161020     C                   parm                    se_errore
069200161020     C                   parm                    msg_vin_80
069300161020      **
069400161019     c                   End
069500161019      **
069600161019     c                   Endsr
069700130904      *  ------------------------------------------------------------------ */
069800161019      *       Campi in errore
069900161019      *  ------------------------------------------------------------------ */
070000161019     c     ctrl_campi    Begsr
070100161019      **
070200150702     c                   do        60            sk
070300150702     c                   IF        OBBL(sk)= *blank
070400150702     c                   LEAVE
070500150702     c                   EndIF
070600150702      **
070700150716     c                   IF        UTIL(sk)<>'S'
070800150716     c                   ITER
070900150716     c                   EndIF
071000150722      **
071100150722      * Prima segnalazione
071200150722     c                   IF        MSGER(sk)<>*blank
071300150722     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
071400150722     c                                          + %trim(MSGER(sk))  + ';'
071500150722     c                   EndIF
071600150716      **
071700150702      ** o è OBBLIGATORIO o è Numerico ma NON contiene NUMERI
071800150702     c                   If        LunDato_sk(sk) =  0
071900150702     c                               and OBBL(sk) = Obbligatorio
072000150702     c                             or
072100150702     c                             DatoNum_sk(sk) = 'N'
072200150702     c                               and NUME(sk) = Numerico
072300150702      *  -----> Errore
072400150702     c                   exsr      Field_Error
072500150702     c                   End
072600150717      *
072700150702     c                   EnDDO
072800150702      *
072900150702     c                   Endsr
073000150702      *  ------------------------------------------------------------------ */
073100150702      *       Campi in errore
073200150702      *  ------------------------------------------------------------------ */
073300150702     c     Field_Error   Begsr
073400150716      *
073500130904     C                   eval      se_errore   = 'S'
073600150717      * Errore su VINMSG
073700150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
073800150717     c                                          + %trim(Testa_sk(sk))
073900130904      * msg video o in posta
074000130904     c                   If        Msg_Err = *blank
074100130904     c
074200130904     c                   Select
074300130904     c
074400130904     c                   When      LunDato_sk(sk) =  0
074500130904     c                               and OBBL(sk) = Obbligatorio
074600130904     c                   eval      Tipo_Error = 1
074700150717     C                   eval      Msg_Err =
074800150717     C                             ' >> Obbl.- Manca Dato !!'
074900130904     c
075000130904     c                   When      DatoNum_sk(sk) = 'N'
075100130904     c                               and NUME(sk) = Numerico
075200130904     c                   If        LunDato_sk(sk) =  0
075300130904     c                   eval      Tipo_Error = 2
075400150717     C                   eval      Msg_Err =
075500150717     C                             ' >> Obbl.Manca o NON Numero !!'
075600130904     c                   else
075700130904     c                   eval      Tipo_Error = 3
075800150717     C                   eval      Msg_Err =
075900150717     C                             ' >> NON Numero !!'
076000130904     c                   end
076100130904     c                   When      Tipo_Error = 4
076200150717     C                   eval      Msg_Err =
076300150717     C                             ' >> Conver.non riuscita !!'
076400130904     c                   EndSL
076500150717      *
076600150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) + Msg_err
076700150717     c*
076800130904     c                   End
076900130904      **
077000130904     c                   Endsr
077100130904      *  ================================================================== */
077200130904      *   DEFINIZIONE CHIAVI                               *
077300161019      *  ================================================================== */
077400130904     C     *INZSR        BEGSR
077500130904      *
077600130904     c     *ENTRY        PLIST
077700130904     C                   parm                    esito             1
077800130904     C                   eval      esito ='0'
077900150923      *
078000130904     C*------------------
078100130904      * Recupero data e ora
078200130904     C                   TIME                    WORA
078300130904     C                   TIME                    W0140
078400130904      * UDATE IN GGMMAAAA
078500130904     C                   MOVE      W0140         WDTGIO
078600130904      * UDATE IN AAAAMMGG
078700130904     C     *eur          MOVEL     WDTGIO        DATA_eur
078800130904     C     *iso          MOVEL     DATA_eur      dateu
078900150922      *
079000130904     C                   ENDSR
079100130904      *  ------------------------------------------------------------------ */
079200130904      *   Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
079300130904      *  ------------------------------------------------------------------ */
079400130904     C     eMail_alert   BEGSR
079500151217     C*>>>>>>>>
079600151217     c                   if        invia_mail_alert  ='N'
079700151217     c                   leaveSR
079800151217     c                   end
079900151217     C*>>>>>>>>
080000130904     C*
080100130904     C                   eval      wrkEml =
080200130906     C                             'Andrea.Bertocchi@brt.it'
080300130904     C*
080400150720     C                   eval      wrkOgg='PROBLEMI UPLOAD DPD GEODATA'
080500130904     C*
080600130906     C                   call(e)   'TIS701C1'
080700130904     C                   parm                    wrkEml          253
080800130906     C                   parm                    wrkEmlcc        253
080900130904     C                   parm                    wrkOgg           44
081000130904     C                   parm                    wrkMsg         5000
081100130906     C                   parm                    wrkEsito          1
081200130904      *
081300130904     C                   eval      wrkEml =
081400130905     C                             'CedAlert@brt.it'
081500130906     C*  ??????????????????
081600130906     c                   goto      endmsg
081700130906     C*
081800130906     C                   call(e)   'TIS701C1'
081900130904     C                   parm                    wrkEml          253
082000130906     C                   parm                    wrkEmlcc        253
082100130904     C                   parm                    wrkOgg           44
082200130904     C                   parm                    wrkMsg         5000
082300130906     C                   parm                    wrkEsito          1
082400130904      *
082500130906     C     endmsg        ENDSR
082600150715      *  ================================================================== */
082700150716      * *------------------------*
082800150716     C     Syntax        begSR
082900150716      * *------------------------*
083000150716      *
083100150716     c                   movel     dati          TxtInOut
083200150716     c                   eval      ElencoChar = SETA
083300150716     c                   eval      TipoElenco = *blank
083400150716     c                   eval      CharSost   = *blank
083500150716     c                   eval      UpperCase  = '1'
083600150716     c                   eval      ChkNull    = *blank
083700150716     c                   eval      CharNull   = *blank
083800150716     c                   eval      Esito_check= *blank
083900150716      *
084000150716     c                   call      'XCHKCHAR'
084100150716     c                   parm                    TxtInOut
084200150716     c                   parm                    ElencoChar
084300150716     c                   parm                    TipoElenco
084400150716     c                   parm                    CharSost
084500150716     c                   parm                    UpperCase
084600150716     c                   parm                    ChkNull
084700150716     c                   parm                    CharNull
084800150716     c                   parm                    Esito_check
084900150716      *
085000150716      *   se è stato modificato qualcosa nella riga di FLAT
085100150716     c                   if        Esito_check = '1'
085200150716     c                   movel     TxtInOut      dati
085300150716     c                   end
085400150716      *
085500150716     C                   ENDSR
085600150916      *  ------------------------------------------------------------------ */
085700150916      *       X non bloccare in nessun caso il traduttore CLIENTI
085800150916      *  ------------------------------------------------------------------ */
085900150916     C     *pssr         BEGSR
086000150916     C
086100150916     C                   eval      esito = '2'
086200150916     C*
086300161018     C                   eval      wrkMsg='Il File DPD GEODATA TTEVENTS ha avu+
086400161019     C                              to problemi. vedere  UPLOAD ($X) '+
086500151102     C                             'GEODATA  con stato 2: ' + NomeFile
086600161019     c                             + ' da riga : ' + %editc(NRR_riga:'Z') + ' -
086700151103     c                                 riga in errore : '
086800151103     c                             + %editc(Riga_errata:'Z')
086900151103     c
087000150916      **    avviso tramite MAIL
087100150916     C                   exSR      eMail_alert
087200150916      **
087300161019     C                   exSR      SCAN_Errato
087400150916      **
087500150916     C                   COMMIT
087600150916      **
087700150916     C                   ENDSR     '*CANCL'
087800150916      * ?------------------------------------------------------------------ */
087900150916      *?    Flagga il TIVIN con Spedizione ERRATA
088000150916      * ?------------------------------------------------------------------ */
088100161019     C     SCAN_Errato   BEGSR
088200150916      *
088300150916      *
088400150916      *   Scrive su tutti i records l'errore
088500161019     c     NRR_riga      chain     tivin00r
088600150916      *
088700161019     c                   if        %Found(Tivin00r)
088800150916      *
088900161020     C                   eval      vinMSG  = 'PROBLEMI SCAN <-- Riga Errata:'+
089000161020     c                              %Trim(testa_sk(sk))
089100150916      *
089200150916     C                   eval      vinFLG = '2'
089300150916     c                   eval      esito  = '1'
089400150916     c                   eval      se_errore ='S'
089500150916     c                   eval      err_bloccante ='S'
089600150916      *
089700150916     c                   update    tivin000
089800161019     c                   end
089900150916      *
090000150916     C                   ENDSR
090100150916      * ?------------------------------------------------------------------ */
090200130904      *   Skiere definizione Obbligatorierà dei campi
090300130904      *     M=Mandatory    C=Condition    O=Optional
090400130904      *     M=Obbligatorio C=Condizionale O=Facoltativo
090500130904      * e Skiere definizione Alfanumericità o Numericità dei campi A/N
090600161019      * ?------------------------------------------------------------------ */
090700161018**  SK_HED_O
090800161018MMM
090900161018**  SK_HED_N
091000161018AAA
091100161018**  SK_TTE_O
091200161024MMMCCCCCMOOOCOOOCOMCOOCOOOOCCCOOOOOOOOOOOOO
091300161018**  SK_TTE_N
091400161024ANAAANAAAAAAAAAAAANANNANNAAANNNAAANAAAANNAA
