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
001100171213     FTIDP400F  o  a E             DISK           commit
001200171213     FFIPND02L  iF   E           K disk
001300150720      ** -------------------------------------------------------- *
001400150916     d VINds           ds
001500150916     d  nrr_VIN              397    400b 0
001600161019     d NRR_riga        s                   like(nrr_VIN)
001700151103     d Riga_errata     s                   like(nrr_VIN)
001800150916      ** -------------------------------------------------------- *
001900150720      *  VERSIONE ULTIMA da COMPARARE
002000150720      *
002100150720     D Ultima_VERSIONE...
002200161124     D                 C                   '04.00'
002300130904      ** -------------------------------------------------------- *
002400130904      * schiere sull'obbligatorietà e alfanumericità dei campi delle DS
002500130904      ** -------------------------------------------------------- *
002600161018     D SK_HED_O        S              1    DIM(60) CTDATA PERRCD(60)
002700161018     D SK_HED_N        S              1    DIM(60) CTDATA PERRCD(60)
002800161018     D SK_TTE_O        S              1    DIM(60) CTDATA PERRCD(60)
002900161018     D SK_TTE_N        S              1    DIM(60) CTDATA PERRCD(60)
003000150907      ** -------------------------------------------------------- *
003100130904      *     * NOMI INTESTAZIONE CAMPI x segnalare l'errore su VINMSG
003200130904      *  * ------------------------------------------------------ *
003300160210     D Testa_Sk        s            256    DIM(60)
003400161019     D H_HED           s            256    DIM(60)
003500161018     D H_TTE           s            256    DIM(60)
003600130904      ** ------------------------------- *
003700130904      *  obbligatorietà e numericità dei campi
003800130904      ** ------------------------------- *
003900130904     D OBBL            s              1    DIM(60)
004000130904     D NUME            s              1    DIM(60)
004100150716     D UTIL            s              1    DIM(60)
004200150722     D MSGER           s             30    DIM(60)
004300130904      ** ----------------------------------------------- *
004400130904      *  Ds descrittive del dettaglio della spedizione :
004500130904      ** ----------------------------------------------- *
004600161018     D Fieu05RHED    E DS                  prefix(HED_)
004700161124     D Fieu04DS40    E DS                  prefix(TTE_)
004800161019      **?------------------------------------------------------------------ */
004900161019     C*? Ds Decodifica dei campi
005000161019      **?------------------------------------------------------------------ */
005100130904      * Schiere per conversione peso DPD
005200161019      **?------------------------------------------------------------------ */
005300130904     D Tipo_error      S              1  0
005400161019     D Err_Bloccante   S              1
005500130904      *-------------------
005600130904     D W015A           S             15
005700130904     D W0140           S             14  0
005800130904     D WORA            S              6  0
005900130904     D WDTGIO          S              8  0
006000130904     D DATEU           S              8  0
006100130904     D DATA_eur        S               D   DATFMT(*eur)
006200150923     D DATA_iso        S               D   DATFMT(*iso)
006300130904      *-------------------
006400130904     D KPJBA         E DS
006500130904      *  ================================================================== */
006600161024      * ?   * Campi da tradurre con interi
006700161024     D atoll           PR            20I 0
006800161024     D                                     EXTPROC('atoll')
006900161024     D  Char                           *
007000161024     D                                     VALUE
007100161024     D                                     OPTIONS(*STRING)
007200130904      *     * Campi da tradurre con interi
007300130904     D atoi            PR            10I 0
007400130904     D                                     EXTPROC('atoi')
007500130904     D  Char                           *
007600130904     D                                     VALUE
007700130904     D                                     OPTIONS(*STRING)
007800130904      *     *--------------------------------------------------------------*
007900130904      *     ( Descrizione Campi x scomposizione FLAT record )
008000130904      *     *--------------------------------------------------------------*
008100130904      *
008200130904      *     * Campi x decodifica * (INPUT  del Record)
008300130904     D  Dati           s           1500
008400130904     D Separa_campi    s              1
008500130904     D Decimal_Separ   s              1
008600130904      *     * Campi decodificati * (OUTPUT del Record)
008700130904      *       Schiere di Output / Campi di Output
008800130904     d  Sk             s              3u 0
008900130904     D    NR_campi     s              3u 0
009000130904     D  Campi_Record   ds
009100160210     D    Dato_sk                   256    DIM(100)
009200130904     D  Campi_Lunghi   ds
009300130904     D    LunDato_sk                  3u 0 DIM(100)
009400130904     D Campi_Numerici  ds
009500130904     D    DatoNum_sk                  1    DIM(100)
009600130904     D Campi_Decimali  ds
009700130904     D    Decimal_sk                  3u 0 DIM(100)
009800130904      *     *--------------------------------------------------------------*
009900150619      *-----
010000130904     D  position       s              3  0 INZ(0)
010100130904
010200130904     D  se_errore      s              1    inz(' ')
010300130904     D  Msg_Err        s            132    inz(' ')
010400130904     D  Msg_Vin_80     s             80    inz(' ')
010500151102     D  NomeFILE       s            150    inz(' ')
010600130904      *  * ------------------------------------------------------ *
010700151217     d invia_mail_alert...
010800151217     d                 s              1    inz('N')
010900151217      *  * ------------------------------------------------------ *
011000130904      **
011100130904     D Obbligatorio    C                   'M'
011200130904     D Condizionato    C                   'C'
011300130904     D Facoltativo     C                   'O'
011400130904     D  Numerico       C                   'N'
011500150716      * ?------------------------------------------------------------------ */
011600150716      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
011700150716      * ?------------------------------------------------------------------ */
011800150716     D TxtInOut        S           2048
011900150716     D ElencoChar      S            256
012000150716     D TipoElenco      S              1
012100150716     D CharSost        S              1
012200150716     D UpperCase       S              1
012300150716     D ChkNull         S              1
012400150716     D CharNull        S              1
012500150716     D Esito_check     S              1
012600150716      * ?------------------------------------------------------------------ */
012700150716      * ?controllo x CEDINTESA
012800150716     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
012900150916     D                                     3456789 .,-()/=''+:?! %&*;<>#@_°£$')
013000150716     d bart_it         C                   CONST('@Bartolini.it;')
013100150716     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
013200150716     D   Digits        C                   '0123456789'
013300150827
013400150827     D digitsplus      c                   '0123456789 /-'
013500150907     D solosimboli     c                   ' .-_/*+:,§''=\|!?%&$£"ç°#@'
013600150716     D*------------------
013700150716      * ?set caratteri Maiuscoli/Minuscoli
013800150716     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
013900150716     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
014000130904      *  ================================================================== */
014100130904      *   Ciclo principale
014200130904      *  ********************************************************************/
014300160408     c                   SETON                                        LR
014400130904      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
014500130904     C*
014600130904     C                   if        not %open(tivin00r)
014700130904     C                   open      tivin00r
014800130904     C                   endif
014900130904      *  ------------------------------------------------------------------ */
015000130904     C*  Controllo dati arrivati da DPD
015100130904      *  ------------------------------------------------------------------ */
015200130904      *  - Occorre fare un primo test sull'integrità della trasmissione
015300130904      *  - controllando che la trasmissione sia completa.
015400130904      *                /*---------------------- */
015500130904     c                   exsr      check_Trasm
015600130904      *                /*---------------------- */
015700130904      **   --> file in errore
015800130904     c                   if        se_errore = 'S'
015900150720      *                 /* ------------------ */
016000150720     c                   exsr      TUTTO_ERRATO
016100150720      *                 /* ------------------ */
016200130904     c                   else
016300150720      *  --------------------------------------------- */
016400150720     C*  Se TUTTO OK esegue l'importazione delle Bolle */
016500150720      *  --------------------------------------------- */
016600130904      *                /*---------------------- */
016700130904     c                   exsr      Importa_Msg
016800130904      *                /*---------------------- */
016900130904     c                   end
017000150714     c                   COMMIT
017100150714
017200130904     C                   if        %open(tivin00r)
017300130904     C                   close     tivin00r
017400130904     C                   endif
017500130904      *
017600160408     c                   RETURN
017700161018      *  ------------------------------------------------------------------ */
017800161018      *  Controlla la trasmissione   se completa
017900161018      *  ------------------------------------------------------------------ */
018000161018     c     Check_Trasm   Begsr
018100161018
018200161018     C                   clear                   se_errore
018300161018     C                   clear                   Riga_iNIZIo       1
018400161018     C                   clear                   Riga_fINe         1
018500161018      ** primo  record
018600161018     c     *start        setll     tivin00r
018700161018     c     rileggi       tag
018800161018     c                   read      tivin00r
018900161018      *
019000161018     c                   dow       not %eof(tivin00r)
019100161018     c                   movel(p)  VINDTA        dati
019200161018      *
019300161018      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
019400161018      * le leggo e le escludo.
019500161018     c                   if        dati = *blank
019600161018     C                   eval      vinFLG = '1'
019700161018     c                   update    Tivin000
019800161018     c                   goto      rileggi
019900161018     c                   end
020000161018      *
020100161018     c                   if        %subst(dati:1:5) ='#FILE'
020200161018     c                               and
020300161018     c                             %subst(dati:1:1)<>' '
020400161018     C                   eval        Riga_iNIZIo ='X'
020500161018     c                   LEAVE
020600161018     c                   end
020700161018      *
020800161018      *                /*---------------------- */
020900161018     c                   exsr      Decod_Record
021000161018      *                /*---------------------- */
021100161018      *
021200161018     c                   read      tivin00r
021300161018     c                   enddo
021400161018      **
021500161018      ** ultimo record
021600161018     c     *hival        setll     tivin00r
021700161018     c     leggiAncora   tag
021800161018     c                   readp     tivin00r
021900161018     c                   DOW       not %eof(tivin00r)
022000161018      *
022100161018     c                   movel(p)  VINDTA        dati
022200161018     c                   if        dati = *blank
022300161018      * le ultime righe potrebbero essere vuote prima di finire il messaggio
022400161018      * le leggo e le escludo.
022500161018     C                   eval      vinFLG = '1'
022600161018     c                   update    Tivin000
022700161018     c                   goto      leggiAncora
022800161018     c                   end
022900161018      *
023000161018     c                   if        %subst(dati:1:4) ='#END'  AND
023100161018     c                             %subst(dati:1:1)<>' '
023200161018     C                   eval       Riga_fINe ='X'
023300161018     c                   LEAVE
023400161018     c                   end
023500161018      *
023600161018      *                /*---------------------- */
023700161018     c                   exsr      Decod_Record
023800161018      *                /*---------------------- */
023900161018     c                   readp     tivin00r
024000161018     c                   end
024100161018
024200161018      *      Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
024300161018      *      lo stesso numero trasmissione allora si deve segnalare l'errore
024400161018      *      e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
024500161018      *  -----> Errore
024600161018     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
024700161018     C                   eval      se_errore = 'S'
024800161018     c                   end
024900161018
025000161018     c                   endSR
025100161018      *
025200161018      * :+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:  */
025300161018      *  Tutto il Messaggio in ERRORE                                      */
025400161018      * :+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:+:  */
025500150720     c     TUTTO_errato  Begsr
025600150720      *
025700150720      ** Messaggio da riportare su ogni record x tutta la trasmissione
025800150720     C                   eval      Msg_Err = 'FILE GEODATA DPD ERRATO +
025900150720     C                              >> o VERSIONE SUPERIORE al TRADUTTORE-
026000150720     C                               << CONTROLLARE !!'
026100150720      *
026200150720      *    Scrive su tutti i records il tipo di errore
026300150720     c     *start        setll     tivin00r
026400150720     c                   read      tivin00r
026500150720     c                   dow       not %eof(tivin00r)
026600150720     C                   eval      vinMSG = Msg_Err
026700150720     C                   eval      vinFLG = '2'
026800150720     c                   eval      esito  = '2'
026900150720     c                   update    tivin000
027000150720     c                   read      tivin00r
027100150720     c                   endDO
027200150720      *
027300150720      **    avviso tramite MAIL
027400150720     C*
027500150720     C                   eval      wrkMsg='Il File DPD GEODATA è arrivato inc'+
027600161019     C                             'ompleto. Controllare UPLOAD ($T) '+
027700150720     C                             'oppure VERSIONE SUPERIORE a quella previst-
027800150720     c                             a nel TRADUTTORE.  -  ' +
027900151102     C                             'GEODATA  con stato 2: '+  nomefile +
028000151102     C                             ' TUTTI i records NON SONO STATI TRADOTTI !'
028100150720     C*
028200150720     C                   exSR      eMail_alert
028300150720      *
028400150720     c                   Endsr
028500150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
028600150715      *  Importa i records della trasmissione                               */
028700150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
028800150715     c     Importa_Msg   Begsr
028900150715
029000150715      * per controllare se almeno un record è stato importato sul VAB
029100150715     c                   clear                   Almeno_Uno        1
029200150715
029300150715     c     *start        setll     Tivin00r
029400150715     c                   read      Tivin00r
029500150715
029600150715     c                   dow       not %eof(Tivin00r)
029700150715
029800150715      * solo i record sflaggati da rielaborare
029900150715     c                   IF        vinFLG = *blank and vinDTA <> *blank
030000150715      ** Controlli formali sui campi
030100150715     c                   clear                   se_errore
030200150715     C                   clear                   Msg_Vin_80
030300150715
030400150715      ** Decodifica record a campi variabili
030500150715      *                /*---------------------- */
030600150715     c                   exsr      Decod_Record
030700150715      *                /*---------------------- */
030800150715
030900150715      **  Se presente un errore nel record emette una segnalazione msg
031000150715     c                   if        se_errore ='S'
031100150715     C                   eval      vinMSG = Msg_Vin_80
031200150715     c                   end
031300150715      *  Sempre Record OK
031400150715     C                   eval      vinFLG = '1'
031500150916     c                   endIF
031600150715
031700150715     c                   if        err_bloccante ='S'
031800150715     C                   eval      vinFLG = '2'
031900150715     c                   eval      esito  = '1'
032000150715     c                   end
032100150715
032200150715     c                   update    Tivin000
032300150720      **
032400150720      **  Se la VERSIONE del messaggio è successiva a questo TRADUTTORE
032500150720      **   deve uscire bloccando tutti i records e avvisando con EMAIL
032600150720      **   di controllare SENZA TRADURRE NULLA.
032700150720     c                   if        HED_VERSION > Ultima_VERSIONE
032800150720     c                   exsr      Tutto_ERRATO
032900150720     c                   leave
033000150720     c                   end
033100150720      **
033200150715     c                   read      Tivin00r
033300150715     C                   ENDdo
033400150715      **
033500150715     c                   endSR
033600130904      *  ------------------------------------------------------------------ */
033700130904      *     Decodifica record a campi variabili
033800130904      *  ------------------------------------------------------------------ */
033900130904     c     Decod_Record  Begsr
034000130904
034100130904      *   Sposta il record a tracciato libero su un campo di lavoro       =
034200150716     c                   clear                   dati
034300150716     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
034400150716     c                   exsr      Syntax
034500150716      **
034600130904     c                   EVAL      err_bloccante =' '
034700130904      **
034800130904      *                /*---------------------- */
034900130904     c                   exsr      Split_Rec
035000130904      *                /*---------------------- */
035100130904      **
035200130904      **  Records di Intestazione e di fine Msg
035300130904     c                   if        %subst(dati:1:1) ='#'
035400130904      *                /*---------------------- */
035500130904     c                   exsr      Rec_Testate
035600130904      *                /*---------------------- */
035700130904     c                   Else
035800130904      ** conta le righe
035900130904      *                /*---------------------- */
036000130904     c                   exsr      Rec_Dettaglio
036100130904      *                /*---------------------- */
036200130904     c                   End
036300130904      **
036400130904     c                   Endsr
036500161018      *  ------------------------------------------------------------------ */
036600161018      *       Suddivide i campi della riga con carattere divisorio (;)
036700161018      *  ------------------------------------------------------------------ */
036800161018     c     Split_Rec     Begsr
036900161018      **
037000161020     c                   eval      dati = %trim(dati) + ';'
037100161020      **
037200161018     c                   clear                   Nr_campi
037300161018     c                   clear                   Campi_Record
037400161018     c                   clear                   Campi_Lunghi
037500161018     c                   clear                   Campi_Numerici
037600161018     c                   clear                   Campi_Decimali
037700161018     c                   movel     ';'           Separa_campi
037800161018     c                   movel     '.'           Decimal_Separ
037900161018      *
038000161018      * sostituito lo standard previsto con campi max 40 bytes l'uno
038100161018      * con un pgm identico che prevede campi da 256 bytes
038200161018     c                   call      'FIEU05R0'
038300161018      *       Input
038400161018     c                   parm                    Separa_campi
038500161018     c                   parm                    Decimal_Separ
038600161018     c                   parm                    dati
038700161018      *       Output  su schiere di 100 elementi
038800161018     c                   parm                    Nr_campi
038900161018     c                   parm                    Campi_Record                   * i dati
039000161018     c                   parm                    Campi_Lunghi                   * lunghezza dati i
039100161018     c                   parm                    Campi_Numerici                 * se numerici
039200161018     c                   parm                    Campi_Decimali                 * quanti decimali i
039300161018      **
039400161018     c                   Endsr
039500130904      *  ------------------------------------------------------------------ */
039600130904      *       Record di Testata e di Fine Msg
039700130904      *  ------------------------------------------------------------------ */
039800130904     c     Rec_Testate   Begsr
039900151022      **
040000151022     c                   clear                   vinmsg
040100130904      **
040200130904      ** esempio di record :
040300161018      *     #FILE;GEODATA_STATUS_DE01_IT01_D20161004T075953_000809.dat;
040400161018      *     #ENCODING;ISO 8859-1;
040500161018      *     #VERSION;03.00;
040600161018      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
040700161018      *     #DEF;GEODATA:
040800161018      *   TTEVENTS;NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;SERVICECODE;DZIPCODE;
040900161018      **  DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;REASONCODE3;DEPOT;AGENTLOCATIONCODE;
041000161018      **  AGENTLOCATIONCODETYPE;AGENTPARCELNUMBER;COUNTRY;CITY;STATUSDATETIME;TIMEZONE;
041100161018      **  MEMDATETIME;DDEPOT;DELIVERYTOUR;MEASUREDWEIGHT;DIMENSION;PARCELREF;PODIMAGEREF;
041200161018      **  RNAME;AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;
041300161018      **  CONTAINERTYPE;ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;RETURNPARCELNUMBER;
041400161018      **  COMPINFO1;;
041500130904      **      .....
041600130904      *     #END;
041700130904      **
041800130904      *    Prima riga
041900130904     c     'FILE'        SCAN      dati:2        position
042000130904     c                   if        %Found and position < 5
042100130904     C                   move      'S'           Riga_iNIZIo
042200151102     c                   eval      NomeFILE = dati
042300150715     C                   move      *blank        Riga_fINe
042400130904     c                   ElSe
042500130904     c                   End
042600130904      **
042700130904      *    Ultima riga
042800130904     c     'END'         SCAN      dati:2        position
042900130904     c                   if        %Found and position < 5
043000130904     C                   move      'S'           Riga_fINe
043100150715      **------
043200130904     c                   End
043300130904      **
043400130904      *    DEFINIZIONI  (testate dei campi)
043500130904     c     'DEF'         SCAN      dati:2        position
043600130904      **
043700130904     c                   if        %Found and position < 5
043800130904      **  IMPOSTA I NOMI DEI CAMPI SU SKIERE
043900150702      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;
044000161018      *     #DEF;GEODATA:TTEVENTS;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MP
044100130904      **
044200130904      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
044300150702      **   eliminando il primo elemento che è sempre "#DEF"
044400150702      *  quindi muove dal 2°campo nella relativa schiera di testata nome campi
044500150702      **
044600161019     c                   IF        %subst(dati:14:6) ='HEADER'                     H_HED
044700161019     c                   movea     Dato_sk(2)    H_HED
044800161019      *
044900161019     c                   elseIF    %subst(dati:14:8) ='TTEVENTS'                   H_TTE
045000161018     c                   movea     Dato_sk(2)    H_TTE
045100130904     c                   End
045200130904      **
045300130904     c                   End
045400130904      **
045500130904     c                   Endsr
045600130904      *  ------------------------------------------------------------------ */
045700130904      *       Riga Bolla Import da controllare e tradurre
045800130904      *  ------------------------------------------------------------------ */
045900130904     c     Rec_Dettaglio Begsr
046000130904      **
046100130904     c                   clear                   Testa_sk
046200130904     c                   clear                   Msg_Err
046300151103      ** ------------
046400151103     c                   eval      Riga_errata = Riga_errata + 1
046500130904      ** ------------
046600160210     c                   IF        %subst(Dato_sk(1):1:+LunDato_sk(1))
046700130904     c                               ='HEADER'
046800130904     c                   exsr      sr_HEADER
046900130904      ** ------------
047000130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
047100161018     c                               ='TTEVENTS'
047200161018     c                   exsr      sr_TTEVENTS
047300130904     c                   end
047400130904      ** ------------
047500130904     c                   Endsr
047600130904      *  ------------------------------------------------------------------ */
047700130904      *      HEADER
047800130904      *  ------------------------------------------------------------------ */
047900130904     c     sr_HEADER     Begsr
048000130904      **
048100150720     c                   clear                   Fieu05RHED
048200130904     c                   movea     H_HED         Testa_sk
048300150702     c                   movea     SK_HED_O      OBBL
048400150702     c                   movea     SK_HED_N      NUME
048500150716     c                   clear                   UTIL
048600150722     c                   clear                   MSGER
048700150703      **
048800150703      **  #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
048900150703      **
049000150703      * cerca in quale elemento si trova il dato
049100160210     c                   clear                   trova_campo     256
049200150702     c                   eval      trova_campo = 'VERSION'
049300150702     c                   z-add     1             sk
049400150702     c     trova_campo   lookup    Testa_sk(sk)                           21
049500150717     c   21              eval      UTIL(sk) ='S'
049600150702     c   21              eval      HED_VERSION    = %subst(Dato_sk(sk):1:+
049700150702     c                                              LunDato_sk(sk))
049800150720      **
049900150720      * Attenzione se il Messaggio ricevuto è successivo all'ultima versione
050000150720      **  da tradurre con questo programma blocca il messaggio e AVVISA.
050100150720     c   21              if        HED_VERSION > Ultima_VERSIONE
050200150720     c                   eval      se_errore ='S'
050300150720     c                   eval      err_bloccante ='S'
050400150720     C                   eval      Msg_Vin_80 = 'VERSIONE SUCCESSIVA al FORMAT-
050500150720     c                                          O previsto in TRADUZIONE !!!'
050600150720     c                   LEAVEsr
050700150720     c                   end
050800130904      **
050900150716     c                   exsr      ctrl_campi
051000150717      **
051100150717     c                   Endsr
051200130904      *  ------------------------------------------------------------------ */
051300161019      *      TTEVENTS
051400130904      *  ------------------------------------------------------------------ */
051500161019     c     sr_TTEVENTS   Begsr
051600130904      **
051700161124     c                   clear                   fieu04ds40
051800161018     c                   movea     H_TTE         Testa_sk
051900161018     c                   movea     SK_TTE_O      OBBL
052000161018     c                   movea     SK_TTE_N      NUME
052100150716     c                   clear                   UTIL
052200150722     c                   clear                   MSGER
052300161019     c                   eval      NRR_riga  = NRR_VIN
052400151103     c                   eval      Riga_errata=NRR_VIN
052500150703      **
052600171213      *     #DEF;GEODATA:TTEVENTS;
052700171213      *   NUMORDER;PARCELNUMBER;STATUSTYPE;ORIGINPARCELNUMBER;SERVICECODE;ASCODE;DZIPCODE;
052800171213      **  DCOUNTRYCODE;SCANCODE;REASONCODE;REASONCODE2;REASONCODE3;DEPOT;AGENTLOCATIONCODE;
052900171213      **  AGEENTLOCATIONCODETYPE;AGENTPARCELNUMBER;COUNTRY;CITY;STATUSDATETIME;TIMEZONE;
053000171213      **  MEMDATETIME;DDEPOT;DELIVERYTOUR;MEASUREDWEIGHT;DIMENSION;PARCELREF;PARCELREF2;
053100171213      **  PODIMAGEREF;RNAME;TRDPRNAME;AREA;STOP;CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;STOP;
053200171213      **  CONTAINERINFO1;CONTAINERINFO2;SHIPMENTREF;SUPPLIERREF;CONTAINERREF;CONTAINERTYPE;
053300171213      **  ACTORID;OPERATIVERID;PICKUPORDERNUMBER;GPSLAT;GPSLONG;RETURNPARCELNUMBER;COMPINFO1;;
053400161019      **
053500161019     c                   clear                   trova_campo
053600161019     c                   eval      trova_campo = 'PARCELNUMBER'
053700161019     c                   z-add     1             sk
053800161019     c     trova_campo   lookup    Testa_sk(sk)                           21
053900161019     c   21              eval      TTE_PARCELNR = %subst(Dato_sk(sk):1:+
054000161019     c                                            LunDato_sk(sk))
054100161019     c   21              eval      UTIL(sk) ='S'
054200161019      **
054300161019     c                   clear                   trova_campo
054400161019     c                   eval      trova_campo = 'STATUSTYPE'
054500161019     c                   z-add     1             sk
054600161019     c     trova_campo   lookup    Testa_sk(sk)                           21
054700161019     c   21              eval      TTE_STATUSTYPE = %subst(Dato_sk(sk):1:+
054800161019     c                                            LunDato_sk(sk))
054900161019      **
055000161019     c                   clear                   trova_campo
055100161019     c                   eval      trova_campo = 'ORIGINPARCELNUMBER'
055200161019     c                   z-add     1             sk
055300161019     c     trova_campo   lookup    Testa_sk(sk)                           21
055400161019     c   21              eval      TTE_ORIGPARCEL = %subst(Dato_sk(sk):1:+
055500161019     c                                            LunDato_sk(sk))
055600150921      **
055700160210     c                   clear                   trova_campo
055800161020     c                   eval      trova_campo = 'SERVICECODE'
055900150921     c                   z-add     1             sk
056000150921     c     trova_campo   lookup    Testa_sk(sk)                           21
056100161019     c   21              eval      TTE_SERVICECOD = atoi(%TRIM(Dato_sk(sk)))
056200171213     c** 21 *******      eval      UTIL(sk) ='S'
056300171213      **
056400171213     c                   clear                   trova_campo
056500171213     c                   eval      trova_campo = 'ASCODE'
056600171213     c                   z-add     1             sk
056700171213     c     trova_campo   lookup    Testa_sk(sk)                           21
056800171213     c   21              eval      TTE_ASCODE = %subst(Dato_sk(sk):1:+
056900171213     c                                              LunDato_sk(sk))
057000171213     c** 21 *******      eval      UTIL(sk) ='S'
057100171213      **
057200171213     c                   clear                   trova_campo
057300171213     c                   eval      trova_campo = 'DZIPCODE'
057400171213     c                   z-add     1             sk
057500171213     c     trova_campo   lookup    Testa_sk(sk)                           21
057600171213     c   21              eval      TTE_DZIPCODE = %subst(Dato_sk(sk):1:+
057700171213     c                                              LunDato_sk(sk))
057800171213     c***21********      eval      UTIL(sk) ='S'
057900171213      **
058000171213     c                   clear                   trova_campo
058100171213     c                   eval      trova_campo = 'DCOUNTRYCODE'
058200171213     c                   z-add     1             sk
058300171213     c     trova_campo   lookup    Testa_sk(sk)                           21
058400171213     c   21              eval      TTE_DCOUNTRYCD = atoi(%TRIM(Dato_sk(sk)))
058500171213     c***21********      eval      UTIL(sk) ='S'
058600161019      **
058700161019     c                   clear                   trova_campo
058800161019     c                   eval      trova_campo = 'SCANCODE'
058900161019     c                   z-add     1             sk
059000161019     c     trova_campo   lookup    Testa_sk(sk)                           21
059100161019     c   21              eval      TTE_SCANCODE   = %subst(Dato_sk(sk):1:+
059200161019     c                                              LunDato_sk(sk))
059300161019     c   21              eval      UTIL(sk) ='S'
059400161019      **
059500161019     c                   clear                   trova_campo
059600161019     c                   eval      trova_campo = 'REASONCODE'
059700161019     c                   z-add     1             sk
059800161019     c     trova_campo   lookup    Testa_sk(sk)                           21
059900161019     c   21              eval      TTE_REASONCOD1 = %subst(Dato_sk(sk):1:+
060000161019     c                                              LunDato_sk(sk))
060100161019      **
060200161019     c                   clear                   trova_campo
060300161019     c                   eval      trova_campo = 'REASONCODE2'
060400161019     c                   z-add     1             sk
060500161019     c     trova_campo   lookup    Testa_sk(sk)                           21
060600161019     c   21              eval      TTE_REASONCOD2 = %subst(Dato_sk(sk):1:+
060700161019     c                                              LunDato_sk(sk))
060800161019      **
060900161019     c                   clear                   trova_campo
061000161019     c                   eval      trova_campo = 'REASONCODE3'
061100161019     c                   z-add     1             sk
061200161019     c     trova_campo   lookup    Testa_sk(sk)                           21
061300161019     c   21              eval      TTE_REASONCOD3 = %subst(Dato_sk(sk):1:+
061400161019     c                                              LunDato_sk(sk))
061500161019      **
061600160210     c                   clear                   trova_campo
061700161019     c                   eval      trova_campo = 'DEPOT'
061800150703     c                   z-add     1             sk
061900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
062000161019     c   21              eval      TTE_DEPOT    = %subst(Dato_sk(sk):1:+
062100150703     c                                              LunDato_sk(sk))
062200150717     c   21              eval      UTIL(sk) ='S'
062300150703      **
062400161019     c                   clear                   trova_campo
062500161019     c                   eval      trova_campo = 'STATUSDATETIME'
062600161019     c                   z-add     1             sk
062700161019     c     trova_campo   lookup    Testa_sk(sk)                           21
062800161024     c   21              eval      TTE_STDATETIME = atoLL(%TRIM(Dato_sk(sk)))
062900161019     c   21              eval      UTIL(sk) ='S'
063000161019      **
063100161019     c                   clear                   trova_campo
063200161019     c                   eval      trova_campo = 'TIMEZONE'
063300161019     c                   z-add     1             sk
063400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
063500161019     c   21              eval      TTE_TIMEZONE   = %subst(Dato_sk(sk):1:+
063600161019     c                                              LunDato_sk(sk))
063700161019      **
063800161019     c                   clear                   trova_campo
063900161019     c                   eval      trova_campo = 'DDEPOT'
064000161019     c                   z-add     1             sk
064100161019     c     trova_campo   lookup    Testa_sk(sk)                           21
064200161019     c   21              eval      TTE_DDEPOT     = atoi(%TRIM(Dato_sk(sk)))
064300171213      **
064400171213     c                   clear                   trova_campo
064500171213     c                   eval      trova_campo = 'DELIVERYTOUR'
064600171213     c                   z-add     1             sk
064700171213     c     trova_campo   lookup    Testa_sk(sk)                           21
064800171213     c   21              eval      TTE_DELIVTOUR  = %subst(Dato_sk(sk):1:+
064900171213     c                                              LunDato_sk(sk))
065000161019      **
065100161019     c                   clear                   trova_campo
065200161019     c                   eval      trova_campo = 'MEASUREDWEIGHT'
065300161019     c                   z-add     1             sk
065400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
065500161019     c   21              eval      TTE_WEIGHT    = atoi(%TRIM(Dato_sk(sk)))
065600171213      **
065700171213     c                   clear                   trova_campo
065800171213     c                   eval      trova_campo = 'PARCELREF'
065900171213     c                   z-add     1             sk
066000171213     c     trova_campo   lookup    Testa_sk(sk)                           21
066100171213     c   21              eval      TTE_PARCELREF  = %subst(Dato_sk(sk):1:+
066200171213     c                                              LunDato_sk(sk))
066300171213      **
066400171213     c                   clear                   trova_campo
066500171213     c                   eval      trova_campo = 'PARCELREF2'
066600171213     c                   z-add     1             sk
066700171213     c     trova_campo   lookup    Testa_sk(sk)                           21
066800171213     c   21              eval      TTE_PARCELREF2 = %subst(Dato_sk(sk):1:+
066900171213     c                                              LunDato_sk(sk))
067000161019      **
067100161019     c                   clear                   trova_campo
067200161019     c                   eval      trova_campo = 'PODIMAGEREF'
067300161019     c                   z-add     1             sk
067400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
067500161019     c   21              eval      TTE_PODIMAGREF = %subst(Dato_sk(sk):1:+
067600161019     c                                              LunDato_sk(sk))
067700171213      **
067800171213     c                   clear                   trova_campo
067900171213     c                   eval      trova_campo = 'RNAME'
068000171213     c                   z-add     1             sk
068100171213     c     trova_campo   lookup    Testa_sk(sk)                           21
068200171213     c   21              eval      TTE_RNAME      = %subst(Dato_sk(sk):1:+
068300171213     c                                              LunDato_sk(sk))
068400161019      **
068500161019     c                   clear                   trova_campo
068600161019     c                   eval      trova_campo = 'CONTAINERINFO1'
068700161019     c                   z-add     1             sk
068800161019     c     trova_campo   lookup    Testa_sk(sk)                           21
068900161019     c   21              eval      TTE_INFOCONT1  = atoi(%TRIM(Dato_sk(sk)))
069000161019      **
069100161019     c                   clear                   trova_campo
069200161019     c                   eval      trova_campo = 'CONTAINERINFO2'
069300161019     c                   z-add     1             sk
069400161019     c     trova_campo   lookup    Testa_sk(sk)                           21
069500161019     c   21              eval      TTE_INFOCONT2  = %subst(Dato_sk(sk):1:+
069600161019     c                                              LunDato_sk(sk))
069700161019      **
069800161019      **
069900161019     c                   clear                   trova_campo
070000161019     c                   eval      trova_campo = 'COMPINFO1'
070100161019     c                   z-add     1             sk
070200161019     c     trova_campo   lookup    Testa_sk(sk)                           21
070300161019     c   21              eval      TTE_COMPINFO1  = %subst(Dato_sk(sk):1:+
070400161019     c                                              LunDato_sk(sk))
070500161019      **
070600150716     c                   exsr      ctrl_campi
070700161019      **
070800161019      *     Campi necessari:
070900171213      **  1) PARCELNUMBER;
071000171213      **  2) SERVICECODE;
071100171213      **  3) ASCODE;
071200171213      **  4) DZIPCODE;
071300171213      **  5) DCOUNTRYCODE;
071400171213      **  6) SCANCODE;
071500171213      **  7) REASONCODE;
071600171213      **  8) REASONCODE2;
071700171213      **  9) REASONCODE3;
071800171213      ** 10) DEPOT;
071900171213      ** 11) STATUSDATETIME;
072000171213      ** 12) DDEPOT;
072100171213      ** 13) DELIVERYTOUR;
072200171213      ** 14) MEASUREDWEIGHT;
072300171213      ** 15) PARCELREF;
072400171213      ** 16) PARCELREF2;
072500171213      ** 17) PODIMAGEREF;
072600171213      ** 18) RNAME;
072700171213      ** 19) CONTAINERINFO1;
072800171213      ** 20) CONTAINERINFO2;
072900150716      **
073000161019     C                   if        se_errore   = *blank
073100161020      **
073200161020     C                   call(e)   'FIEU04R1'
073300161124     C                   parm                    FIEU04ds40
073400161020     C                   parm                    se_errore
073500161020     C                   parm                    msg_vin_80
073600171213     c                   Else
073700171213      **     se errore
073800171213     c                   End
073900161019      **
074000171213      **  Memorizza lo SCAN Ricevuto
074100171213     c                   if        se_errore <> *blank and
074200171213     c                             %subst(msg_vin_80:1:16) = 'SCAN da ignorare'
074300171213     c                   leavesr
074400171213     c                   end
074500171213     c                   Exsr      write_DP4
074600171213      **
074700161019     c                   Endsr
074800130904      *  ------------------------------------------------------------------ */
074900171213      *      scrive il TIDP400F  per memorizzare gli SCAN ricevuti
075000161019      *  ------------------------------------------------------------------ */
075100171213     c     write_DP4     Begsr
075200161019      **
075300171213     c                   clear                   TIDP400R
075400171213     c                   eval      DP4PARCEL    = TTE_PARCELNR
075500171213     c                   eval      DP4SERVICE   = TTE_SERVICECOD
075600171213     c                   eval      DP4ASCODE    = TTE_ASCODE
075700171213     c                   eval      DP4DZIPCOD   = TTE_DZIPCODE
075800171213     c                   eval      DP4DCOUNTR   = TTE_DCOUNTRYCD
075900171213     c                   eval      DP4SCAN      = TTE_SCANCODE
076000171213     c                   eval      DP4ADDCOD1   = TTE_REASONCOD1
076100171213     c                   eval      DP4ADDCOD2   = TTE_REASONCOD2
076200171213     c                   eval      DP4ADDCOD3   = TTE_REASONCOD3
076300171213     c                   eval      DP4DEPCOD    = TTE_DEPOT
076400171213     c                   eval      DP4DATTIM    = TTE_STDATETIME
076500171213     c                   eval      DP4DELDEP    = TTE_DDEPOT
076600171213     c                   eval      DP4TOUR      = TTE_DELIVTOUR
076700171214     c                   eval      DP4WEIGHT    = TTE_WEIGHT/100
076800171213     c                   eval      DP4PARREF    = TTE_PARCELREF
076900171213     c                   eval      DP4PARREF2   = TTE_PARCELREF2
077000171213     c                   eval      DP4PODIMAG   = TTE_PODIMAGREF
077100171213     c                   eval      DP4RECNAME   = TTE_RNAME
077200171213     c                   eval      DP4INFO01    = TTE_INFOCONT1
077300171213     c                   eval      DP4INFO02    = TTE_INFOCONT2
077400171213     c                   eval      DP4DATRIC =  dateu
077500171213     c                   eval      DP4ORARIC =  wora
077600171213      **
077700171213     c                   if        se_errore <> *blank
077800171213     c                   eval      DP4NOELAB = 'N'
077900171213     c                   eval      DP4MSGERR = msg_vin_80
078000171213     c                   end
078100171213      **
078200171214     c     DP4PARCEL     setgt     FIPND02L
078300171214     c     DP4PARCEL     readpe    FIPND02L
078400171214     c                   if        not %EoF(FIPND02L)
078500171213     c                   eval      DP4AAS =  PNDAAS
078600171213     c                   eval      DP4LNP =  PNDLNP
078700171213     c                   eval      DP4NRS =  PNDNRS
078800171213     c                   eval      DP4NSP =  PNDNSP
078900171213     c                   write     TIDP400R
079000171213     c                   End
079100171213      **
079200171213     c                   Endsr
079300171213      *  ------------------------------------------------------------------ */
079400171213      *       Campi in errore
079500171213      *  ------------------------------------------------------------------ */
079600171213     c     ctrl_campi    Begsr
079700171213      **
079800150702     c                   do        60            sk
079900150702     c                   IF        OBBL(sk)= *blank
080000150702     c                   LEAVE
080100150702     c                   EndIF
080200150702      **
080300150716     c                   IF        UTIL(sk)<>'S'
080400150716     c                   ITER
080500150716     c                   EndIF
080600150722      **
080700150722      * Prima segnalazione
080800150722     c                   IF        MSGER(sk)<>*blank
080900150722     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
081000150722     c                                          + %trim(MSGER(sk))  + ';'
081100150722     c                   EndIF
081200150716      **
081300150702      ** o è OBBLIGATORIO o è Numerico ma NON contiene NUMERI
081400150702     c                   If        LunDato_sk(sk) =  0
081500150702     c                               and OBBL(sk) = Obbligatorio
081600150702     c                             or
081700150702     c                             DatoNum_sk(sk) = 'N'
081800150702     c                               and NUME(sk) = Numerico
081900150702      *  -----> Errore
082000150702     c                   exsr      Field_Error
082100150702     c                   End
082200150717      *
082300150702     c                   EnDDO
082400150702      *
082500150702     c                   Endsr
082600150702      *  ------------------------------------------------------------------ */
082700150702      *       Campi in errore
082800150702      *  ------------------------------------------------------------------ */
082900150702     c     Field_Error   Begsr
083000150716      *
083100130904     C                   eval      se_errore   = 'S'
083200150717      * Errore su VINMSG
083300150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
083400150717     c                                          + %trim(Testa_sk(sk))
083500130904      * msg video o in posta
083600130904     c                   If        Msg_Err = *blank
083700130904     c
083800130904     c                   Select
083900130904     c
084000130904     c                   When      LunDato_sk(sk) =  0
084100130904     c                               and OBBL(sk) = Obbligatorio
084200130904     c                   eval      Tipo_Error = 1
084300150717     C                   eval      Msg_Err =
084400150717     C                             ' >> Obbl.- Manca Dato !!'
084500130904     c
084600130904     c                   When      DatoNum_sk(sk) = 'N'
084700130904     c                               and NUME(sk) = Numerico
084800130904     c                   If        LunDato_sk(sk) =  0
084900130904     c                   eval      Tipo_Error = 2
085000150717     C                   eval      Msg_Err =
085100150717     C                             ' >> Obbl.Manca o NON Numero !!'
085200130904     c                   else
085300130904     c                   eval      Tipo_Error = 3
085400150717     C                   eval      Msg_Err =
085500150717     C                             ' >> NON Numero !!'
085600130904     c                   end
085700130904     c                   When      Tipo_Error = 4
085800150717     C                   eval      Msg_Err =
085900150717     C                             ' >> Conver.non riuscita !!'
086000130904     c                   EndSL
086100150717      *
086200150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) + Msg_err
086300150717     c*
086400130904     c                   End
086500130904      **
086600130904     c                   Endsr
086700130904      *  ================================================================== */
086800130904      *   DEFINIZIONE CHIAVI                               *
086900161019      *  ================================================================== */
087000130904     C     *INZSR        BEGSR
087100130904      *
087200130904     c     *ENTRY        PLIST
087300130904     C                   parm                    esito             1
087400130904     C                   eval      esito ='0'
087500150923      *
087600130904     C*------------------
087700130904      * Recupero data e ora
087800130904     C                   TIME                    WORA
087900130904     C                   TIME                    W0140
088000130904      * UDATE IN GGMMAAAA
088100130904     C                   MOVE      W0140         WDTGIO
088200130904      * UDATE IN AAAAMMGG
088300130904     C     *eur          MOVEL     WDTGIO        DATA_eur
088400130904     C     *iso          MOVEL     DATA_eur      dateu
088500150922      *
088600130904     C                   ENDSR
088700130904      *  ------------------------------------------------------------------ */
088800130904      *   Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
088900130904      *  ------------------------------------------------------------------ */
089000130904     C     eMail_alert   BEGSR
089100151217     C*>>>>>>>>
089200151217     c                   if        invia_mail_alert  ='N'
089300151217     c                   leaveSR
089400151217     c                   end
089500151217     C*>>>>>>>>
089600130904     C*
089700130904     C                   eval      wrkEml =
089800130906     C                             'Andrea.Bertocchi@brt.it'
089900130904     C*
090000150720     C                   eval      wrkOgg='PROBLEMI UPLOAD DPD GEODATA'
090100130904     C*
090200130906     C                   call(e)   'TIS701C1'
090300130904     C                   parm                    wrkEml          253
090400130906     C                   parm                    wrkEmlcc        253
090500130904     C                   parm                    wrkOgg           44
090600130904     C                   parm                    wrkMsg         5000
090700130906     C                   parm                    wrkEsito          1
090800130904      *
090900130904     C                   eval      wrkEml =
091000130905     C                             'CedAlert@brt.it'
091100130906     C*  ??????????????????
091200130906     c                   goto      endmsg
091300130906     C*
091400130906     C                   call(e)   'TIS701C1'
091500130904     C                   parm                    wrkEml          253
091600130906     C                   parm                    wrkEmlcc        253
091700130904     C                   parm                    wrkOgg           44
091800130904     C                   parm                    wrkMsg         5000
091900130906     C                   parm                    wrkEsito          1
092000130904      *
092100130906     C     endmsg        ENDSR
092200150715      *  ================================================================== */
092300150716      * *------------------------*
092400150716     C     Syntax        begSR
092500150716      * *------------------------*
092600150716      *
092700150716     c                   movel     dati          TxtInOut
092800150716     c                   eval      ElencoChar = SETA
092900150716     c                   eval      TipoElenco = *blank
093000150716     c                   eval      CharSost   = *blank
093100150716     c                   eval      UpperCase  = '1'
093200150716     c                   eval      ChkNull    = *blank
093300150716     c                   eval      CharNull   = *blank
093400150716     c                   eval      Esito_check= *blank
093500150716      *
093600150716     c                   call      'XCHKCHAR'
093700150716     c                   parm                    TxtInOut
093800150716     c                   parm                    ElencoChar
093900150716     c                   parm                    TipoElenco
094000150716     c                   parm                    CharSost
094100150716     c                   parm                    UpperCase
094200150716     c                   parm                    ChkNull
094300150716     c                   parm                    CharNull
094400150716     c                   parm                    Esito_check
094500150716      *
094600150716      *   se è stato modificato qualcosa nella riga di FLAT
094700150716     c                   if        Esito_check = '1'
094800150716     c                   movel     TxtInOut      dati
094900150716     c                   end
095000150716      *
095100150716     C                   ENDSR
095200150916      *  ------------------------------------------------------------------ */
095300150916      *       X non bloccare in nessun caso il traduttore CLIENTI
095400150916      *  ------------------------------------------------------------------ */
095500150916     C     *pssr         BEGSR
095600150916     C
095700150916     C                   eval      esito = '2'
095800150916     C*
095900161018     C                   eval      wrkMsg='Il File DPD GEODATA TTEVENTS ha avu+
096000161019     C                              to problemi. vedere  UPLOAD ($X) '+
096100151102     C                             'GEODATA  con stato 2: ' + NomeFile
096200161019     c                             + ' da riga : ' + %editc(NRR_riga:'Z') + ' -
096300151103     c                                 riga in errore : '
096400151103     c                             + %editc(Riga_errata:'Z')
096500151103     c
096600150916      **    avviso tramite MAIL
096700150916     C                   exSR      eMail_alert
096800150916      **
096900161019     C                   exSR      SCAN_Errato
097000150916      **
097100150916     C                   COMMIT
097200150916      **
097300150916     C                   ENDSR     '*CANCL'
097400150916      * ?------------------------------------------------------------------ */
097500150916      *?    Flagga il TIVIN con Spedizione ERRATA
097600150916      * ?------------------------------------------------------------------ */
097700161019     C     SCAN_Errato   BEGSR
097800150916      *
097900150916      *
098000150916      *   Scrive su tutti i records l'errore
098100161019     c     NRR_riga      chain     tivin00r
098200150916      *
098300161019     c                   if        %Found(Tivin00r)
098400150916      *
098500161020     C                   eval      vinMSG  = 'PROBLEMI SCAN <-- Riga Errata:'+
098600161020     c                              %Trim(testa_sk(sk))
098700150916      *
098800150916     C                   eval      vinFLG = '2'
098900150916     c                   eval      esito  = '1'
099000150916     c                   eval      se_errore ='S'
099100150916     c                   eval      err_bloccante ='S'
099200150916      *
099300150916     c                   update    tivin000
099400161019     c                   end
099500150916      *
099600150916     C                   ENDSR
099700150916      * ?------------------------------------------------------------------ */
099800130904      *   Skiere definizione Obbligatorierà dei campi
099900130904      *     M=Mandatory    C=Condition    O=Optional
100000130904      *     M=Obbligatorio C=Condizionale O=Facoltativo
100100130904      * e Skiere definizione Alfanumericità o Numericità dei campi A/N
100200161019      * ?------------------------------------------------------------------ */
100300161018**  SK_HED_O
100400161018MMM
100500161018**  SK_HED_N
100600161018AAA
100700161018**  SK_TTE_O
100800170216MMMCCCCCMOOOCOOOCOMCOOCOOOOCCCOOOOOOOOOOOOO
100900161018**  SK_TTE_N
101000161024ANAAANAAAAAAAAAAAANANNANNAAANNNAAANAAAANNAA
