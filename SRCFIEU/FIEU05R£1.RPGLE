000100130903     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200130903     H BNDDIR('QC2LE')
000300130903     H DECEDIT('0,') DATEDIT(*YMD/)
000400130903      ** ************************************************************************
000500130903      *  Da UPLOAD                                                              *
000600130903      *  TRASCODIFICA : MPSEXPDATA / GEODATA                                    *
000700130903      *               BOLLE IMPORT DPD nuovo parcel e depot                     *
000800130903      ** ************************************************************************
000900130904      * Il pgm crea:                                                            *
001000130904      *             FiVAB01L file spedizioni da confermare                      *
001100130904      ** ************************************************************************
001200150916     Ftivin00r  uF   E             DISK    usropn commit  infds(VINds)
001300150715     FFLNUF01L  UF   E           K DISK
001400150717     Ffidpo06l  IF   E           K DISK
001500150717     Ffnorm01L  iF   E           K DISK
001600130904     FTNTBE01L  IF   E           K DISK
001700150714     FTABEL00F  IF   E           K DISK
001800130904     FazORG01L  IF   E           K DISK
001900170925     FfiPND03L  IF   E           K DISK
002000150922      *--
002100150922     Fdpcdp01l  IF   E           K DISK
002200150923     Fdpcve02l  IF   E           K DISK
002300150922     Fdppdc01l  IF   E           K DISK
002400150922      *--
002500160404     Ffnblp01l  IF   E           K DISK
002600150921     Ffnarb01l  IF   E           K DISK
002700170925      *----
002800150717     FTNCLD00F  IF   E           K DISK
002900130904     FFiVAT02L  if   E           K DISK    rename(FIVAT000:FIVAT02)
003000150717     FFiVAB04L  uf   E           K DISK    rename(FIVAB000:FIVAB04) commit
003100130904      *
003200150714     FFiVAB01L  UF A E           K DISK    commit
003300150714     FFiVAT01L  Uf A E           K DISK    commit
003400150908      *
003500150908     FEdiVAT4L  if   E           K DISK    rename(EDIVAT00:EDIVAT4)
003600150908     FEdiVAB3L  UF A E           K DISK    commit
003700150908     FEdiVAT1L  Uf A E           K DISK    commit
003800151030     Fwfieu05F  if A E             DISK
003900160121      *
004000160121     FTIDP200F  UF   E           K DISK
004100160229     Fwfieu00F  uf A E           k DISK
004200150720      ** -------------------------------------------------------- *
004300150916      *
004400150916     d VINds           ds
004500150916     d  nrr_VIN              397    400b 0
004600150916     d NRR_shipm       s                   like(nrr_VIN)
004700151103     d Riga_errata     s                   like(nrr_VIN)
004800150916      ** -------------------------------------------------------- *
004900150720      *  VERSIONE ULTIMA da COMPARARE
005000150720      *
005100150720     D Ultima_VERSIONE...
005200150720     D                 C                   '03.00'
005300130904      ** -------------------------------------------------------- *
005400130904      * schiere sull'obbligatorietà e alfanumericità dei campi delle DS
005500130904      ** -------------------------------------------------------- *
005600150702     D SK_HED_O        S              1    DIM(60) CTDATA PERRCD(60)
005700150702     D SK_HED_N        S              1    DIM(60) CTDATA PERRCD(60)
005800150702     D SK_SHP_O        S              1    DIM(60) CTDATA PERRCD(60)
005900150702     D SK_SHP_N        S              1    DIM(60) CTDATA PERRCD(60)
006000150702     D SK_SND_O        S              1    DIM(60) CTDATA PERRCD(60)
006100150702     D SK_SND_N        S              1    DIM(60) CTDATA PERRCD(60)
006200150702     D SK_RCV_O        S              1    DIM(60) CTDATA PERRCD(60)
006300150702     D SK_RCV_N        S              1    DIM(60) CTDATA PERRCD(60)
006400150702     D SK_PRC_O        S              1    DIM(60) CTDATA PERRCD(60)
006500150702     D SK_PRC_N        S              1    DIM(60) CTDATA PERRCD(60)
006600150702     D SK_MSG_O        S              1    DIM(60) CTDATA PERRCD(60)
006700150702     D SK_MSG_N        S              1    DIM(60) CTDATA PERRCD(60)
006800150907      ** -------------------------------------------------------- *
006900150907     D PAROLE          S             15    DIM(40) CTDATA PERRCD(1)
007000150907      ** -------------------------------------------------------- *
007100130904      *  * ------------------------------------------------------ *
007200130904      *     * NOMI INTESTAZIONE CAMPI x segnalare l'errore su VINMSG
007300130904      *  * ------------------------------------------------------ *
007400160210     D Testa_Sk        s            256    DIM(60)
007500160210     D H_HED           s            256    DIM(60)
007600160210     D H_SHP           s            256    DIM(60)
007700160210     D H_SND           s            256    DIM(60)
007800160210     D H_RCV           s            256    DIM(60)
007900160210     D H_PRC           s            256    DIM(60)
008000160210     D H_MSG           s            256    DIM(60)
008100130904      *
008200130904      ** ------------------------------- *
008300130904      *  obbligatorietà e numericità dei campi
008400130904      ** ------------------------------- *
008500130904     D OBBL            s              1    DIM(60)
008600130904     D NUME            s              1    DIM(60)
008700150716     D UTIL            s              1    DIM(60)
008800150722     D MSGER           s             30    DIM(60)
008900130904      ** ----------------------------------------------- *
009000130904      *  Ds descrittive del dettaglio della spedizione :
009100130904      ** ----------------------------------------------- *
009200150702     D Fieu05RHED    E DS                  prefix(HED_)                          3 campi
009300150702      **
009400130905     D Fieu05RSHP    E DS                  prefix(SHP_)                         28 campi
009500130904      **
009600130905     D Fieu05RSND    E DS                  PREFIX(SND_)                         25 campi
009700130904      **
009800130905     D Fieu05RRCV    E DS                  PREFIX(RCV_)                         27 campi
009900130904      **
010000130905     D Fieu05RPRC    E DS                  PREFIX(PRC_)                         22 campi
010100130904      **
010200130905     D Fieu05RMSG    E DS                  PREFIX(MSG_)                         10 campi
010300130904      *----------------------------------------------------*
010400130904      * Schiere per conversione peso DPD
010500130904      *----------------------------------------------------*
010600130904     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
010700130904     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
010800141211     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
010900150924
011000150924     d Wdata8          DS
011100150924     d  dadata                 1      8  0
011200150924     d  adata                  9     16  0
011300150924     d  GioLav                17     21  0
011400150924      *
011500130904      *-------------------
011600130904     D Tipo_error      S              1  0
011700150716     D Parcel14        S             14
011800150716     D DepotParcel     S              4
011900130904      *-------------------
012000130904     D W015A           S             15
012100130904     D W0140           S             14  0
012200130904     D WORA            S              6  0
012300130904     D XX              S              3  0 INZ
012400130904     D WDTGIO          S              8  0
012500130904     D DATEU           S              8  0
012600130904     D DATA_eur        S               D   DATFMT(*eur)
012700150923     D DATA_iso        S               D   DATFMT(*iso)
012800130904     D KAAA            S                   LIKE(NUFAAA)
012900130904     D KCNU            S                   LIKE(NUFCNU)
013000130904     D KFIL            S                   LIKE(NUFFIL)
013100130904     D NUM_Sped        s                   LIKE(vabnsp)
013200130904     D svkpjbu         s                   like(kpjbu)
013300150701     D nr_SPED         s              7S 0 DIM(999)
013400150717     D kg0_SPED        s              3S 0 DIM(999)
013500150916     D RifNUM          s             15S 0 DIM(999)
013600150925     D tariffa_VTG     s                   like(vabctr)
013700151012     D tariffa_LHS     s                   like(vabctr)
013800130904      *-------------------
013900130904     D KPJBA         E DS
014000130904     D DDPP          E DS
014100150923     D DLPD          E DS
014200130904     D OG143         E DS
014300150925     D tisie2ds      E DS
014400151016     D fieuLPDds     E DS
014500160404     D fnlsi7ds      E DS
014600130904      ** ------------------------------------------------------------------ */
014700170922     d dPNDflo       e ds
014800130904     D DVate         E DS
014900150713     D DVats         E DS
015000150714      * Tabella codici nazioni
015100150714     D C15             S              3    DIM(500)
015200150714     D ISO             S              3    DIM(500)
015300150715     D CIE             S              3    DIM(500)
015400150827      * Tabella Regioni
015500150827     D REG             S             25    DIM(55)
015600130904      ** ------------------------------------------------------------------ */
015700130904      * Numeratore Bolle (302)
015800130904     D trul33ds      E DS
015900130904     D Ds3C          E DS
016000150714     D Ds15          E DS
016100130904     D TISI95DS      E DS
016200150923     D DpdcFLO       E DS
016300130904      *  ================================================================== */
016400130904      *     * Campi da tradurre con interi
016500130904     D atoi            PR            10I 0
016600130904     D                                     EXTPROC('atoi')
016700130904     D  Char                           *
016800130904     D                                     VALUE
016900130904     D                                     OPTIONS(*STRING)
017000130904      *     *--------------------------------------------------------------*
017100130904      *     ( Descrizione Campi x scomposizione FLAT record )
017200130904      *     *--------------------------------------------------------------*
017300130904      *
017400130904      *     * Campi x decodifica * (INPUT  del Record)
017500130904     D  Dati           s           1500
017600130904     D Separa_campi    s              1
017700130904     D Decimal_Separ   s              1
017800130904      *     * Campi decodificati * (OUTPUT del Record)
017900130904      *       Schiere di Output / Campi di Output
018000130904     d  Sk             s              3u 0
018100130904     D    NR_campi     s              3u 0
018200130904     D  Campi_Record   ds
018300160210     D    Dato_sk                   256    DIM(100)
018400130904     D  Campi_Lunghi   ds
018500130904     D    LunDato_sk                  3u 0 DIM(100)
018600130904     D Campi_Numerici  ds
018700130904     D    DatoNum_sk                  1    DIM(100)
018800130904     D Campi_Decimali  ds
018900130904     D    Decimal_sk                  3u 0 DIM(100)
019000130904      *     *--------------------------------------------------------------*
019100150715     d MSG_ServizioMail_Predict...
019200150713     d                 s            100A
019300150715     d MSG_MAIL_come_Predict...
019400150619     d                 s              1A
019500150715     d MSG_ServizioSMS_Predict...
019600150619     d                 s             70A
019700150715     d MSG_SMS_come_Predict...
019800150619     d                 s              1A
019900150715     d EMAIL_no_Predict...
020000150715     d                 s              1A
020100150715     d WRT_MAIL_SMS...
020200150715     d                 s              1A
020300150619      *-----
020400130904     D  position       s              3  0 INZ(0)
020500130904
020600130904     D  se_errore      s              1    inz(' ')
020700130904     D  Msg_Err        s            132    inz(' ')
020800130904     D  Msg_Vin_80     s             80    inz(' ')
020900151102     D  NomeFILE       s            150    inz(' ')
021000130904      *  * ------------------------------------------------------ *
021100151217     d invia_mail_alert...
021200151217     d                 s              1    inz('N')
021300151217      *  * ------------------------------------------------------ *
021400130904     d non_inviato_prima...
021500130904     d                 s              1
021600130904      **
021700130904     D Obbligatorio    C                   'M'
021800130904     D Condizionato    C                   'C'
021900130904     D Facoltativo     C                   'O'
022000130904     D  Numerico       C                   'N'
022100130904     d Normal_Parcel   c                   101
022200130904     d  Small_Parcel   c                   136
022300130904     d Return_Sender   c                   298
022400130904     d Return_Parcel   c                   300
022500130904     d Normal_Parcel_327...
022600130904     d                 c                   327
022700130904     d Normal_Parcel_329...
022800130904     d                 c                   329
022900130904     d Small_Parcel_328...
023000130904     d                 c                   328
023100141211     d Gomme_365       c                   365
023200141211     d Gomme_366       c                   366
023300141211     d Gomme_367       c                   367
023400160121     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
023500150716      * ?------------------------------------------------------------------ */
023600150716      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
023700150716      * ?------------------------------------------------------------------ */
023800150716     D TxtInOut        S           2048
023900150716     D ElencoChar      S            256
024000150716     D TipoElenco      S              1
024100150716     D CharSost        S              1
024200150716     D UpperCase       S              1
024300150716     D ChkNull         S              1
024400150716     D CharNull        S              1
024500150716     D Esito_check     S              1
024600150716      * ?------------------------------------------------------------------ */
024700150716      * ?controllo x CEDINTESA
024800150716     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
024900150916     D                                     3456789 .,-()/=''+:?! %&*;<>#@_°£$')
025000150716     d bart_it         C                   CONST('@Bartolini.it;')
025100150716     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
025200150716     D   Digits        C                   '0123456789'
025300150827
025400150827     D digitsplus      c                   '0123456789 /-'
025500150907     D solosimboli     c                   ' .-_/*+:,§''=\|!?%&$£"ç°#@'
025600150828     D ITALIA          c                   'ITALIA'
025700150828     D ITALY           c                   'ITALY'
025800150828     D IT              c                   'IT'
025900150716     D*------------------
026000150716      * ?set caratteri Maiuscoli/Minuscoli
026100150716     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
026200150716     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
026300130904      *  ================================================================== */
026400130904      *   Ciclo principale
026500130904      *  ********************************************************************/
026600160408     c                   SETON                                        LR
026700130904      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
026800130904     C*
026900130904     C                   if        not %open(tivin00r)
027000130904     C                   open      tivin00r
027100130904     C                   endif
027200130904      **
027300130904      *  ------------------------------------------------------------------ */
027400130904     C*  Controllo dati arrivati da DPD
027500130904      *  ------------------------------------------------------------------ */
027600130904      *  - Occorre fare un primo test sull'integrità della trasmissione
027700130904      *  - controllando che la trasmissione sia completa.
027800130904
027900130904      *                /*---------------------- */
028000130904     c                   exsr      check_Trasm
028100130904      *                /*---------------------- */
028200130904      **
028300130904      **  Errore di trasmissione x tutti i records
028400130904      **   --> file in errore
028500130904     c                   if        se_errore = 'S'
028600150720      *                 /* ------------------ */
028700150720     c                   exsr      TUTTO_ERRATO
028800150720      *                 /* ------------------ */
028900130904     c                   else
029000150720      *  --------------------------------------------- */
029100150720     C*  Se TUTTO OK esegue l'importazione delle Bolle */
029200150720      *  --------------------------------------------- */
029300130904      *                /*---------------------- */
029400130904     c                   exsr      Importa_Msg
029500130904      *                /*---------------------- */
029600130904     c                   end
029700130904
029800150714     c                   COMMIT
029900150714
030000130904     C                   if        %open(tivin00r)
030100130904     C                   close     tivin00r
030200130904     C                   endif
030300130904      *
030400160408     c                   RETURN
030500150720      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
030600150720      *  Tutto il Messaggio in ERRORE                                       */
030700150720      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
030800150720     c     TUTTO_errato  Begsr
030900150720      *
031000150720      ** Messaggio da riportare su ogni record x tutta la trasmissione
031100150720     C                   eval      Msg_Err = 'FILE GEODATA DPD ERRATO +
031200150720     C                              >> o VERSIONE SUPERIORE al TRADUTTORE-
031300150720     C                               << CONTROLLARE !!'
031400150720      *
031500150720      *    Scrive su tutti i records il tipo di errore
031600150720     c     *start        setll     tivin00r
031700150720     c                   read      tivin00r
031800150720     c                   dow       not %eof(tivin00r)
031900150720     C                   eval      vinMSG = Msg_Err
032000150720     C                   eval      vinFLG = '2'
032100150720     c                   eval      esito  = '2'
032200150720     c                   update    tivin000
032300150720     c                   read      tivin00r
032400150720     c                   endDO
032500150720      *
032600150720      **    avviso tramite MAIL
032700150720     C*
032800150720     C                   eval      wrkMsg='Il File DPD GEODATA è arrivato inc'+
032900150720     C                             'ompleto. Controllare UPLOAD ($X) '+
033000150720     C                             'oppure VERSIONE SUPERIORE a quella previst-
033100150720     c                             a nel TRADUTTORE.  -  ' +
033200151102     C                             'GEODATA  con stato 2: '+  nomefile +
033300151102     C                             ' TUTTI i records NON SONO STATI TRADOTTI !'
033400150720     C*
033500150720     C                   exSR      eMail_alert
033600150720      *
033700150720     c                   Endsr
033800150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
033900150715      *  Importa i records della trasmissione                               */
034000150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
034100150715     c     Importa_Msg   Begsr
034200150715
034300150715      * per controllare se almeno un record è stato importato sul VAB
034400150715     c                   clear                   Almeno_Uno        1
034500150715
034600150715     c     *start        setll     Tivin00r
034700150715     c                   read      Tivin00r
034800150715
034900150715     c                   dow       not %eof(Tivin00r)
035000150715
035100150715      * solo i record sflaggati da rielaborare
035200150715     c                   IF        vinFLG = *blank and vinDTA <> *blank
035300150715      ** Controlli formali sui campi
035400150715     c                   clear                   se_errore
035500150715     C                   clear                   Msg_Vin_80
035600150715
035700150715      ** Decodifica record a campi variabili
035800150715      *                /*---------------------- */
035900150715     c                   exsr      Decod_Record
036000150715      *                /*---------------------- */
036100150715
036200150715      **  Se presente un errore nel record emette una segnalazione msg
036300150715     c                   if        se_errore ='S'
036400150715     C                   eval      vinMSG = Msg_Vin_80
036500150715     c                   end
036600150715      *  Sempre Record OK
036700150715     C                   eval      vinFLG = '1'
036800150916     c                   endIF
036900150715
037000150715     c                   if        err_bloccante ='S'
037100150715     C                   eval      vinFLG = '2'
037200150715     c                   eval      esito  = '1'
037300150715     c                   end
037400151022
037500151022     c                   if        gia_in_BLP ='S'
037600151022     c                                and %subst(vindta:1:6) = 'PARCEL'
037700151022     C                   eval      vinMSG = %trim(Vinmsg) + ' ' + 'GEO_in_BLP'
037800151022     c                   elseif    cancellato ='S'
037900151022     c                                and %subst(vindta:1:6) = 'PARCEL'
038000151022     C                   eval      vinMSG = %trim(Vinmsg) + ' ' + 'GEO_doppio'
038100151022     c                   elseif      dati_CONSEGNA ='N'
038200151022     c                   eval      vinmsg = %Trim(vinmsg) + ' ' +
038300151022     c                                  'NO_dati_CONSEGNA'
038400151022     c                   end
038500150715
038600150715     c                   update    Tivin000
038700150720      **
038800150720      **  Se la VERSIONE del messaggio è successiva a questo TRADUTTORE
038900150720      **   deve uscire bloccando tutti i records e avvisando con EMAIL
039000150720      **   di controllare SENZA TRADURRE NULLA.
039100150720     c                   if        HED_VERSION > Ultima_VERSIONE
039200150720     c                   exsr      Tutto_ERRATO
039300150720     c                   leave
039400150720     c                   end
039500150720      **
039600150715     c                   read      Tivin00r
039700150715     C                   ENDdo
039800150715      **
039900150715     c                   endSR
040000130904      *  ------------------------------------------------------------------ */
040100130904      *     Decodifica record a campi variabili
040200130904      *  ------------------------------------------------------------------ */
040300130904     c     Decod_Record  Begsr
040400130904
040500130904      *   Sposta il record a tracciato libero su un campo di lavoro       =
040600150716     c                   clear                   dati
040700150716     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
040800150716     c                   exsr      Syntax
040900150716      **
041000130904     c                   EVAL      err_bloccante =' '
041100130904      **
041200130904      *                /*---------------------- */
041300130904     c                   exsr      Split_Rec
041400130904      *                /*---------------------- */
041500130904      **
041600130904      **  Records di Intestazione e di fine Msg
041700130904     c                   if        %subst(dati:1:1) ='#'
041800130904      *                /*---------------------- */
041900130904     c                   exsr      Rec_Testate
042000130904      *                /*---------------------- */
042100130904     c                   Else
042200130904      ** conta le righe
042300130904      *                /*---------------------- */
042400130904     c                   exsr      Rec_Dettaglio
042500130904      *                /*---------------------- */
042600130904     c                   End
042700130904      **
042800130904     c                   Endsr
042900130904      *  ------------------------------------------------------------------ */
043000130904      *       Record di Testata e di Fine Msg
043100130904      *  ------------------------------------------------------------------ */
043200130904     c     Rec_Testate   Begsr
043300151022      **
043400151022     c                   clear                   vinmsg
043500130904      **
043600130904      ** esempio di record :
043700130904      *     #FILE;GEODATA_SHPNOT_1550_N00_D20130729_T170546
043800130904      *     #ENCODING;ISO 8859-1
043900130904      *     #VERSION;1.7
044000150702      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;
044100150702      *     #DEF;GEODATA:SHIPMENT;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MP
044200150702      *     #DEF;GEODATA:SENDER;NUMORDER;SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPN
044300150702      *     #DEF;GEODATA:RECEIVER;NUMORDER;RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPN
044400150702      *     #DEF;GEODATA:PARCEL;NUMORDER;PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRA
044500150702      *     #DEF;GEODATA:MSG;NUMORDER;NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE;
044600130904      **     .....
044700130904      **      .....
044800130904      *     #END;
044900130904      **
045000130904      *    Prima riga
045100130904     c     'FILE'        SCAN      dati:2        position
045200130904     c                   if        %Found and position < 5
045300130904     C                   move      'S'           Riga_iNIZIo
045400151102     c                   eval      NomeFILE = dati
045500150715     C                   move      *blank        Riga_fINe
045600130904     c                   ElSe
045700130904     c                   End
045800130904      **
045900130904      *    Ultima riga
046000130904     c     'END'         SCAN      dati:2        position
046100130904     c                   if        %Found and position < 5
046200130904     C                   move      'S'           Riga_fINe
046300150715      **
046400150721      * CONTROLLA COME SE DOVESSE FARE UN NUOVO DETTAGLIO
046500150721      **  x l'ultimo giro prima di andare in LR
046600150721     C                   exsr      New_DETTAGLIO
046700150715      **------
046800130904     c                   End
046900130904      **
047000130904      *    DEFINIZIONI  (testate dei campi)
047100130904     c     'DEF'         SCAN      dati:2        position
047200130904      **
047300130904     c                   if        %Found and position < 5
047400130904      **  IMPOSTA I NOMI DEI CAMPI SU SKIERE
047500150702      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;
047600150702      *     #DEF;GEODATA:SHIPMENT;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MP
047700150702      *     #DEF;GEODATA:SENDER;NUMORDER;SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPN
047800150702      *     #DEF;GEODATA:RECEIVER;NUMORDER;RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPN
047900150702      *     #DEF;GEODATA:PARCEL;NUMORDER;PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRA
048000150702      *     #DEF;GEODATA:MSG;NUMORDER;NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE;
048100130904      **
048200130904      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
048300150702      **   eliminando il primo elemento che è sempre "#DEF"
048400150702      *  quindi muove dal 2°campo nella relativa schiera di testata nome campi
048500150702      **
048600130904     c                   IF        %subst(dati:14:6) ='HEADER'                     H_HED
048700150702     c                   movea     Dato_sk(2)    H_HED
048800130904      *
048900130904     c                   ELSEif    %subst(dati:14:8) ='SHIPMENT'                   H_SHP
049000150702     c                   movea     Dato_sk(2)    H_SHP
049100130904      *
049200130904     c                   ELSEif    %subst(dati:14:6) ='SENDER'                     H_SND
049300150702     c                   movea     Dato_sk(2)    H_SND
049400130904      *
049500130904     c                   ELSEif    %subst(dati:14:8) ='RECEIVER'                   H_RCV
049600150702     c                   movea     Dato_sk(2)    H_RCV
049700130904      *
049800130904     c                   ELSEif    %subst(dati:14:6) ='PARCEL'                     H_PRC
049900150702     c                   movea     Dato_sk(2)    H_PRC
050000130904      *
050100130904     c                   ELSEif    %subst(dati:14:3) ='MSG'                        H_MSG
050200150702     c                   movea     Dato_sk(2)    H_MSG
050300130904      *
050400130904     c                   End
050500130904      **
050600130904     c                   End
050700130904      **
050800130904     c                   Endsr
050900130904      *  ------------------------------------------------------------------ */
051000130904      *       Riga Bolla Import da controllare e tradurre
051100130904      *  ------------------------------------------------------------------ */
051200130904     c     Rec_Dettaglio Begsr
051300130904      **
051400130904     c                   clear                   Testa_sk
051500130904     c                   clear                   Msg_Err
051600151103      ** ------------
051700151103     c                   eval      Riga_errata = Riga_errata + 1
051800130904      ** ------------
051900160210     c                   IF        %subst(Dato_sk(1):1:+LunDato_sk(1))
052000130904     c                               ='HEADER'
052100130904     c                   exsr      sr_HEADER
052200130904      ** ------------
052300130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
052400130904     c                               ='SHIPMENT'
052500150717     c                   exsr      new_DETTAGLIO
052600130904     c                   exsr      sr_SHIPMENT
052700130904     ** ------------
052800130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
052900130904     c                               ='SENDER'
053000130904     c                   exsr      sr_SENDER
053100130904      ** ------------
053200130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
053300130904     c                               ='RECEIVER'
053400130904     c                   exsr      sr_RECEIVER
053500130904      ** ------------
053600130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
053700130904     c                               ='PARCEL'
053800130904     c                   exsr      sr_PARCEL
053900130904      ** ------------
054000130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
054100130904     c                               ='MSG'
054200130904     c                   exsr      sr_MSG
054300130904      **
054400130904     c                   end
054500130904      ** ------------
054600130904     c                   Endsr
054700130904      *  ------------------------------------------------------------------ */
054800130904      *      HEADER
054900130904      *  ------------------------------------------------------------------ */
055000130904     c     sr_HEADER     Begsr
055100130904      **
055200150720     c                   clear                   Fieu05RHED
055300130904     c                   movea     H_HED         Testa_sk
055400150702     c                   movea     SK_HED_O      OBBL
055500150702     c                   movea     SK_HED_N      NUME
055600150716     c                   clear                   UTIL
055700150722     c                   clear                   MSGER
055800150703      **
055900150703      **  #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
056000150703      **
056100150703      * cerca in quale elemento si trova il dato
056200160210     c                   clear                   trova_campo     256
056300150702     c                   eval      trova_campo = 'VERSION'
056400150702     c                   z-add     1             sk
056500150702     c     trova_campo   lookup    Testa_sk(sk)                           21
056600150717     c   21              eval      UTIL(sk) ='S'
056700150702     c   21              eval      HED_VERSION    = %subst(Dato_sk(sk):1:+
056800150702     c                                              LunDato_sk(sk))
056900150720      **
057000150720      * Attenzione se il Messaggio ricevuto è successivo all'ultima versione
057100150720      **  da tradurre con questo programma blocca il messaggio e AVVISA.
057200150720     c   21              if        HED_VERSION > Ultima_VERSIONE
057300150720     c                   eval      se_errore ='S'
057400150720     c                   eval      err_bloccante ='S'
057500150720     C                   eval      Msg_Vin_80 = 'VERSIONE SUCCESSIVA al FORMAT-
057600150720     c                                          O previsto in TRADUZIONE !!!'
057700150720     c                   LEAVEsr
057800150720     c                   end
057900150702      *
058000150715     c                   eval      EMAIL_no_Predict = *blank
058100150715     c                   eval      WRT_MAIL_SMS     = *blank
058200130904      **
058300150716     c                   exsr      ctrl_campi
058400150717      **
058500150717     c                   Endsr
058600150717      *  ------------------------------------------------------------------ */
058700150717      *      Rottura di Spedizione - prima della successiva
058800150717      *  ------------------------------------------------------------------ */
058900150717     c     New_DETTAGLIO Begsr
059000150717      **
059100150717      **  Aggiorna il Peso sulle spedizioni della bolla precedente
059200150717      **   Se era una multicollo e NON tutti i colli avevano il peso sul Parcel
059300150717      **    prende il peso totale rimasto lo divide per i colli che non avevano
059400150717      **     il peso ottenendo un peso medio da dichiarare sulla spedizione.
059500150717     c                   if        nr > 1 and Kg_rimasti > 0
059600150717     c                   xfoot     kg0_SPED      quanti_senzaKG    3 0
059700150717     c                   if        quanti_senzaKG > 0
059800150717     c                   exsr      SPALMA_KG
059900150717     c                   end
060000150717     c                   end
060100150717      **
060200150717      * Inoltre x particolari comunicazioni come PREDICT
060300150717     c                   IF          WRT_MAIL_SMS  = 'S'
060400150916     c                   if         MSG_MAIL_come_Predict ='S'
060500150916     c                               or
060600150916     c                              MSG_SMS_come_Predict  ='S'
060700150916     c                               or
060800150916     c                              EMAIL_no_Predict      ='S'
060900150916      *
061000160217      *** NON SI VUOLE CREARE IL RESO (non modifico le vecchie spec.NON si sa mai)
061100160217      ***   SALTIAMO semplicemente la scrittura  anche dei MSG su VAT
061200160217      ***
061300160217     c                   If        Sped_Reso <>'S'
061400160217      ***                          ==========
061500150717     C                   exsr      WRITE_MSG
061600160217      ***                          ==========
061700160217     C                   endIF
061800160217      ***
061900150916     c                   end
062000150916      *
062100150717     c                   END
062200150717     c                   eval      WRT_MAIL_SMS     = *blank
062300150717     c                   eval      EMAIL_no_Predict = *blank
062400150717      **------
062500150717     c                   eval      WRT_MAIL_SMS = *blank
062600150717     c                   eval      MSG_ServizioMail_Predict = *blank
062700150717     c                   eval      MSG_ServizioSMS_Predict  = *blank
062800150717     c                   eval      MSG_MAIL_come_Predict = *blank
062900150717     c                   eval      MSG_SMS_come_Predict  = *blank
063000150717      **
063100150921     c                   clear                   edivab00
063200150717     c                   clear                   fivab000
063300150717     c                   clear                   nr_SPED
063400150717     c                   clear                   kg0_SPED
063500150717     c                   clear                   nr                5 0
063600150717     c                   clear                   Kg_rimasti        9 1
063700150923     c                   clear                   su_EDIVAB         1
063800151022     c                   clear                   dati_CONSEGNA     1
063900150716      **
064000130904     c                   Endsr
064100150717      *  ------------------------------------------------------------------ */
064200150717      *      SPALMA il peso residuo sui colli rimasti senza
064300150717      *  ------------------------------------------------------------------ */
064400150717     c     SPALMA_KG     Begsr
064500150717      **
064600150717     c     Kg_rimasti    div       quanti_senzaKGPeso_medio        7 1
064700150717      **
064800150717     c                   do        nr            wrt               5 0
064900150717     c                   if        nr_sped(wrt) = 0
065000150717     c                   leave
065100150717     c                   end
065200150717      **
065300150717     c                   if        kg0_sped(wrt) = 1
065400150717     c                   z-add     nr_sped(wrt)  vabNSP
065500150717     c     kvab04        chain     FiVAB04L
065600150717     c                   if        %Found(FIVAB04L)
065700150717     c                   z-add     peso_medio    vabPKB
065800150916      **
065900150916      *
066000150916      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
066100150916     c                   if        vabPKB > 31,5
066200150916     c                   eval           vabPKB = 30
066300150916     c                   EndIF
066400150916      **
066500150717     C                   UPDATE    fiVAB04
066600150717     c                   end
066700150717     c                   end
066800150717     c                   enddo
066900150717      *-----
067000150717     c                   Endsr
067100130904      *  ------------------------------------------------------------------ */
067200130904      *      SHIPMENT
067300130904      *  ------------------------------------------------------------------ */
067400130904     c     sr_SHIPMENT   Begsr
067500130904      **
067600130905     c                   clear                   fieu05RSHP
067700130904     c                   movea     H_SHP         Testa_sk
067800150702     c                   movea     SK_SHP_O      OBBL
067900150702     c                   movea     SK_SHP_N      NUME
068000150716     c                   clear                   UTIL
068100150722     c                   clear                   MSGER
068200150916     c                   eval      NRR_shipm = NRR_VIN
068300151103     c                   eval      Riga_errata=NRR_VIN
068400150703      **
068500150703      *  #DEF;GEODATA:SHIPMENT;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCREF1;
068600150703      *  MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;MPSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;
068700150703      *  CDATE;CTIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME;SPTREALDATE;SPTREALTIME;
068800150703      *  DELMODALLOW;ROUTINGPLANVERSION;SPARTNERREF1;SPARTNERREF2;SPARTNERCODE;OSORT;
068900150703      *  SSORT;MSORT;COLREQUESTFLAG;ROUTINGPLACE;;
069000150921      **
069100160210     c                   clear                   trova_campo
069200150921     c                   eval      trova_campo = 'CDATE'
069300150921     c                   z-add     1             sk
069400150921     c     trova_campo   lookup    Testa_sk(sk)                           21
069500150921     c   21              eval      SHP_CDATE = atoi(%TRIM(Dato_sk(sk)))
069600150921     c   21              eval      UTIL(sk) ='S'
069700150921      **
069800160210     c                   clear                   trova_campo
069900150922     c                   eval      trova_campo = 'SDEPOT'
070000150922     c                   z-add     1             sk
070100150922     c     trova_campo   lookup    Testa_sk(sk)                           21
070200150922     c   21              eval      SHP_SDEPOT = %subst(Dato_sk(sk):1:+
070300150922     c                                              LunDato_sk(sk))
070400150922     c   21              eval      UTIL(sk) ='S'
070500150703      **
070600160210     c                   clear                   trova_campo
070700150703     c                   eval      trova_campo = 'MPSCREF1'
070800150703     c                   z-add     1             sk
070900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
071000150703     c   21              eval      SHP_MPSCREF1 = %subst(Dato_sk(sk):1:+
071100150703     c                                              LunDato_sk(sk))
071200150717     c   21              eval      UTIL(sk) ='S'
071300150703      **
071400160210     c                   clear                   trova_campo
071500150703     c                   eval      trova_campo = 'MPSCOUNT'
071600150703     c                   z-add     1             sk
071700150703     c     trova_campo   lookup    Testa_sk(sk)                           21
071800150703     c   21              eval      SHP_MPSCOUNT = atoi(%TRIM(Dato_sk(sk)))
071900150717     c   21              z-add     SHP_MPSCOUNT  multicollo
072000150717     c   21              eval      UTIL(sk) ='S'
072100150722     c   21              if        SHP_MPSCOUNT > 1
072200150722     c                   eval      MSGER(sk) ='Sped.MULTICOLLO'
072300150722     c                   end
072400150703      **
072500160210     c                   clear                   trova_campo
072600150703     c                   eval      trova_campo = 'MPSVOLUME'
072700150703     c                   z-add     1             sk
072800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
072900150703     c   21              eval      SHP_MPSVOLUME = atoi(%TRIM(Dato_sk(sk)))
073000150703      **
073100160210     c                   clear                   trova_campo
073200150703     c                   eval      trova_campo = 'MPSWEIGHT'
073300150703     c                   z-add     1             sk
073400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
073500150703     c   21              eval      SHP_MPSWEIGHT = atoi(%TRIM(Dato_sk(sk)))
073600150716     c   21              eval      UTIL(sk) ='S'
073700150717     c   21SHP_MPSWEIGHT div       100           Kg_rimasti
073800150716      **
073900150716     c                   exsr      ctrl_campi
074000150716      **
074100130904     c                   Endsr
074200130904      *  ------------------------------------------------------------------ */
074300130904      *      SENDER
074400130904      *  ------------------------------------------------------------------ */
074500130904     c     sr_SENDER     Begsr
074600130904      **
074700130905     c                   clear                   fieu05RSND
074800130904     c                   movea     H_SND         Testa_sk
074900150702     c                   movea     SK_SND_O      OBBL
075000150702     c                   movea     SK_SND_N      NUME
075100150716     c                   clear                   UTIL
075200150722     c                   clear                   MSGER
075300150703      **
075400150703      *  #DEF;GEODATA:SENDER;NUMORDER;SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME;
075500150703      *  SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNUM;SADD2;SADD3;SFLOOR;SBUILDING;
075600150703      *  SDEPARTMENT;SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTACT;SPHONE;SFAX;
075700150703      *  SEMAIL;SCOMMENT;SGLN;SGPSLAT;SGPSLONG;SACCOUNTOWNER;;
075800130904      **
075900160210     c                   clear                   trova_campo
076000150703     c                   eval      trova_campo = 'SNAME1'
076100150703     c                   z-add     1             sk
076200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
076300150703     c   21              eval      SND_SNAME1 = %subst(Dato_sk(sk):1:+
076400150703     c                                              LunDato_sk(sk))
076500150716     c   21              eval      UTIL(sk) ='S'
076600150703      **
076700160210     c                   clear                   trova_campo
076800150703     c                   eval      trova_campo = 'SCOUNTRYCODE'
076900150703     c                   z-add     1             sk
077000150703     c     trova_campo   lookup    Testa_sk(sk)                           21
077100150703     c   21              eval      SND_SCOUNTRYCD
077200150703     c                                        = %subst(Dato_sk(sk):1:+
077300150703     c                                              LunDato_sk(sk))
077400150716     c   21              eval      UTIL(sk) ='S'
077500150722     c   21              if        SND_SCOUNTRYCD = *blank
077600150722     c                   eval      MSGER(sk) ='NO Naz.Mitt.Or.'
077700150722     c                   end
077800150703      **
077900160210     c                   clear                   trova_campo
078000150703     c                   eval      trova_campo = 'SZIPCODE'
078100150703     c                   z-add     1             sk
078200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
078300150703     c   21              eval      SND_SZIPCODE = %subst(Dato_sk(sk):1:+
078400150703     c                                              LunDato_sk(sk))
078500150716     c   21              eval      UTIL(sk) ='S'
078600150722     c   21              if        SND_SZIPCODE = *blank
078700150722     c                   eval      MSGER(sk) ='NO CAP Mitt.Or.'
078800150722     c                   end
078900150703      **
079000150716     c                   exsr      ctrl_campi
079100150716      **
079200130904     c                   Endsr
079300130904      *  ------------------------------------------------------------------ */
079400130904      *      RECEIVER
079500130904      *  ------------------------------------------------------------------ */
079600130904     c     sr_RECEIVER   Begsr
079700130904      **
079800130905     c                   clear                   fieu05RRCV
079900130904     c                   movea     H_RCV         Testa_sk
080000150702     c                   movea     SK_RCV_O      OBBL
080100150702     c                   movea     SK_RCV_N      NUME
080200150716     c                   clear                   UTIL
080300150722     c                   clear                   MSGER
080400130904      **
080500150703      *  #DEF;GEODATA:RECEIVER;NUMORDER;RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2;
080600150703      *  RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLOOR;RBUILDING;RDEPARTMENT;
080700150703      *  RCOUNTRYCODE;RSTATE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONTACTPHO1;
080800150703      *  RCONTACTPHO2;RINTERPHONENAME;RFAX;REMAIL;RCOMMENT;RGPSLAT;RGPSLONG;;
080900150703      **
081000160210     c                   clear                   trova_campo
081100150703     c                   eval      trova_campo = 'RNAME1'
081200150703     c                   z-add     1             sk
081300150703     c     trova_campo   lookup    Testa_sk(sk)                           21
081400150703     c   21              eval      RCV_RNAME1 = %subst(Dato_sk(sk):1:+
081500150703     c                                              LunDato_sk(sk))
081600150716     c   21              eval      UTIL(sk) ='S'
081700150703      **
081800160210     c                   clear                   trova_campo
081900150703     c                   eval      trova_campo = 'RNAME2'
082000150703     c                   z-add     1             sk
082100150703     c     trova_campo   lookup    Testa_sk(sk)                           21
082200150827     c   21              eval      RCV_RNAME2 = %subst(Dato_sk(sk):1:+
082300150703     c                                              LunDato_sk(sk))
082400150827     c   21              eval      UTIL(sk) ='S'
082500150827      **
082600160210     c                   clear                   trova_campo
082700150827     c                   eval      trova_campo = 'RCOMPNAME'
082800150827     c                   z-add     1             sk
082900150827     c     trova_campo   lookup    Testa_sk(sk)                           21
083000150827     c   21              eval      RCV_RCOMPNAME  = %subst(Dato_sk(sk):1:+
083100150827     c                                              LunDato_sk(sk))
083200150827     c   21              eval      UTIL(sk) ='S'
083300150703      **
083400160210     c                   clear                   trova_campo
083500150703     c                   eval      trova_campo = 'RSTREET'
083600150703     c                   z-add     1             sk
083700150703     c     trova_campo   lookup    Testa_sk(sk)                           21
083800150703     c   21              eval      RCV_RSTREET = %subst(Dato_sk(sk):1:+
083900150703     c                                              LunDato_sk(sk))
084000150716     c   21              eval      UTIL(sk) ='S'
084100150827      **
084200160210     c                   clear                   trova_campo
084300150827     c                   eval      trova_campo = 'RPROPNUM'
084400150827     c                   z-add     1             sk
084500150827     c     trova_campo   lookup    Testa_sk(sk)                           21
084600150827     c   21              eval      RCV_RPROPNUM  = %subst(Dato_sk(sk):1:+
084700150827     c                                              LunDato_sk(sk))
084800150827     c   21              eval      UTIL(sk) ='S'
084900150703      **
085000160210     c                   clear                   trova_campo
085100150703     c                   eval      trova_campo = 'RADD2'
085200150703     c                   z-add     1             sk
085300150703     c     trova_campo   lookup    Testa_sk(sk)                           21
085400150703     c   21              eval      RCV_RADD2  = %subst(Dato_sk(sk):1:+
085500150703     c                                              LunDato_sk(sk))
085600150716     c   21              eval      UTIL(sk) ='S'
085700150827      **
085800160210     c                   clear                   trova_campo
085900150827     c                   eval      trova_campo = 'RADD3'
086000150827     c                   z-add     1             sk
086100150827     c     trova_campo   lookup    Testa_sk(sk)                           21
086200150827     c   21              eval      RCV_RADD3  = %subst(Dato_sk(sk):1:+
086300150827     c                                              LunDato_sk(sk))
086400150827     c   21              eval      UTIL(sk) ='S'
086500150703      **
086600160210     c                   clear                   trova_campo
086700150703     c                   eval      trova_campo = 'RCOUNTRYCODE'
086800150703     c                   z-add     1             sk
086900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
087000150703     c   21              eval      RCV_RCOUNTRYCD
087100150703     c                                        = %subst(Dato_sk(sk):1:+
087200150703     c                                              LunDato_sk(sk))
087300150716     c   21              eval      UTIL(sk) ='S'
087400150703      **
087500160210     c                   clear                   trova_campo
087600150703     c                   eval      trova_campo = 'RSTATE'
087700150703     c                   z-add     1             sk
087800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
087900150703     c   21              eval      RCV_RSTATE = %subst(Dato_sk(sk):1:+
088000150703     c                                              LunDato_sk(sk))
088100150716     c   21              eval      UTIL(sk) ='S'
088200150703      **
088300160210     c                   clear                   trova_campo
088400150703     c                   eval      trova_campo = 'RZIPCODE'
088500150703     c                   z-add     1             sk
088600150703     c     trova_campo   lookup    Testa_sk(sk)                           21
088700150703     c   21              eval      RCV_RZIPCODE = %subst(Dato_sk(sk):1:+
088800150703     c                                              LunDato_sk(sk))
088900150716     c   21              eval      UTIL(sk) ='S'
089000150703      **
089100160210     c                   clear                   trova_campo
089200150703     c                   eval      trova_campo = 'RTOWN'
089300150703     c                   z-add     1             sk
089400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
089500150827     c   21              eval      RCV_RTOWN  = %subst(Dato_sk(sk):1:+
089600150703     c                                              LunDato_sk(sk))
089700150827     c   21              eval      UTIL(sk) ='S'
089800150828      **
089900160210     c                   clear                   trova_campo
090000150703     c                   eval      trova_campo = 'RCONTACT'
090100150703     c                   z-add     1             sk
090200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
090300150703     c   21              eval      RCV_RCONTACT = %subst(Dato_sk(sk):1:+
090400150703     c                                              LunDato_sk(sk))
090500150716     c   21              eval      UTIL(sk) ='S'
090600150703      **
090700160210     c                   clear                   trova_campo
090800150703     c                   eval      trova_campo = 'RCONTACTPHO1'
090900150703     c                   z-add     1             sk
091000150703     c     trova_campo   lookup    Testa_sk(sk)                           21
091100150703     c   21              eval      RCV_RCONTPHO1 = %subst(Dato_sk(sk):1:+
091200150703     c                                              LunDato_sk(sk))
091300150716     c   21              eval      UTIL(sk) ='S'
091400150703      **
091500160210     c                   clear                   trova_campo
091600150703     c                   eval      trova_campo = 'RFAX'
091700150703     c                   z-add     1             sk
091800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
091900150703     c   21              eval      RCV_RFAX   = %subst(Dato_sk(sk):1:+
092000150703     c                                              LunDato_sk(sk))
092100150716     c   21              eval      UTIL(sk) ='S'
092200150703      **
092300160210     c                   clear                   trova_campo
092400150703     c                   eval      trova_campo = 'REMAIL'
092500150703     c                   z-add     1             sk
092600150703     c     trova_campo   lookup    Testa_sk(sk)                           21
092700150703     c   21              eval      RCV_REMAIL = %subst(Dato_sk(sk):1:+
092800150703     c                                              LunDato_sk(sk))
092900150716     c   21              eval      UTIL(sk) ='S'
093000150715      * c'è l'indirizzo mail per eventuali avvisi se non ci sarà l'email x il predict
093100150715     c                   if        RCV_REMAIL <> *blank
093200150715     c                   eval      EMAIL_no_Predict = 'S'
093300150715     c                   eval      WRT_MAIL_SMS = 'S'
093400150715     c                   end
093500130904      **
093600150716     c                   exsr      ctrl_campi
093700150716      **
093800150828      **--------
093900150828      * AGGIUSTAMENTI  per contenuti NON IDONEI alla traduzione
094000150828      **--------
094100150828      ** CAP:
094200150828      ** se viene passato un numero inferiore di 5 cifre
094300150916     c**********         IF          RCV_RZIPCODE <> *blank
094400160225     c                   if        %len(%trim(RCV_RZIPCODE)) < 5   and
094500160225     c                             %len(%trim(RCV_RZIPCODE)) > 0
094600150828     c                   movel     '00000'       campo5            5            C.A.P.
094700150828     c                   eval      %subst(campo5
094800150828     c                             :5-%len(%trim(RCV_RZIPCODE))+1
094900150828     c                             :%len(%trim(RCV_RZIPCODE)) )
095000150828     c                                =  %subst(RCV_RZIPCODE
095100150828     c                             :1:%len(%trim(RCV_RZIPCODE)) )
095200150828     c                   eval       RCV_RZIPCODE = campo5
095300150828     c                   end
095400150916     c***********        endIF
095500150828      **
095600150828      **  se la Località inizia con il CAP lo tolgo e allineao a sinistra Città
095700150828     c                   if        %subst(RCV_RZIPCODE:1:5) =
095800150828     c                             %subst(RCV_RTOWN:1:5)
095900150828     c                   eval      %subst(RCV_RTOWN:1:5) = *blank
096000150828     c                   eval      RCV_RTOWN = %trim(RCV_RTOWN)
096100150828     c                   end
096200150828      **
096300150828     c                   if        %subst(RCV_RZIPCODE:1:5) =
096400150828     c                             %subst(RCV_RADD2:1:5)
096500150828     c                   eval      %subst(RCV_RADD2:1:5) = *blank
096600150828     c                   eval      RCV_RADD2 = %trim(RCV_RADD2)
096700150828     c                   end
096800150828      **
096900150828     c                   if        %subst(RCV_RZIPCODE:1:5) =
097000150828     c                             %subst(RCV_RADD3:1:5)
097100150828     c                   eval      %subst(RCV_RADD3:1:5) = *blank
097200150828     c                   eval      RCV_RADD3 = %trim(RCV_RADD3)
097300150828     c                   end
097400150828      **
097500150828     c                   if         RCV_RNAME1 = *BLANK  and
097600150828     c                              RCV_RNAME2 <>*BLANK
097700150828     c                   eval       RCV_RNAME1 = RCV_RNAME2
097800150828     c                   clear                   RCV_RNAME2
097900150828      **
098000150828     c                   elseif     RCV_RNAME1 = RCV_RNAME2
098100150828     c                   clear                   RCV_RNAME2
098200150828     c                   end
098300150828      **
098400150828     c                   if         RCV_RNAME1 = RCV_RCOMPNAME
098500150828     c                   clear                   RCV_RCOMPNAME
098600150828     c                   end
098700150828      **
098800150828     c                   if         RCV_RNAME1 = RCV_RSTREET
098900150828     c                   clear                   RCV_RSTREET
099000150828     c                   end
099100150828      **
099200150828     c                   if         RCV_RNAME1 = RCV_RTOWN
099300150828     c                   clear                   RCV_RTOWN
099400150828     c                   end
099500150828      **
099600150828     c                   if         RCV_RADD2  = RCV_RADD3
099700150828     c                   clear                   RCV_RADD3
099800150828     c                   end
099900150828      **
100000150828     c                   if         RCV_RTOWN  = RCV_RADD2
100100150828     c                   clear                   RCV_RADD2
100200150828     c                   end
100300150828      **
100400150828     c                   if         RCV_RTOWN  = RCV_RADD3
100500150828     c                   clear                   RCV_RADD3
100600150828     c                   end
100700150828      **
100800150828      ** se il contenuto  dei campi sono (-_./ o blank) pulisco la colonna
100900150828      **  oppure è il nome di una regione
101000150828      **  oppure c'è in 3 modi diversi ITALIA
101100150828    2c                   if        %check(solosimboli:RCV_RNAME1)=0
101200150828     c                               or %lookup(RCV_RNAME1:reg)>0
101300150828     c                               or RCV_RNAME1 = ITALIA
101400150828     c                               or RCV_RNAME1 = ITALY
101500150828     c                               or RCV_RNAME1 = IT
101600150828     c                   clear                   RCV_RNAME1
101700150828     c                   end
101800150828      **
101900150828    2c                   if        %check(solosimboli:RCV_RNAME2)=0
102000150828     c                               or %lookup(RCV_RNAME2:reg)>0
102100150828     c                               or RCV_RNAME2 = ITALIA
102200150828     c                               or RCV_RNAME2 = ITALY
102300150828     c                               or RCV_RNAME2 = IT
102400150828     c                   clear                   RCV_RNAME2
102500150828     c                   end
102600150828      **
102700150828    2c                   if        %check(solosimboli:RCV_RCOMPNAME)=0
102800150828     c                               or %lookup(RCV_RCOMPNAME:reg)>0
102900150828     c                               or RCV_RCOMPNAME = ITALIA
103000150828     c                               or RCV_RCOMPNAME = ITALY
103100150828     c                               or RCV_RCOMPNAME = IT
103200150828     c                   clear                   RCV_RCOMPNAME
103300150828     c                   end
103400150828      **
103500150828    2c                   if        %check(solosimboli:RCV_RSTREET)=0
103600150828     c                               or %lookup(RCV_RSTREET:reg)>0
103700150828     c                               or RCV_RSTREET = ITALIA
103800150828     c                               or RCV_RSTREET = ITALY
103900150828     c                               or RCV_RSTREET = IT
104000150828     c                   clear                   RCV_RSTREET
104100150828     c                   end
104200150828      **
104300150828    2c                   if        %check(solosimboli:RCV_RPROPNUM)=0
104400150828     c                               or %lookup(RCV_RPROPNUM:reg)>0
104500150828     c                               or RCV_RPROPNUM = ITALIA
104600150828     c                               or RCV_RPROPNUM = ITALY
104700150828     c                               or RCV_RPROPNUM = IT
104800150828     c                   clear                   RCV_RPROPNUM
104900150828     c                   end
105000150828      **
105100150828    2c                   if        %check(solosimboli:RCV_RADD2)=0
105200150828     c                               or %lookup(RCV_RADD2:reg)>0
105300150828     c                               or RCV_RADD2 = ITALIA
105400150828     c                               or RCV_RADD2 = ITALY
105500150828     c                               or RCV_RADD2 = IT
105600150828     c                   clear                   RCV_RADD2
105700150828     c                   end
105800150828      **
105900150828    2c                   if        %check(solosimboli:RCV_RADD3)=0
106000150828     c                               or %lookup(RCV_RADD3:reg)>0
106100150828     c                               or RCV_RADD3 = ITALIA
106200150828     c                               or RCV_RADD3 = ITALY
106300150828     c                               or RCV_RADD3 = IT
106400150828     c                   clear                   RCV_RADD3
106500150828     c                   end
106600150828      **
106700150828    2c                   if        %check(solosimboli:RCV_RTOWN)=0
106800150828     c                               or %lookup(RCV_RTOWN:reg)>0
106900150828     c                               or RCV_RTOWN = ITALIA
107000150828     c                               or RCV_RTOWN = ITALY
107100150828     c                               or RCV_RTOWN = IT
107200150828      **
107300150828     c                               or RCV_RTOWN = RCV_RZIPCODE
107400150828     c                   clear                   RCV_RTOWN
107500150828     c                   end
107600150828      **----
107700150908     c                   clear                   totparole         3 0
107800150908     c                   eval      totparole = %elem(Parole)
107900150908      *
108000150908      **  Controllo tab:"parole"
108100150908      *     Verifica se l'inizio del contenuto del campo corrisponde ad una delle
108200150908      *     parole elencate nella tabella in fondo al programma per individuare se
108300150908      *     si tratta dell'indirizzo.
108400150908      *
108500150908      ** Sulla RTOWN
108600150908     c                   do        totparole     P                 3 0
108700150908      *   se finito l'elenco ...esce
108800150908     c                   if        Parole(P) = *blank
108900150908     c                   leave
109000150908     c                   end
109100150908      *
109200150908      *  verifica l'inizio del campo
109300150908     c                   if        %subst(RCV_RTOWN:1:%len(%Trim(PAROLE(P)))+1)
109400150908     c                               =
109500150908     c                             %subst(PAROLE(P):1:%len(%Trim(PAROLE(P)))+1)
109600150908      *
109700150908     c                   if         RCV_RNAME2 = *blank
109800150908     c                   eval       RCV_RNAME2 = RCV_RSTREET
109900150908     c                   eval       RCV_RSTREET= RCV_RTOWN
110000150908     c                   clear                   RCV_RTOWN
110100150908     c                   else
110200150908     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
110300150908     c                             + ' ' + RCV_RTOWN
110400150908     c                   clear                   RCV_RTOWN
110500150908     c                   end
110600150908      *
110700150908     c                   end
110800150908      *
110900150908     c                   enddo
111000150908      **----
111100150908      ** Sulla RADD2
111200150908     c                   do        totparole     P
111300150908      *   se finito l'elenco ...esce
111400150908     c                   if        Parole(P) = *blank
111500150908     c                   leave
111600150908     c                   end
111700150908      *
111800150908      *  verifica l'inizio del campo
111900150908     c                   if        %subst(RCV_RADD2:1:%len(%Trim(PAROLE(P)))+1)
112000150908     c                               =
112100150908     c                             %subst(PAROLE(P):1:%len(%Trim(PAROLE(P)))+1)
112200150908      *
112300150908     c                   if         RCV_RNAME2 = *blank
112400150908     c                   eval       RCV_RNAME2 = RCV_RSTREET
112500150908     c                   eval       RCV_RSTREET= RCV_RADD2
112600150908     c                   clear                   RCV_RADD2
112700150908     c                   else
112800150908     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
112900150908     c                             + ' ' + RCV_RADD2
113000150908     c                   clear                   RCV_RADD2
113100150908     c                   end
113200150908      *
113300150908     c                   end
113400150908      *
113500150908     c                   enddo
113600150908      **----
113700150908      ** Sulla RADD3
113800150908     c                   do        totparole     P
113900150908      *   se finito l'elenco ...esce
114000150908     c                   if        Parole(P) = *blank
114100150908     c                   leave
114200150908     c                   end
114300150908      *
114400150908      *  verifica l'inizio del campo
114500150908     c                   if        %subst(RCV_RADD3:1:%len(%Trim(PAROLE(P)))+1)
114600150908     c                               =
114700150908     c                             %subst(PAROLE(P):1:%len(%Trim(PAROLE(P)))+1)
114800150908      *
114900150908     c                   if         RCV_RNAME2 = *blank
115000150908     c                   eval       RCV_RNAME2 = RCV_RSTREET
115100150908     c                   eval       RCV_RSTREET= RCV_RADD3
115200150908     c                   clear                   RCV_RADD3
115300150908     c                   else
115400150908     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
115500150908     c                             + ' ' + RCV_RADD3
115600150908     c                   clear                   RCV_RADD3
115700150908     c                   end
115800150908      *
115900150908     c                   end
116000150908      *
116100150908     c                   enddo
116200150828      **----
116300150828     c                   if        %subst(RCV_RADD2:1:3)='C/O'  or
116400150828     c                             %subst(RCV_RADD2:1:6)='PRESSO'
116500150828     c                   if         RCV_RNAME2 = *blank
116600150828     c                   eval       RCV_RNAME2 = RCV_RADD2
116700150828     c                   clear                   RCV_RADD2
116800150828     c                   else
116900150828     c                   eval      RCV_RNAME1  = %trim(RCV_RNAME1)
117000150828     c                             + ' ' + RCV_RADD2
117100150828     c                   clear                   RCV_RADD2
117200150828     c                   end
117300150828     c                   end
117400150828      **----
117500150828     c                   if        %len(%trim(RCV_RSTREET)) < 6 or
117600150828     c                              RCV_RSTREET = *BLANK
117700150828     c                   if         RCV_RADD2 <> *BLANK
117800150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
117900150828     c                              + ' ' +
118000150828     c                             %trim(RCV_RADD2)
118100150828     c                   clear                   RCV_RADD2
118200150828    2c                   endif
118300150828    2c                   endif
118400150828      **----
118500150828     c* Se la località è un numero la unisco all'indirizzo e la pulisco
118600150828    2c                   if        %check(digitsplus:RCV_RTOWN)=0
118700150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
118800150828     c                             + ' ' + RCV_RTOWN
118900150828     c                   clear                   RCV_RTOWN
119000150828      **
119100150828      * se la località inizia per numero, per non perderlo lo unisco all'indirizzo
119200150828   x2c                   else
119300150828      **
119400150828   2ac                   if        %subst(RCV_RSTREET:33:3)='   '
119500150828     c                   z-add     1             xx
119600150828    3c                   dow       %check(digitsplus:
119700150828    3c                             %subst(RCV_RTOWN:xx:1))=0
119800150828    4c                   if        xx=1
119900150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
120000150828     c                             + '-' + %subst(RCV_RTOWN:xx:1)
120100150828   x4c                   else
120200150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
120300150828     c                             + %subst(RCV_RTOWN:xx:1)
120400150828    4c                   endif
120500150828     c                   eval      %subst(RCV_RTOWN:xx:1) = *blank
120600150828     c
120700150828     c                   add       1             xx
120800150828    4c                   if        xx>35
120900150828     c                   leave
121000150828    4c                   endif
121100150828    3c                   enddo
121200150828      * alla fine compatta la località a sinistra
121300150828     c                   eval      RCV_RTOWN = %trim(RCV_RTOWN)
121400150828   2ac                   endif
121500150828     c
121600150828    2c                   endif
121700150828      **----
121800150828     c* Se la località è vuota e ADD2 o ADD3 sono pieni
121900150828    2c                   if        RCV_RTOWN = *blank   and
122000150828    2c                             RCV_RADD3 <>*blank
122100150828     c                   eval      RCV_RTOWN = RCV_RADD3
122200150828      *
122300150828    2c                   elseif    RCV_RTOWN = *blank   and
122400150828    2c                             RCV_RADD2 <>*blank
122500150828     c                   eval      RCV_RTOWN = RCV_RADD2
122600150828    2c                   endif
122700150828      **
122800150828      **----
122900150828      **  il numero civico se c'è lo accoda alla via
123000150828     c                   if        RCV_RPROPNUM <> *blank
123100150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
123200150828     c                              + ' ' +
123300150828     c                             %trim(RCV_RPROPNUM)
123400150828     c                   end
123500150910      **----
123600150910      **  La COMPNAME potrebbe essere carica e servire
123700150910     c                   if        RCV_RCOMPNAME <> *blank
123800150910     c                   if        RCV_RNAME1 =     *blank
123900150910     c                   eval      RCV_RNAME1 = RCV_RCOMPNAME
124000150910     c                   elseif    RCV_RNAME2 =     *blank
124100150910     c                   eval      RCV_RNAME2 = RCV_RCOMPNAME
124200150910     c                   elseif    RCV_RSTREET =     *blank
124300150910     c                   eval      RCV_RSTREET = RCV_RCOMPNAME
124400150910     c                   elseif    RCV_RTOWN =       *blank
124500150910     c                   eval      RCV_RTOWN =   RCV_RCOMPNAME
124600150910     c                   end
124700150910     c                   end
124800150828      **----
124900150828      **
125000150831     c     end_rcv       Endsr
125100130904      *  ------------------------------------------------------------------ */
125200130904      *      PARCEL
125300130904      *  ------------------------------------------------------------------ */
125400130904     c     sr_PARCEL     Begsr
125500130904      **
125600130905     c                   clear                   fieu05RPRC
125700130904     c                   movea     H_PRC         Testa_sk
125800150702     c                   movea     SK_PRC_O      OBBL
125900150702     c                   movea     SK_PRC_N      NUME
126000150716     c                   clear                   UTIL
126100150722     c                   clear                   MSGER
126200130904      **
126300150703      *  #DEF;GEODATA:PARCEL;NUMORDER;PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;
126400150703      *  SENDPARCELREF1;SENDPARCELREF2;SENDPARCELREF3;SENDPARCELREF4;RECPARCELREF;
126500150703      *  SERVICECODE;PPARTNERREF1;PPARTNERREF2;PPARTNERREF3;PPARTNERREF4;DIMENSION;
126600150703      *  DECLAREDWEIGHT;MEASUREDWEIGHT;HINSAMOUNT;HINSCURRENCY;HINSCONTENT;HAZLQ;
126700150703      *  HZDPACKCODE;OPCODE;PCONTENT;ORIGINPARCELNUMBER;POWNERBU;PPARTNERCODE;ASCODE;;
126800150703      **
126900160210     c                   clear                   trova_campo
127000150703     c                   eval      trova_campo = 'PARCELNUMBER'
127100150703     c                   z-add     1             sk
127200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
127300150703     c   21              eval      PRC_PARCELNR = %subst(Dato_sk(sk):1:+
127400150703     c                                              LunDato_sk(sk))
127500150722      * se è arrivato un PARCEL minore di 14 caratteri è come se NON FOSSE ARRIVATO nulla
127600150722     c   21              if        LunDato_sk(sk) < 14
127700150722     c                   clear                   Dato_sk(sk)
127800150722     c                   end
127900150716     c   21              eval      UTIL(sk) ='S'
128000150703      **
128100160210     c                   clear                   trova_campo
128200150703     c                   eval      trova_campo = 'SENDPARCELREF1'
128300150703     c                   z-add     1             sk
128400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
128500150703     c   21              eval      PRC_SNDPRCREF1 = %subst(Dato_sk(sk):1:+
128600150703     c                                              LunDato_sk(sk))
128700150716     c   21              eval      UTIL(sk) ='S'
128800150703      **
128900160210     c                   clear                   trova_campo
129000150703     c                   eval      trova_campo = 'SERVICECODE'
129100150703     c                   z-add     1             sk
129200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
129300150703     c   21              eval      PRC_SERVICECOD = atoi(%TRIM(Dato_sk(sk)))
129400150716     c   21              eval      UTIL(sk) ='S'
129500150703      **
129600160210     c                   clear                   trova_campo
129700150703     c                   eval      trova_campo = 'DECLAREDWEIGHT'
129800150703     c                   z-add     1             sk
129900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
130000150703     c   21              eval      PRC_DECLWEIGHT = atoi(%TRIM(Dato_sk(sk)))
130100150722     c   21              if        PRC_DECLWEIGHT > 3150
130200150722     c                   eval      MSGER(sk) ='Peso Superiore 31,5kg'
130300150722     c                   end
130400150703      **
130500160210     c                   clear                   trova_campo
130600150703     c                   eval      trova_campo = 'MEASUREDWEIGHT'
130700150703     c                   z-add     1             sk
130800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
130900150703     c   21              eval      PRC_MESWEIGHT = atoi(%TRIM(Dato_sk(sk)))
131000150716     c   21              eval      UTIL(sk) ='S'
131100150722     c   21              if        PRC_MESWEIGHT > 3150
131200150722     c                   eval      MSGER(sk) ='Peso Superiore 31,5kg'
131300150722     c                   end
131400150716      *
131500150716     c                   exsr      ctrl_campi
131600150716      *
131700150701      *-------------------------------------------------------------
131800150701      *  Controlla se la spedizione è già arrivata precedentemente
131900150701      *    sulle bolle in partenza o da confermare
132000150701     c                   clear                   GIA_PRESENTE      1
132100150921     c                   clear                   LNA_Sped_RESO     3 0
132200150701      **
132300150701      **  Imposta il Codice Parcel x verifica di precedente immissione
132400150701      **   PARCEL da controllare se già ricevuto e se NON è un Reso
132500150716     c                   eval         Parcel14  = PRC_PARCELNR
132600150701     c                   exsr      check_PARCEL
132700150701      **
132800150701      **  Qui fa scrivere la Spedizione sul VAB
132900150922      **                         =======================
133000150716     c                   if        gia_presente  = 'S'
133100150922      **                         =======================
133200160405      ***------***
133300160405     c                   if        vediPacco = *blanks
133400150701      *   Segnala che è già presente il Parcel quindi NON scrive la spedizione
133500150716     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(Parcel14) +
133600150701     c                                          ' già ricevuto precedentemente!'
133700160405     c                   elseIF    vediPacco = 'S'
133800160405      ***------>>
133900160405     c                   if        ERRvediPacco = *blanks
134000160405      *   Segnala aggiornamento di una spedizione VEDI PACCO
134100160405     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(Parcel14) +
134200160405     c                                          ' era un VEDI PACCO!'
134300160405     c                   elseIF    ERRvediPacco = 'S'
134400160405      *   Se ci sono stati ERRORI su aggiornamento VEDI PACCO
134500160405     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(Parcel14) +
134600160405     c                                     ' ERRORI aggiornando il VEDI PACCO!'
134700160405     c                   end
134800160405      ***------>>
134900160405     c                   end
135000160405      ***------***
135100160405     C                   eval      vinMSG = Msg_Vin_80
135200160405      **
135300150722     c                   elseif    PRC_PARCELNR = *blank or
135400150722     c                               %len(%trim(PRC_PARCELNR)) < 14
135500150722     C                   eval      Msg_Vin_80 = 'Parcel : NON RICEVUTO/-
135600150722     c                             MANCANTE/INCOMPLETO !!'
135700150722     C                   eval      vinMSG = Msg_Vin_80
135800150922      *
135900150722     c                   else
136000150922      *
136100150716      * Dopo il record con il PARCEL è possibile scrivere la spedizione
136200150716     C                   exsr      WRITE_PARCEL
136300150701     c                   end
136400150701      *
136500130904     c                   Endsr
136600130904      *  ------------------------------------------------------------------ */
136700130904      *      MESSAGES
136800130904      *  ------------------------------------------------------------------ */
136900130904     c     sr_MSG        Begsr
137000130904      **
137100130905     c                   clear                   fieu05RMSG
137200130904     c                   movea     H_MSG         Testa_sk
137300150716     c                   movea     SK_MSG_O      OBBL
137400150716     c                   movea     SK_MSG_N      NUME
137500150716     c                   clear                   UTIL
137600150722     c                   clear                   MSGER
137700130904      **
137800150703      *  #DEF;GEODATA:MSG;NUMORDER;NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE;
137900150703      *  MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSGLANG;MSGSENDERURL;;
138000150703      **
138100160210     c                   clear                   trova_campo
138200150703     c                   eval      trova_campo = 'MSGTYPE'
138300150703     c                   z-add     1             sk
138400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
138500150703     c   21              eval      MSG_MSGTYPE = atoi(%TRIM(Dato_sk(sk)))
138600150716     c   21              eval      UTIL(sk) ='S'
138700150703      **
138800160210     c                   clear                   trova_campo
138900150703     c                   eval      trova_campo = 'MSGDESTINATION'
139000150703     c                   z-add     1             sk
139100150703     c     trova_campo   lookup    Testa_sk(sk)                           21
139200150716     c   21              eval      UTIL(sk) ='S'
139300150703     c                   if        *in21
139400130904     c                   eval      MSG_MSGDESTIN = %subst(Dato_sk(sk):1:+
139500130904     c                                              LunDato_sk(sk))
139600150622      **  Qui fa scrivere eventuali  MSG sulla consegna
139700150715     c                   eval        WRT_MAIL_SMS = 'S'
139800150715      *-
139900150619      * carica il campo da riportare poi sul VAT
140000150619     c                   if        MSG_MSGTYPE = 1
140100150715     c                   eval      %subst(MSG_ServizioMail_Predict:1:35) =
140200150619     c                             %subst(MSG_MSGDESTIN:1:35)
140300150715     c                   eval      %subst(MSG_ServizioMail_Predict:36:35)=
140400150713     c                             %subst(MSG_MSGDESTIN:36:35)
140500150715     c                   eval      EMAIL_no_Predict = *blank
140600150715      *-
140700150619     c                   elseif    MSG_MSGTYPE = 3
140800150715     c                   eval      %subst(MSG_ServizioSMS_Predict:1:35) =
140900150619     c                             %subst(MSG_MSGDESTIN:1:35)
141000150619     c                   end
141100150703     c                   endif
141200150703      **
141300160210     c                   clear                   trova_campo
141400150703     c                   eval      trova_campo = 'MSGTRIGGER'
141500150703     c                   z-add     1             sk
141600150703     c     trova_campo   lookup    Testa_sk(sk)                           21
141700150716     c   21              eval      UTIL(sk) ='S'
141800150703     c                   if        *IN21
141900150703     c                   eval      MSG_MSGTRIGGER = atoi(%TRIM(Dato_sk(sk)))
142000150619      *
142100150619     c                   if        MSG_MSGTRIGGER = 904
142200150619     c                   if        MSG_MSGTYPE = 1
142300150715     c                   eval       MSG_MAIL_come_Predict ='S'
142400150619     c                   elseif    MSG_MSGTYPE = 3
142500150715     c                   eval       MSG_SMS_come_Predict ='S'
142600150619     c                   end
142700150703     c                   end
142800150619     c                   endif
142900130904      **
143000150716     c                   exsr      ctrl_campi
143100150701      *
143200130904     c                   Endsr
143300130904      *  ------------------------------------------------------------------ */
143400130904      *       Campi in errore
143500130904      *  ------------------------------------------------------------------ */
143600150702     c     ctrl_campi    Begsr
143700150702      **
143800150702     c                   do        60            sk
143900150702     c                   IF        OBBL(sk)= *blank
144000150702     c                   LEAVE
144100150702     c                   EndIF
144200150702      **
144300150716     c                   IF        UTIL(sk)<>'S'
144400150716     c                   ITER
144500150716     c                   EndIF
144600150722      **
144700150722      * Prima segnalazione
144800150722     c                   IF        MSGER(sk)<>*blank
144900150722     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
145000150722     c                                          + %trim(MSGER(sk))  + ';'
145100150722     c                   EndIF
145200150716      **
145300150702      ** o è OBBLIGATORIO o è Numerico ma NON contiene NUMERI
145400150702     c                   If        LunDato_sk(sk) =  0
145500150702     c                               and OBBL(sk) = Obbligatorio
145600150702     c                             or
145700150702     c                             DatoNum_sk(sk) = 'N'
145800150702     c                               and NUME(sk) = Numerico
145900150702      *  -----> Errore
146000150702     c                   exsr      Field_Error
146100150702     c                   End
146200150717      *
146300150702     c                   EnDDO
146400150717      *
146500150717      *  -----> Segnalazione solamente
146600150717     c                   if        multicollo > 1
146700150717      *
146800150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) +
146900150717     c                             '< Multicollo -->' + %editc(Multicollo:'Z') +
147000150717     c                             ' colli.'
147100150717     c                   clear                   multicollo        3 0
147200150717     C                   eval      se_errore   = 'S'
147300150717     c                   End
147400150702      *
147500150702     c                   Endsr
147600150702      *  ------------------------------------------------------------------ */
147700150702      *       Campi in errore
147800150702      *  ------------------------------------------------------------------ */
147900150702     c     Field_Error   Begsr
148000150716      *
148100130904     C                   eval      se_errore   = 'S'
148200150717      * Errore su VINMSG
148300150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
148400150717     c                                          + %trim(Testa_sk(sk))
148500130904      * msg video o in posta
148600130904     c                   If        Msg_Err = *blank
148700130904     c
148800130904     c                   Select
148900130904     c
149000130904     c                   When      LunDato_sk(sk) =  0
149100130904     c                               and OBBL(sk) = Obbligatorio
149200130904     c                   eval      Tipo_Error = 1
149300150717     C                   eval      Msg_Err =
149400150717     C                             ' >> Obbl.- Manca Dato !!'
149500130904     c
149600130904     c                   When      DatoNum_sk(sk) = 'N'
149700130904     c                               and NUME(sk) = Numerico
149800130904     c                   If        LunDato_sk(sk) =  0
149900130904     c                   eval      Tipo_Error = 2
150000150717     C                   eval      Msg_Err =
150100150717     C                             ' >> Obbl.Manca o NON Numero !!'
150200130904     c                   else
150300130904     c                   eval      Tipo_Error = 3
150400150717     C                   eval      Msg_Err =
150500150717     C                             ' >> NON Numero !!'
150600130904     c                   end
150700130904     c                   When      Tipo_Error = 4
150800150717     C                   eval      Msg_Err =
150900150717     C                             ' >> Conver.non riuscita !!'
151000130904     c                   EndSL
151100150717      *
151200150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) + Msg_err
151300150717     c*
151400130904     c                   End
151500130904      **
151600130904     c                   Endsr
151700130904      *  ------------------------------------------------------------------ */
151800130904      *       Suddivide i campi della riga con carattere divisorio (;)
151900130904      *  ------------------------------------------------------------------ */
152000130904     c     Split_Rec     Begsr
152100130904      **
152200130904     c                   clear                   Nr_campi
152300130904     c                   clear                   Campi_Record
152400130904     c                   clear                   Campi_Lunghi
152500130904     c                   clear                   Campi_Numerici
152600130904     c                   clear                   Campi_Decimali
152700130904     c                   movel     ';'           Separa_campi
152800130904     c                   movel     '.'           Decimal_Separ
152900130904      *
153000160210      * sostituito lo standard previsto con campi max 40 bytes l'uno
153100160210      * con un pgm identico che prevede campi da 256 bytes
153200160210     c******             call      'FIEU00R'
153300160210     c                   call      'FIEU05R0'
153400130904      *       Input
153500130904     c                   parm                    Separa_campi
153600130904     c                   parm                    Decimal_Separ
153700130904     c                   parm                    dati
153800130904      *       Output  su schiere di 100 elementi
153900130904     c                   parm                    Nr_campi
154000130904     c                   parm                    Campi_Record                   * i dati
154100130904     c                   parm                    Campi_Lunghi                   * lunghezza dati i
154200130904     c                   parm                    Campi_Numerici                 * se numerici
154300130904     c                   parm                    Campi_Decimali                 * quanti decimali i
154400130904      **
154500130904     c                   Endsr
154600160229      * ?-----------------------------------------------------------------  */
154700160229      *    Rileva la prima volta che arriva il GEODATA
154800160229      * ?-----------------------------------------------------------------  */
154900160229     C     check_WFIEU00 BEGSR
155000160229     C*
155100160229     c     Parcel14      chain     wfieu00f
155200160229     c                   if        %Found(wfieu00f)
155300160229     c                   if        F00DATGEO = 0
155400160229     c                   eval      F00DATGEO = dateu
155500160229     c                   eval      F00ORAGEO = wora
155600160229     c                   update    fieu000
155700160229     c                   end
155800160229     c                   else
155900160229     c                   clear                   fieu000
156000160229     c                   eval      F00PARCEL = parcel14
156100160229     c                   eval      F00DATGEO = dateu
156200160229     c                   eval      F00ORAGEO = wora
156300160229     c                   write     fieu000
156400160229     c                   end
156500160229     C*
156600160229     C                   ENDSR
156700130904      *  ================================================================== */
156800130904      *     CONTROLLA SE il parcel è arrivato in una precedente trasmissione
156900130904      *  ================================================================== */
157000130904     c     check_PARCEL  Begsr
157100160229      *
157200160229      * registra il 1° arrivo dello SCAN 05
157300160229     c                   exsr      check_WFIEU00
157400150629      **
157500150908      **  se già confermato in Partenza  NON PUO' RICARICARE la spedizione
157600170925      *--
157700151022     c                   clear                   cancellato        1
157800151022     c                   clear                   gia_in_BLP        1
157900160405     c                   clear                   vediPacco         1
158000160405     c                   clear                   errvediPacco      1
158100151023     c                   clear                   sono_gomme        1
158200160211     c                   clear                   salva_CMR        35
158300160218     c                   clear                   salva_SCL         1
158400170925      ******
158500170925      ** Sostituisce il FIAR4 tipo record "I"
158600170925     C     Parcel14      chain     fipnd03l
158700170925     c                   IF        %Found(fipnd03l)
158800170925      **
158900150921     c                   movel     Parcel14      DepotParcel
159000150921      **
159100150921      ** si tratta potenzialmente di un reso
159200150921     c                   if        DepotParcel = '0844'
159300150921     c                              or
159400150921     c                             DepotParcel = '0845'
159500150921      **
159600150921      * cerca la LNA della bolla originale
159700170925     c     kPNDbolla     chain     fnarb01l
159800150921     c                   if        %Found(fnarb01l)
159900150921     c                   z-add     arbLNA        LNA_Sped_RESO
160000150921     c                   end
160100150921      **
160200150921     c                   else
160300150921      **
160400150629      *   controlla che sia DPD con il Network e non con la linea
160500170925     c     PNDLNP        chain     azorg01l
160600150629     c                   if        %Found(azorg01l)
160700150629     c                   eval      OG143 = orgDE3
160800160405      **
160900170925     c                   if         §OGNTW = 'DPD'
161000170922      ** Non serve più agganciare il FISICO del FIAR4 perchè il dato è stato spostato
161100170922      ** sul FIPND00F
161200170922      **
161300170922      ** il flag lo deve impostare dal nuovo file
161400170922     c                   movel     PNDflo        dPNDflo
161500170922     c                   movel     §PNDTRD       wflag             1
161600160404      **
161700160405      ** Solo per records VEDI PACCO  trd=1
161800170925     c     kPNDbolla     chain     fnblp01l
161900160404     c                   if        %Found(fnblp01l) and blpFT1 = *blank
162000170922     c                             and wflag  = '1'
162100170922      **
162200160405     c                   eval        vediPacco = 'S'
162300160405      **
162400160404      ** se NON ancora partita e essendo VEDI PACCO,
162500160404      **  aggiorna i dati in partenza variando la bolla.
162600160404     c                   movel     kpjbu         svkpjbu
162700160404     c                   clear                   kpjbu
162800160404     C                   clear                   FNLSI7DS
162900160404     C                   eval      ILSI7AAS =  blpAAS
163000160404     C                   eval      ILSI7LNP =  blpLNP
163100160404     C                   eval      ILSI7NRS =  blpNRS
163200160404     C                   eval      ILSI7NSP =  blpNSP
163300160404      **
163400160404     C                   eval      ILSI7RSD =  RCV_RNAME1
163500160404     C                   eval      ILSI7RD2 =  RCV_RNAME2
163600160404     C                   eval      ILSI7IND =  RCV_RSTREET
163700160404     C                   eval      ILSI7CAD =  RCV_RZIPCODE
163800160404     C                   eval      ILSI7LOD =  RCV_RTOWN
163900160404     C                   movel     FNLSI7DS      KPJBU
164000160404     C                   call      'FNLSI7R'
164100160404     C                   parm                    KPJBA
164200160405     C                   parm                    FNLSI7DS
164300160404     C                   if        OLSI7ERR <>*blank
164400160405     c                   eval        ERRvediPacco ='S'
164500160404     C                   endif
164600160404     c                   movel     svkpjbu       kpjbu
164700160404      **
164800160404     c                   else
164900160404     c                   end
165000160404      **
165100150908     c                   eval        GIA_PRESENTE = 'S'
165200151022     c                   eval          gia_in_BLP = 'S'
165300160404      **
165400150908     c                   LEAVESR
165500150629     c                   end
165600160405     c                   end
165700150629     c                   end
165800150921     c                   end
165900150629      ** --------
166000130904      **  se non ancora confermato da Cliente
166100150908      ** --------
166200130904     C                   eval        vatTRC = 'E'
166300150716     C                   eval        vatNOT = Parcel14
166400130904     C     Kvat4E        setll     fivat02l
166500130904     C     vatTRC        reade     fivat02l
166600150717     c                   IF        not %EoF(fivat02l)
166700150717     c                   movel     vatNOT        dVATe
166800150908     C                   if        Parcel14= §vatEPRN
166900150716     c     vatLNP        chain     azorg01l
167000150629     c                   if        %Found(azorg01l)
167100150629     c                   eval      OG143 = orgDE3
167200150717      *
167300150717     c                   if        §OGNTW = 'DPD'
167400150908     c                   eval        vabCCM = vatCCM
167500150717     c                   move      Parcel14      vabRMN
167600150717     c     kvab01        chain     fivab01l
167700150717     c                   if        %Found(fivab01l)
167800150717      * Elimina il VAT
167900150717     C     kvat01        setll     fivat01l
168000150717     C     kvat01        reade     fivat01l
168100150717     c                   dow       not %EoF(fivat01l)
168200150717     c                   delete    fivat000
168300150717     C     kvat01        reade     fivat01l
168400150717     c                   enddo
168500151022     c                   move      'S'           cancellato
168600150717     c                   delete    fivab000
168700150717     c                   end
168800150717     c                   end
168900150629      *
169000150629     c                   end
169100150716     c                   end
169200150717     c                   end
169300150908      **
169400150908      **  se non ancora confermato CMR
169500150908     C                   eval        vatTRC = 'E'
169600150908     C                   eval        vatNOT = Parcel14
169700150908     C     Kvat4E        setll     edivat4l
169800150908     C     vatTRC        reade     edivat4l
169900150908     c                   IF        not %EoF(edivat4l)
170000150908     c                   movel     vatNOT        dVATe
170100150908     C                   if        Parcel14= §vatEPRN
170200150908     c     vatLNP        chain     azorg01l
170300150908     c                   if        %Found(azorg01l)
170400150908     c                   eval      OG143 = orgDE3
170500150908      *
170600150908     c                   if        §OGNTW = 'DPD'
170700150908     c                   eval        vabFGS = vatFGS
170800150908     c                   eval        vabCCM = vatCCM
170900150908     c                   move      Parcel14      vabRMN
171000150908     c     kvab03        chain     edivab3l
171100150908     c                   if        %Found(edivab3l)
171200150908      * Elimina il VAT
171300150908     C     kevat01       setll     edivat1l
171400150908     C     kevat01       reade     edivat1l
171500150908     c                   dow       not %EoF(edivat1l)
171600150908     c                   delete    edivat00
171700150908     C     kevat01       reade     edivat1l
171800150908     c                   enddo
171900150908      *
172000160211      * Se arrivava da un IGATEAVIS salva CMR x reimpostarlo sulla nuova bolla
172100160218     c                   if        VABcmr <> GEO_IN_SOSPESO
172200160211     c                   eval        salva_CMR  = VABcmr
172300160218     c                   eval        salva_SCL  = VABscl
172400160211     c                   end
172500160211      *
172600160302      *  deve cancellare anche dalla schiera degli eventuali multicollo
172700160302     c                   z-add     1             dlt               3 0
172800160302     c     vabnsp        lookup    nr_sped(dlt)                           21
172900160302     c   21              eval      nr_sped(dlt) = *all'9'
173000151022     c                   move      'S'           cancellato
173100150908     c                   delete    edivab00
173200150908     c                   end
173300150908     c                   end
173400150908      *
173500150908     c                   end
173600150908     c                   end
173700150908     c                   end
173800150622      **
173900151023      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
174000151023     c                   if        PRC_SERVICECOD = Gomme_365 or
174100151023     c                             PRC_SERVICECOD = Gomme_366 or
174200151023     c                             PRC_SERVICECOD = Gomme_367
174300151023      **   SI--> sono gomme
174400151023     c                   eval      sono_gomme ='S'
174500151023      **
174600151023     c                   end
174700151023      **
174800130904     c                   endSR
174900130904      *  ------------------------------------------------------------------ */
175000130904      *     CONTROLLA il parcel e se OK lo scrive
175100130904      *  ------------------------------------------------------------------ */
175200130904     c     write_PARCEL  Begsr
175300130904      *
175400130904      *  con il record in canna scrive direttamente il VAB ed il VAT
175500130904      *                /*---------------------- */
175600130904     c                   exsr      WRT_VAB_VAT
175700130904      *                /*---------------------- */
175800130904
175900130904      *  Problemi nella decodifica dei campi VAB/VAT
176000130904     c                   if        se_errore ='S'
176100130904     C                   eval      vinMSG = Msg_Vin_80
176200130904     c                   end
176300150716      *
176400130904      **** Meglio evidenziare i SERVICE CODE NON previsti
176500130904     c                   if        PRC_SERVICECOD <> Normal_Parcel and
176600130904     c                             PRC_SERVICECOD <>  Small_Parcel and
176700130904     c                             PRC_SERVICECOD <> Return_Sender and
176800141211     c                             PRC_SERVICECOD <> Return_Parcel and
176900141215     c                             PRC_SERVICECOD <> Gomme_365     and
177000141215     c                             PRC_SERVICECOD <> Gomme_366     and
177100150716     c                             PRC_SERVICECOD <> Gomme_367     and
177200150716     c                             PRC_SERVICECOD <> Normal_Parcel_327 and
177300150716     c                             PRC_SERVICECOD <> Normal_Parcel_329 and
177400150716     c                             PRC_SERVICECOD <> Small_Parcel_328
177500130904     C                   eval      Msg_Vin_80 = '/ Service Code :' +
177600130904     c                              %editc(PRC_SERVICECOD:'X') +
177700130904     c                              ' non previsto!'
177800130904     C                   eval      vinMSG = %trim(vinMSG) +  Msg_Vin_80
177900130904     c                   end
178000130904      **
178100130904     c                   endSR
178200130904      *  ================================================================== */
178300150710     C*  ESEGUO SCRITTURA fiVAT Riferimenti x il destinatario
178400130904      *  ================================================================== */
178500130904     C     WRT_VAB_VAT   BEGSR
178600130904      *
178700150710      * Clearizzo        in quanto ha gli stessi campi di FiVAB più
178800130904     c                   clear                   err_bloccante     1
178900130904     c                   clear                   Sped_RESO         1
179000150922      * Default
179100150925      ** aggancia i dati della tab.LPD  di default
179200151012     c                   z-add     190           Linea_Partenza    3 0
179300150925     c                   exsr      tabella_LPD
179400150925      **
179500150922      * con il Depot del Parcel ricava LINEA Cliente etc.etc.
179600150923     c                   exsr      LNP_DA_DOVE
179700150716      *  Codice bolla
179800150716     C                   MOVEL     '1'           vabcbo                         NO C/assegno
179900150716      *
180000150716      * -----------------                  -------------------
180100150716      *  Mittente Originale
180200150917     C                   CLEAR                   TISI95DS
180300150716     C                   eval       VABRMO = SND_SNAME1
180400150716     C                   eval       VABCMO = SND_SZIPCODE
180500150716     c                   z-add     1             sk
180600150716     c     SND_SCOUNTRYCDlookup    CIE(sk)                                21
180700150716     C   21              eval       VABNMO = c15(sk)
180800150722     c                   if        vabnmo = *blank or
180900150722     c                             vabcmo = *blank
181000150722     c                   clear                   vabcmo
181100150722     c                   clear                   vabnmo
181200150916     c                   else
181300150916      ** nazione NON Italia
181400150916     C                   MOVEL     vabNMO        I95NAR
181500150916     C                   MOVEL     vabCMO        I95CAP
181600150916     C                   MOVEL     DATEU         I95DAT
181700150916     C                   MOVEL     '3'           I95TCN
181800150916     C                   CALL      'TISI95R'
181900150916     C                   PARM                    TISI95DS
182000150917     c                   end
182100150917      *
182200150916      *     Errore
182300150916     C     O95ERR        IFne      *BLANKS
182400150917     c     vabrmo        orEQ      *BLANKS
182500150916     c                   clear                   vabrmo
182600150916     c                   clear                   vabcmo
182700150916     c                   clear                   vabnmo
182800150916     C                   END
182900150916      *
183000150716      * -----------------                  -------------------
183100150716      *  Data  spedizione
183200160208     C                   MOVE      DATEU         VABMGS                         = oggi
183300150716      *
183400150827      * -----------------                  -------------------
183500150716      *  Dati  destinatario
183600150716     c                   movel     RCV_RNAME1    vabrsd                         Ragione Sociale
183700150716     C                   movel     RCV_RNAME2    vabrd2                         rag.soc.2
183800150716     c                   movel     RCV_RSTREET   vabind                         Indirizzo
183900150716     c                   movel     RCV_RTOWN     vablod                         Località
184000160216      **
184100160216      *  lasciamo nel CAP solo dei numeri
184200160216     c                   do        9             §n                3 0
184300160216      **
184400160216     c                   if        %check(digits:%subst(RCV_RZIPCODE:§n:1))<>0
184500160216     c                   eval      %subst(RCV_RZIPCODE:§n:1) = ' '
184600160216     c                   end
184700160216      **
184800160216     c                   end
184900160216      **
185000150828     c                   movel     RCV_RZIPCODE  vabcad                         C.A.P.
185100150925      **-----------------                  -------------------
185200150716      *  Se manca la provincia la calcolo
185300150716     c                   if        vabprd = *blank
185400150716     C                   CLEAR                   TISI95DS
185500150716     C                   MOVEL     vabCAD        I95CAP
185600150716     C                   MOVEL     DATEU         I95DAT
185700150716     C                   MOVEL     '3'           I95TCN
185800150716     C                   CALL      'TISI95R'
185900150716     C                   PARM                    TISI95DS
186000150716      *     Errore
186100150716     C     O95ERR        IFeq      *BLANKS
186200150716     C                   MOVEL     O95PRV        vabPRD
186300150716     C                   END
186400150716      *
186500150716     c                   end
186600150716      *  Tipo servizio
186700150716     c                   movel     'C'           vabtsp
186800150716      ** RMA
186900150722     C**********         eval       vabrma = %Trim(SHP_MPSCREF1)
187000130904      ** ------------------------------------------------------------------ */
187100130904      *     ( Il parcel non è + numerico )
187200130904     c
187300130904      *  il Parcel deve essere impostato nella DS VATE
187400150716      *  se comunque il Parcel ricevuto è numerico impostiamo nel RmittNUMERICO
187500150716      *  il nr.Parcel come avveniva prima che diventasse alfa il campo
187600130904      **
187700130904     c     digits        check     PRC_PARCELNR  position
187800130904      **
187900130904      **  presenti campi non numerici: (Ossia trovati)
188000150716      **   ma sono oltre il codice PARCEL sulla destra ... allora è OK
188100150716     C                   z-add     1             vabrmn
188200150716      **    in tal caso sono solo i blanks alla destra del numero
188300150716     c                   iF        %Found and position > 14
188400150716     c                             or not %Found
188500130904     c                   move      parcel14      vabrmn
188600130904     c                   end
188700160121      *
188800160121      **-----------------                  -------------------
188900160121      * ?Scrivo EDiVAB
189000160121     C                   If        su_EDIVAB = 'S'
189100160121      *
189200160121      *   Controlla se ricevuto già lo SCAN 05 di collo partito
189300160121      *    con il PARCEL di 14
189400160121     c                   clear                   SCAN05_OK         1
189500160121     c                   exsr      CHECK_SCAN05
189600160121      *
189700160121     c                   end
189800160121      *
189900150716      **-----------------                  -------------------
190000150717      *  Preleva il Nr.Spedizione
190100150717     C                   EXSR      SUB_keybol
190200150717      *
190300150716      *  Colli  - può esserci un multicollo   QUINDI impostiamo a (1)
190400150701     c                   z-add     1             vabncl
190500150716      *
190600150717      **-----------------                  -------------------
190700150716      * Il volume viene impostato solo se la spedizione è MONOCOLLO.
190800150716      * poiké è passato il valore intero ma è in cm cubi
190900150716      *    ossia il valore di 3000000= 3mc. divido x 1000000
191000150716     c                   if        SHP_MPSVOLUME <> 0  and
191100150716     c                             SHP_MPSCOUNT  = 1
191200160927     c**** SHP_MPSVOLUME div       1000000       vabVLB
191300150716     c                   end
191400150716      **-----------------                  -------------------
191500150716      *   Prioritariamente imposto il Peso
191600150716      *     misurato sul PARCEL da DPD
191700150716     c                   if        PRC_MESWEIGHT <> 0
191800150716     c     PRC_MESWEIGHT div       100           vabPKB
191900150722     c                   if        vabpkb =0
192000150722     c                   eval      vabpkb =0,1
192100150722     c                   end
192200150717     c                   sub       vabPKB        Kg_rimasti
192300150716      *
192400150716      * se non c'è
192500150716      *    metto quello dichiarato dal cliente
192600150716     c                   elseIF    PRC_DECLWEIGHT<> 0
192700150716     c     PRC_DECLWEIGHTdiv       100           vabPKB
192800150722     c                   if        vabpkb =0
192900150722     c                   eval      vabpkb =0,1
193000150722     c                   end
193100150717     c                   sub       vabPKB        Kg_rimasti
193200150716      *
193300150716      * se non c'è imposto quello della spedizione
193400150716      *                 il valore intero ma ha 2 decimali
193500150716      *           ossia il valore di 300 = 3kg. divido x 100
193600150717     c                   elseIF     SHP_MPSWEIGHT <> 0 and SHP_MPSCOUNT =1
193700150716     c     SHP_MPSWEIGHT div       100           vabPKB
193800150722     c                   if        vabpkb =0
193900150722     c                   eval      vabpkb =0,1
194000150722     c                   end
194100150916     c                   endIF
194200150716      *
194300150716      *  e se non c'è nemmeno questo vado alla vecchia con il SERVICE CODE
194400150916     c                   if        vabPKB  = 0 or vabPKB > 31,5
194500150717     c                   z-add     1             kg0_SPED(nr)
194600150717      *
194700130904      *  Service  Code:  101-Normal Parcel
194800130904      *                  136-Small Parcel
194900130904      *                  298-Return Sender
195000130904      *                  300-Return Parcel
195100130904      *                  327-Normal Parcel B2C
195200130904      *                  329-Normal Parcel B2C
195300130904      *                  328-Small  Parcel B2C
195400141211      *                  365-Gomme BtoB
195500141211      *                  366-Gomme BtoC
195600141211      *                  367-Gomme BtoC con COD
195700130904     c                   clear                   w015A
195800130904     c                   if        PRC_SERVICECOD = Normal_Parcel
195900130904     c                               or
196000130904     c                             PRC_SERVICECOD = Normal_Parcel_327
196100130904     c                               or
196200130904     c                             PRC_SERVICECOD = Normal_Parcel_329
196300130904     c                   eval      W015A = 'PESO01'
196400150716     c                   elseIF    PRC_SERVICECOD = Small_Parcel
196500130904     c                               or
196600130904     c                             PRC_SERVICECOD = Small_Parcel_328
196700130904     c                   eval      W015A = 'PESO02'
196800150716     c                   elseIF    PRC_SERVICECOD = Gomme_365
196900141211     c                               or
197000141215     c                             PRC_SERVICECOD = Gomme_366
197100141211     c                               or
197200141215     c                             PRC_SERVICECOD = Gomme_367
197300141211     c                   eval      W015A = 'PESO03'
197400141211     c                   end
197500141211      *
197600130904      *  Peso, ricavato dal tipo servizio del ROUTELABEL
197700130904     c                   z-add     1             xx
197800130904     c                   if        w015a <> *blank
197900130904     c     W015A         lookup    DPPK(xx)                               31
198000130904     c                   If        *in31 = *off
198100130904     c                   eval      W015A = 'PESO'
198200130904     c     W015A         lookup    DPPK(xx)                               31
198300130904     c                   Endif
198400150916     c                   If        *in31
198500130904     c                   z-add     DPP(xx)       vabPKB
198600161006     c                   z-add     VLB(xx)       vabVLB
198700150916     c                   End
198800130904     c                   End
198900141104      *
199000150716     c                   EndIF
199100150916      *
199200150916      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
199300150916     c                   if        vabPKB > 31,5
199400150916     c                   eval           vabPKB = 30
199500150916     c                   EndIF
199600150722      *
199700130904      *  Se spedizione RESA imposto diversamente il VAB
199800130904      *     ma se DPD non invia correttamente il service Code
199900130904      *     identifichiamo il reso dal Depot Bartolini sul Parcel
200000130904      **
200100150716     c                   movel     PRC_PARCELNR  DepotParcel
200200150722      **
200300130904     c                   if        PRC_SERVICECOD  = Return_Sender
200400130904     c                               or
200500130904     c                             PRC_SERVICECOD  = Return_Parcel
200600130904     c                               or
200700150716     c                             (DepotParcel= '0844' or DepotParcel= '0845')
200800130904     C                   clear                   VABpkb
200900130904     C                   clear                   VABncl
201000150925      *
201100150925     c                   if        LNA_Sped_RESO =  0
201200150925     c                   eval      LNA_Sped_RESO =  190
201300150925     c                   end
201400151012      *
201500150923     c                   if        LNA_Sped_RESO <> 0
201600151012      *
201700150925     c                   z-add     LNA_Sped_RESO Linea_Partenza
201800151012     c                   eval      Sped_RESO = 'S'
201900151012      *
202000151012      ** aggancia i dati della tab.LPD x il RESO
202100150925     c                   exsr      tabella_LPD
202200151012      *
202300150923     c                   end
202400150925      *
202500150721     c                   movel     vabrsd        campo35          35
202600150721     C                   eval      VABrsd = '**RESO**' + campo35
202700150925      ***
202800150925     C                   MOVEL     '2'           vabcbo
202900130904     c                   ENDIF
203000150925      **
203100150925      * -----------------                  -------------------
203200150925      ** imposta il Depot di provenienza x testare se VTG
203300150925     c                   movel     parcel14      DepotParcel
203400150925     C                   movel(p)  'VTG'         cldCOD
203500150925     C                   move      DepotParcel   cldDEP
203600150925     c     key_VTG       setll     tncld00F
203700150925     c     key_VTG       reade     tncld00F
203800150925     c                   dow       not %EoF(tncld00F)
203900150925     c                   if        dateu >= cldDDA and
204000150925     c                             dateu <= cldADA
204100150925      **  Se è un collo VTG deve avere una tariffa differente
204200151012     c                   movel     tariffa_VTG   vabCTR
204300150925     c                   leave
204400150925     c                   end
204500150925     c     key_VTG       reade     tncld00F
204600150925     c                   endDO
204700150925      * -----------------                  -------------------
204800130904      *  FAX destinatario da contattare
204900130904     c                   if        RCV_RFAX   <> *blank
205000130904     c                   ENDIF
205100130904      *
205200130904      *  Controlla esistenza del Parcel sul file degli ORM dati a DPD per
205300130904      *   impostare la bolla in assegnato
205400130904      *  Controlla sul FIDPO x metterlo in assegnato
205500150716     C     parcel14      chain     fidpo06l
205600150716      *assegnato
205700130904     c                   if        %Found(fidpo06l)
205800130904     C                   MOVEL     '2'           vabcbo
205900130904      *
206000130904      *  SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
206100130904      *  in FERMO DEPOSITO.
206200130904     c                   eval      vabFFD = *blank
206300150716     C                   if          dpoPOE > 0
206400130904     C                   z-add     dpoPOE        ORMPOE
206500130904     C                   z-add     dpoNRS        ORMNSR
206600130904     C                   z-add     dpoNOR        ORMNOR
206700130904     C                   z-add     dpoNRV        ORMNRV
206800130904      *  lo vedo sull'ORM
206900130904     C     KORM          chain     fnORM01l
207000130904     c                   if        %Found(FnOrm01l) and ormffd ='S'
207100130904     c                   eval      vabFFD = 'S'
207200130904     c                   end
207300130904     c                   end
207400150716     c                   end
207500150831      ***
207600160217      *** NON SI VUOLE CREARE IL RESO (non modifico le vecchie spec.NON si sa mai)
207700160217      ***   SALTIAMO semplicemente la scrittura
207800160217      ***
207900160217     c                   If        Sped_Reso <>'S'
208000160217      ***                          ==========
208100160217      ***
208200150908      ***
208300150916      *   non facciamo scrivere la spedizione se:
208400150916      *         1) destinatario ed indirizzo = ' '
208500150916      *         2) località e cap = ' '
208600150916      ***
208700150916     c                   if        vabrsd =*blank and vabind = *blank
208800150916     c                                or
208900150916     c                             vablod =*blank and vabcad = *blank
209000151022      *  segnala mancanza dati fondamentali
209100151022     c                   eval        dati_CONSEGNA ='N'
209200150922      *  altrimenti scrive la spedizione
209300150916     c                   else
209400161215      *
209500161215      *** Provenienza  x distinguere i records dall'IGATEAVIS
209600161215     c                   movel     'G'           VABscl
209700150921      *
209800150921      * ?Scrivo EDiVAB o FiVAB
209900150921     C                   If        su_EDIVAB = 'S'
210000150923      *
210100150923      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
210200150923    2c                   if        SHP_CDATE<>0
210300151103     c                   TEST(D)                 shp_CDATE              99
210400151103     c   99*iso          movel     DATEU         DATA_iso
210500151103     c  n99*iso          movel     SHP_CDATE     DATA_iso
210600150923     c                   else
210700150923     c     *iso          movel     DATEU         DATA_iso
210800150923     c                   end
210900150929      *
211000150929      *  se Non c'è incremento
211100150929     c                   if        gg_incr = 0
211200150929     c                   exsr      test_gg_lav
211300150929      *
211400150929     c                   else
211500150928      *
211600150928      *  aggiunge x giorni (ma solo lavorativi)
211700150928     c                   do        gg_incr       giorni            3 0
211800150928     c                   adddur    1:*D          data_iso
211900150929     c                   exsr      test_gg_lav
212000150928     c                   enddo
212100150929     c                   endif
212200150928      *
212300150923     c                   move      data_iso      data_iso8         8 0
212400150928      *   Se la data risultante è inferiore alla UDATE forza UDATE
212500150928     c                   if        data_iso8 < dateu
212600160121     c                   move      dateu         data_iso8
212700150928     c                   end
212800150923      *
212900150923     c                   eval      vabcmr = 'GEO_' + %editc(data_iso8:'Y')
213000151023     c                   If        sono_gomme ='S'
213100151023     c                   eval      vabcmr = %trim(vabcmr) + '_GOMME'
213200151023     c                   end
213300150921      *
213400150921      * se trattasi di Reso (Evidenzio)
213500150923     c                   If        Sped_Reso = 'S'
213600150923     c                   clear                   campo35          35
213700150923     c                   eval      campo35 = 'RESI_' + vabCMR
213800150923     c                   eval      VABcmr  = campo35
213900150921     c                   end
214000150921      *
214100160121      *  Se non era arrivato già lo SCAN 05 come COLLO PARTITO fisicamente
214200160121      *   allora va mantenuto in sospensione in attesa dello SCAN 05.
214300160121     c                   if              SCAN05_OK <>'S'
214400160121     c                   eval      VABcmr  = GEO_in_sospeso
214500160121     c                   end
214600160211      *
214700160211      *
214800160211      * Significa che prima era arrivato sull'IGATEAVIS la spedizione
214900160211     c                   if        salva_CMR <> *blank
215000160211      * reimposta il CMR dell'IGATEAVIS
215100160211     c                   eval      vabCMR = salva_CMR
215200160218     c                   eval      vabSCL = salva_SCL
215300160211     c                   end
215400160121      *
215500150921     c                   z-add     DATEU         VABdcm
215600150921     c                   z-add     DATEU         VABdts
215700150921     c                   movel     WORA          VABhms
215800150921      ***
215900150921     c                   write     EDivab00
216000151103      *** al momento sospeso --> vedremo se ci servirà....
216100151103     c******             exsr      wri_wfieu05
216200150921     c                   ELSE
216300130904     c                   write     FiVAB000
216400150921      ***
216500150921     c                   ENDIF
216600150921      *
216700130904     c                   exsr      wrtvat
216800150916      *
216900150916     c                   end
217000130904      ***
217100160217     c                   endIF
217200160217      ***
217300160217      ***  saltato se SPED. di RESO  (stessa cosa per il MSG)
217400160217      ***
217500130904     c                   move      'S'           Almeno_Uno
217600130904      *
217700130904     c     Error_VAB     Endsr
217800130904     C*----------------------------------------------------------------
217900160121      *  Controlla se arrivato SCAN 05 su TIDP200F
218000160121     C*----------------------------------------------------------------
218100160121     C     CHECK_SCAN05  BEGSR
218200160121      *
218300160121      * Se lo ha trovato deve prendere il SERVICE CODE il PESO e la DATA
218400160121     c     parcel14      chain     TIDP200F
218500160121     c                   If        %Found(TIDP200F)
218600160121     c                   eval         SCAN05_OK ='S'
218700160121     c                   move      I2WEIGHT      PRC_MESWEIGHT
218800160121     c                   move      I2SERVICE     PRC_SERVICECOD
218900160121      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
219000160121     c                   movel     I2DATTIM      I2DATAw           8 0
219100160121    2c                   if        I2DATAw >0
219200160121     c     *iso          movel     DATEU         DATA_iso
219300160121     c                   TEST(D)                 I2DATAw                99
219400160121     c  n99*iso          movel     I2DATAw       DATA_iso
219500160121     c                   movel     DATA_iso      SHP_CDATE
219600160121     c                   end
219700160121      *
219800160121     c                   delete    tidp2000
219900160121     c                   End
220000160121      *
220100160121     c                   Endsr
220200160121     C*----------------------------------------------------------------
220300160121     C*  test su gg lavorativi altrimenti incrementa al primo lavorativo
220400130904     C*----------------------------------------------------------------
220500150929     C     test_gg_lav   BEGSR
220600130904      *
220700150929      * -----------
220800150929      * Cerca definitivamente il D-day lavorativo
220900150929      *  esclude sia il sabato che la domenica
221000150929      * -----------
221100150929     c                   do        *hival
221200150929     c                   move      data_iso      dadata
221300150929     c                   move      data_iso      adata
221400150929     c                   CALL      'XSRLAV8'
221500150929     c                   PARM                    Wdata8
221600150929      * se corrisponde ho trovato il D-Day
221700150929     c                   if        giolav <> 0
221800150929     c                   leave
221900150929     c                   else
222000150929      * altrimenti continuo a cercare il primo giorno lavorativo valido.
222100150929     c     data_iso      adddur    1:*d          data_iso
222200150929     c                   iter
222300150929     c                   endif
222400150929     c                   enddo
222500150929      *
222600150929     c                   Endsr
222700150929     C*----------------------------------------------------------------
222800150929     C*  in base al Depot di provenienza determina Linea e Cliente
222900150929     C*----------------------------------------------------------------
223000150929     C     LNP_da_dove   BEGSR
223100150929      *
223200150923      ** imposta il default
223300150923     c                   z-add     1             gg_incr           3 0
223400151015     c                   movel     SHP_SDEPOT    Start_depot       4
223500151015     c     ritenta       tag
223600150923      *
223700150922      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
223800150925      *   e se non fosse presente con i primi 4 caratteri del Parcel
223900151015     c                   if        Start_depot <> *blank
224000151015     c                   movel     Start_depot   DepotParcel
224100151015     c                   z-add     1             giro_nr           3 0
224200150925     c                   Else
224300151015     c                   z-add     2             giro_nr
224400150925     c                   movel     PRC_PARCELNR  DepotParcel
224500150925     c                   End
224600150922      *
224700150922     C     key_Depot     chain     DPcdp01l
224800150922     c                   if        %Found(DPcdp01l)
224900150922      *
225000150922     C     key_Dep       setgt     DPpdc01l
225100150922     C     DepotParcel   readpe    DPpdc01l
225200150922     c                   dow       not %Eof(DPpdc01l)
225300150922      *   se trovato il Depot VALIDO
225400150924     c                   if        pdcDDE <= dateU and dateU <= pdcDSC
225500150923     c                   eval      DpdcFLO = pdcFLO
225600150922      *
225700150923     c                   if        §PDCFLOLNP <> *blank  and
225800150923     c                             §PDCFLOLNP <> '000'
225900150923     c                   if        %check(digits:§PDCFLOLNP)=0
226000150923     c                   move      §PDCFLOLNP    linea_partenza    3 0
226100150925      **
226200150925      ** tab.definizione dati x Linea DPD
226300150925     c                   exsr      tabella_LPD
226400150925      **
226500150922     c                   end
226600150923     c                   end
226700150922      *
226800150923     c                   if        §PDCFLOGGI <> *blank and
226900150923     c                             §PDCFLOGGI <> '000'
227000150923     c                   if        %check(digits:§PDCFLOGGI)=0
227100150923     c                   move      §PDCFLOGGI    gg_incr           3 0
227200150923      *
227300150922     c                   end
227400150923     c                   end
227500150922      *
227600150922     c                   end
227700150922      *
227800150922     C     DepotParcel   readpe    DPpdc01l
227900150922     c                   Enddo
228000150922     c                   End
228100150922      *
228200151015     c                   if        §PDCFLOLNP = '000' or
228300151015     c                             §PDCFLOLNP = *blank
228400151015     c                   if          giro_nr = 1
228500151015     c                   clear                   Start_depot
228600151015     c                   goto      ritenta
228700151015     c                   End
228800151015     c                   End
228900151015      *
229000150922     c                   Endsr
229100150922     C*----------------------------------------------------------------
229200150925     C*  Aggancia tab.LPD ed imposta i campi del VAB
229300150922     C*----------------------------------------------------------------
229400150925     C     tabella_LPD   BEGSR
229500150922      *
229600151016     c                   clear                   FIEULPDds
229700151016     c                   clear                   dLPD
229800151016     c                   movel(p)  linea_partenzaiLPDlnp
229900151016     c                   movel(p)  PRC_PARCELNR  iLPDparcel
230000151016      *
230100151016     c                   call      'FIEULPDR'
230200151016     c                   parm                    FIEULPDds
230300151016      *
230400150925      * ha trovato i dati necessari per instradare il record della spedizione
230500150925      * alla filiale di partenza
230600151016     c                   if        oLPDtip <> 'E'
230700151016     c                   eval         dLPD = oLPDUNI
230800150925     C                   eval      su_EDIVAB = §LPDWRK
230900150925     C                   eval      vabCCM    = §LPDKSC
231000150925     C                   eval      vabCTM    = §LPDCTM
231100151016     C                   eval      vabCTR    = OLPDTAR
231200151012     c                   movel     §LPDTARvtg    tariffa_VTG
231300151012      ** x i Resi
231400151012     c                   if        Sped_RESO = 'S'
231500151012     C                   eval      vabCCM    = §LPDKSCre
231600151016     C                   eval      vabCTR    = OLPDTRE
231700151012     c                   movel     §LPDTREvtg    tariffa_VTG
231800151012     c                   end
231900150925     C                   movel     linea_partenzavabLNP
232000150925     C                   movel     linea_partenzavabFGS
232100150925     c                   end
232200150925      *
232300151012      *  QUI la tariffa LHS non deve essere considerata poichè posso conoscere
232400151012      *  la linea di PARTENZA ma la linea di ARRIVO non la conosco.
232500151012      *
232600150925     c                   Endsr
232700150925     C*----------------------------------------------------------------
232800150925     C*  ESEGUO SCRITTURA FiVAT Riferimenti x il destinatario
232900150925     C*----------------------------------------------------------------
233000150925     C     WRTVAT        BEGSR
233100150925      *
233200130904      *   Parcel Nr. --> Riferimento Parcel DPD
233300130904     c                   clear                   fivat000
233400130904      **
233500130904     C                   movel     VABfgs        VATfgs
233600130904     C                   Z-ADD     VABCCM        VATCCM
233700130904     C                   Z-ADD     VABAAS        VATAAS
233800130904     C                   Z-ADD     VABLNP        VATLNP
233900130904     C                   Z-ADD     VABNRS        VATNRS
234000130904     C                   Z-ADD     VABNSP        VATNSP
234100130904      **
234200130905      ** ------------------------------------------------------------------
234300130905      *   E' già predisposto per il Multicollo: DIPENDE come arrivano i dati.
234400130905      ** ------------------------------------------------------------------
234500130904     C                   MOVEL     'E'           VATTRC
234600130904      **
234700130904      **  su nuovo tipo record (E)
234800130904     c                   clear                   DVate
234900130904      *
235000130904      *  IMPOSTA la DS VAT(E)
235100130904     C                   MOVEL     PRC_PARCELNR  §VATEPRN
235200130904     C                   move      *zeros        §VATESCD
235300130904     C                   move      PRC_SERVICECOD§VATESCD
235400130904     C                   move(p)   '%'           §VATEBID
235500130904     C                   MOVEL     *blank        §VATEROU
235600130904     C                   MOVEL     DVate         VATNOT
235700130904      **
235800150922     C                   If        su_EDIVAB = 'S'
235900150922     C                   MOVEL     VABCMR        VATCMR
236000150922     C                   Z-ADD     VABCNT        VATCNT
236100150922     C                   WRITE     EDiVAT00
236200150922     c                   else
236300130904     C                   WRITE     fiVAT000
236400150922     c                   end
236500150710      **
236600130905      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
236700130904      *   Contatto del Destinatario
236800150716      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
236900130904     C                   if        RCV_RCONTACT <> *Blanks
237000130904     c                   clear                   fivat000
237100130904      **
237200130904     C                   movel     VABfgs        VATfgs
237300130904     C                   Z-ADD     VABCCM        VATCCM
237400130904     C                   Z-ADD     VABAAS        VATAAS
237500130904     C                   Z-ADD     VABLNP        VATLNP
237600130904     C                   Z-ADD     VABNRS        VATNRS
237700130904     C                   Z-ADD     VABNSP        VATNSP
237800130904      **
237900130904     C                   MOVEL     'A'           VATTRC
238000130904     C                   MOVEL     RCV_RCONTACT  VATNOT
238100130904      **
238200150922     C                   If        su_EDIVAB = 'S'
238300150922     C                   MOVEL     VABCMR        VATCMR
238400150922     C                   Z-ADD     VABCNT        VATCNT
238500150922     C                   WRITE     EDiVAT00
238600150922     c                   else
238700130904     C                   WRITE     fiVAT000
238800150922     c                   end
238900150922      *
239000130904     c                   endIF
239100130904      *
239200150716      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
239300130904      *   Contatto Telefonico
239400150716      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
239500130904     C                   if        RCV_RCONTPHO1 <> *Blanks
239600130904     c                   clear                   fivat000
239700130904      **
239800130904     C                   movel     VABfgs        VATfgs
239900130904     C                   Z-ADD     VABCCM        VATCCM
240000130904     C                   Z-ADD     VABAAS        VATAAS
240100130904     C                   Z-ADD     VABLNP        VATLNP
240200130904     C                   Z-ADD     VABNRS        VATNRS
240300130904     C                   Z-ADD     VABNSP        VATNSP
240400130904      **
240500130904     C                   MOVEL     'B'           VATTRC
240600130904     C                   MOVEL     RCV_RCONTPHO1 VATNOT
240700130904      **
240800150922     C                   If        su_EDIVAB = 'S'
240900150922     C                   MOVEL     VABCMR        VATCMR
241000150922     C                   Z-ADD     VABCNT        VATCNT
241100150922     C                   WRITE     EDiVAT00
241200150922     c                   else
241300130904     C                   WRITE     fiVAT000
241400150922     c                   end
241500150922      *
241600130904     c                   endIF
241700150619      *
241800130905      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
241900130905      *
242000130904     C                   ENDSR
242100150715      *  ------------------------------------------------------------------ */
242200150715      *    Cosa scrivere  x records MSG
242300150715      *  ------------------------------------------------------------------ */
242400150715     c     write_MSG     Begsr
242500151109      *
242600151109      *    S O S P E S O
242700151109      *               AL MOMENTO la scrittura dell'ALERT.
242800151109      *              ======================================
242900150715      *
243000150715      *   Servizio PREDICT Mail
243100150715     c                   if         MSG_MAIL_come_Predict ='S'
243200150715     c                               or
243300150715     c                              EMAIL_no_Predict      ='S'
243400150715      *
243500150715     c                   clear                   fivat000
243600150715      **
243700150715     C                   movel     VABfgs        VATfgs
243800150715     C                   Z-ADD     VABCCM        VATCCM
243900150715     C                   Z-ADD     VABAAS        VATAAS
244000150715     C                   Z-ADD     VABLNP        VATLNP
244100150715     C                   Z-ADD     VABNRS        VATNRS
244200150715     C                   MOVEL     'I'           VATTRC
244300150715      **
244400150715     c                   if         MSG_MAIL_come_Predict ='S'
244500150715     C                   eval      VATNOT =
244600150715     c                             %subst(MSG_ServizioMail_Predict:1:35)
244700150715     c                   elseif     EMAIL_no_Predict      ='S'
244800150715     C                   eval      VATNOT =
244900150715     c                             %subst(RCV_Remail:1:35)
245000150715     c                   end
245100150715      **
245200150715     c                   do        nr            wrt               5 0
245300150715     c                   if        nr_sped(wrt) = 0
245400150715     c                   leave
245500150715     c                   end
245600160302     c                   if        nr_sped(wrt) = *all'9'
245700160302     c                   iter
245800160302     c                   end
245900150715     c                   z-add     nr_sped(wrt)  vatNSP
246000150715      *
246100150922     C                   If        su_EDIVAB = 'S'
246200150922     C                   MOVEL     VABCMR        VATCMR
246300150922     C                   Z-ADD     VABCNT        VATCNT
246400150922     C                   WRITE     EDiVAT00
246500150922     c                   else
246600150715     C                   WRITE     fiVAT000
246700150922     c                   end
246800150922      *
246900150715     c                   enddo
247000150715      *-----
247100150715     c                   if        %subst(MSG_ServizioMail_Predict:36:35)
247200150715     c                                 <> *blank
247300150715     c                               or
247400150715     c                             %subst(RCV_Remail:36:35)
247500150715     c                                 <> *blank
247600150715     C                   MOVEL     'J'           VATTRC
247700150715      *-
247800150715     c                   if         MSG_MAIL_come_Predict ='S'
247900150715     C                   eval       VATNOT =
248000150715     c                             %subst(MSG_ServizioMail_Predict:36:35)
248100150715     c                   elseif     EMAIL_no_Predict      ='S'
248200150715     C                   eval      VATNOT =
248300150715     c                             %subst(RCV_Remail:36:35)
248400150715     c                   end
248500150715      **
248600150715     c                   do        nr            wrt               5 0
248700150715     c                   if        nr_sped(wrt) = 0
248800150715     c                   leave
248900150715     c                   end
249000160302     c                   if        nr_sped(wrt) = *all'9'
249100160302     c                   iter
249200160302     c                   end
249300150715     c                   z-add     nr_sped(wrt)  vatNSP
249400150715      *
249500150922     C                   If        su_EDIVAB = 'S'
249600150922     C                   MOVEL     VABCMR        VATCMR
249700150922     C                   Z-ADD     VABCNT        VATCNT
249800150922     C                   WRITE     EDiVAT00
249900150922     c                   else
250000150715     C                   WRITE     fiVAT000
250100150922     c                   end
250200150922      *
250300150715     c                   enddo
250400150715      *
250500150715     c                   endIF
250600150715      **----
250700150715     c                   endIF
250800150715      *
250900150715      *   Servizio PREDICT SMS
251000150715     c                   if         MSG_SMS_come_Predict ='S'  and
251100150715     c                             %subst(MSG_ServizioSMS_Predict:1:16)
251200150715     c                              <> *blank
251300150715     c                                 or
251400150715     c                             %subst(MSG_ServizioMail_Predict:1:35)
251500150715     c                              <> *blank
251600150715     c                                 or
251700150715     c                             EMAIL_no_Predict ='S'
251800150715      **
251900150715     c                   clear                   fivat000
252000150715     C                   movel     VABfgs        VATfgs
252100150715     C                   Z-ADD     VABCCM        VATCCM
252200150715     C                   Z-ADD     VABAAS        VATAAS
252300150715     C                   Z-ADD     VABLNP        VATLNP
252400150715     C                   Z-ADD     VABNRS        VATNRS
252500150715     C                   MOVEL     'S'           VATTRC
252600150715      **
252700150715     c                   clear                   dvatS
252800150715      **
252900150715      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
253000150715      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
253100150715     c                   if        MSG_SMS_come_Predict ='S' and
253200150715     c                             %subst(MSG_ServizioSMS_Predict:1:16)
253300150715     c                              <> *blank
253400150715     C                   eval       §VATSTEL =
253500150715     c                             %subst(MSG_ServizioSMS_Predict:1:16)
253600150715     C                   eval       §VATSSMP ='S'
253700150715     c                   end
253800150715      **
253900150715      ** se c'era anche la MAIL
254000150715     c                   IF        %subst(MSG_ServizioMail_Predict:1:35)
254100150715     c                              <> *blank
254200150715     C                   eval       §VATSMMP ='S'
254300150715     c                   else
254400150715      * se non c'è il PREDICT e ho la mail la si deve adoperare per comunicazioni LA o FFD
254500150715     c                   if        EMAIL_no_Predict ='S'
254600150715     C                   eval       §VATSMMP ='N'
254700150715     c                   end
254800150715     c                   end
254900150715      **
255000150715     c                   eval       VATNOT = dvatS
255100150715      **
255200150715     c                   do        nr            wrt               5 0
255300150715     c                   if        nr_sped(wrt) = 0
255400150715     c                   leave
255500150715     c                   end
255600160302     c                   if        nr_sped(wrt) = *all'9'
255700160302     c                   iter
255800160302     c                   end
255900150715     c                   z-add     nr_sped(wrt)  vatNSP
256000150715      *
256100150922     C                   If        su_EDIVAB = 'S'
256200150922     C                   MOVEL     VABCMR        VATCMR
256300150922     C                   Z-ADD     VABCNT        VATCNT
256400150922     C                   WRITE     EDiVAT00
256500150922     c                   else
256600150715     C                   WRITE     fiVAT000
256700150922     c                   end
256800150922      *
256900150715     c                   enddo
257000150715      *
257100150715     c                   end
257200150715      *
257300151109     c     fine_MSG      endSR
257400130904      *----------------------------------------------------------------
257500130904      * DETERMINO DATA E CHIAVE SPEDIZIONE
257600130904      *----------------------------------------------------------------
257700130904     C     SUB_keybol    BEGSR
257800130904      *
257900130904      **              **------------------**
258000130904      *    deve controllare sulla tabella "3C" se il numero spedizione
258100130904      *     deve essere mantenuto oppure no
258200130904     C                   clear                   Ds3C
258300130904     C                   Z-ADD     1             TBLKUT
258400130904     C                   MOVEL     '3C'          TBLCOD
258500130904     C                   movel(p)  VABccm        TBLKEY
258600130904     C     KTABel        CHAIN     TABEL00F                           30
258700130904     C                   IF        %Found(TABEL00F) and tblflg = *blank
258800130904     C                   MOVEL     TBLUNI        Ds3C
258900130904     C                   END
259000130904      *
259100130904      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
259200130904     c                   if        §3CFsp <> 'S'
259300130904      *
259400130904     C* NSP => Stacco un numeratore da AZNUM
259500130904     c                   movel     kpjbu         svkpjbu
259600130904     c                   clear                   kpjbu
259700130904     C                   clear                   TRUL33DS
259800130904     C                   eval      I33OPE = *zeros
259900130904     C                   eval      I33CNU = 302
260000130904     C                   eval      I33NUM = 1
260100130904     C                   movel     TRUL33DS      KPJBU
260200130904     C                   call      'TRUL33R'
260300130904     C                   parm                    KPJBA
260400130904     C                   movel     KPJBU         TRUL33DS
260500130904     C                   if        O33ERR = *zeros
260600130904     c                   z-add     o33nrf        NUM_SPED
260700130904     C                   endif
260800130904     c                   movel     svkpjbu       kpjbu
260900130904      ****
261000130904     c                   ELSE
261100130904      *   se si vuole mantenere il numero spedizione
261200130904      ****
261300130904     C                   MOVE      UYEAR         KAAA
261400130904     C                   Z-ADD     3             KCNU
261500130904     C                   Z-ADD     VABLNP        KFIL
261600130904     C     KNUF          CHAIN     FLNUF                              9999
261700130904     C                   IF        *IN99 = *OFF
261800130904     C                   ADD       1             NUFNUM
261900130904     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE E
262000130904     C                   UPDATE    FLNUF
262100130904     c                   END
262200130904      *
262300130904     c                   EndIF
262400130904      **
262500160408     ******  CONTROLLA se per caso la chiave completa della bolla sia già
262600160408     ******   presente o su EDIVAB o su FIVAB--> se SI, non deve scrivere
262700160408     ******   la spedizione ma deve provare a STACCARE un altro NR dal contatore
262800160408     ******   al max. eseguendo 100 tentativi... dopo di che esce dal programma
262900160408     ******   segnalando il problema del Contatore con NUMERI già presenti.
263000160408      **
263100130904      **              **------------------**
263200130904     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE E
263300130904      **              **------------------**
263400160408      **
263500150701      ** se mandano più PARCEL sotto la stessa spedizione,
263600150701      *  serve per riportare il Predict su più spedizioni.
263700150701     c                   add       1             nr
263800150701     c                   z-add     vabNSP        nr_SPED(nr)
263900130904      *
264000130904     C                   MOVE      UYEAR         KAAA
264100130904     C     2000          add       kaaa          VABAAS
264200130904      *
264300130904     C                   ENDSR
264400130904      *----------------------------------------------------*
264500130904      *   DEFINIZIONE CHIAVI                               *
264600130904      *----------------------------------------------------*
264700130904     C     *INZSR        BEGSR
264800130904      *
264900130904     c     *ENTRY        PLIST
265000130904     C                   parm                    esito             1
265100150916     C                   parm                    Err_dettaglio     1
265200130904     C                   eval      esito ='0'
265300150916     C                   clear                   Err_dettaglio
265400130904      *
265500150625     C     key_VTG       KLIST
265600150625     C                   KFLD                    cldCOD
265700150625     C                   KFLD                    cldDEP
265800150922      *
265900150922     C     key_Depot     KLIST
266000150922     C                   KFLD                    vers_CAP_DPD
266100150922     C                   KFLD                    DepotParcel
266200150922      *
266300150922     C     key_Dep       KLIST
266400150922     C                   KFLD                    DepotParcel
266500150922     C                   KFLD                    DATEU
266600150629      *
266700130904     C     KNUF          KLIST
266800130904     C                   KFLD                    KAAA
266900130904     C                   KFLD                    KCNU
267000130904     C                   KFLD                    KFIL
267100130904      *
267200130904     C     KTABel        KLIST
267300130904     C                   KFLD                    TBLKUT
267400130904     C                   KFLD                    TBLCOD
267500130904     C                   KFLD                    TBLKEY
267600150714      *
267700150714     C     KTAB          KLIST
267800150714     C                   KFLD                    TBLKUT
267900150714     C                   KFLD                    TBLCOD
268000130904      *
268100150923      *
268200150923     C     KTBE          KLIST
268300150923     C                   KFLD                    TBECOD
268400150923     C                   KFLD                    TBEKE1
268500150923     C                   KFLD                    TBEKE2
268600150923      *
268700130904     C     KORM          KLIST
268800130904     C                   KFLD                    ORMPOE
268900130904     C                   KFLD                    ORMNSR
269000130904     C                   KFLD                    ORMNOR
269100130904     C                   KFLD                    ORMNRV
269200130904     C*
269300130904     C     Kvat4E        KLIST
269400130904     C                   KFLD                    vatTRC
269500130904     C                   KFLD                    vatNOT
269600170925      ****
269700170925     C     kPNDbolla     KLIST
269800170925     C                   KFLD                    pndAAS
269900170925     C                   KFLD                    pndLNP
270000170925     C                   KFLD                    pndNRS
270100170925     C                   KFLD                    pndNSP
270200170925      *
270300150717     C     kVAB01        KLIST
270400150717     C                   KFLD                    vabCCM
270500150717     C                   KFLD                    vabRMN
270600150908      *
270700150908     C     kVAB03        KLIST
270800150908     C                   KFLD                    vabFGS
270900150908     C                   KFLD                    vabCCM
271000150908     C                   KFLD                    vabRMN
271100150717      *
271200150717     C     kVAB04        KLIST
271300150717     C                   KFLD                    vabAAS
271400150717     C                   KFLD                    vabLNP
271500150717     C                   KFLD                    vabNRS
271600150717     C                   KFLD                    vabNSP
271700150717      *
271800150717     C     kVAT01        KLIST
271900150717     C                   KFLD                    VABFGS
272000150717     C                   KFLD                    VABCCM
272100150717     C                   KFLD                    VABAAS
272200150717     C                   KFLD                    VABLNP
272300150717     C                   KFLD                    VABNRS
272400150717     C                   KFLD                    VABNSP
272500150908      *
272600150908     C     keVAT01       KLIST
272700150908     C                   KFLD                    VABFGS
272800150908     C                   KFLD                    VABCCM
272900150908     C                   KFLD                    VABCMR
273000150908     C                   KFLD                    VABCNT
273100150908     C                   KFLD                    VABAAS
273200150908     C                   KFLD                    VABLNP
273300150908     C                   KFLD                    VABNRS
273400150908     C                   KFLD                    VABNSP
273500150714     C*------------------
273600150714      *
273700150714     C* Caricamento Tabella 15
273800150714     C                   Z-ADD     0             X                 4 0
273900150714     C                   Z-ADD     1             TBLKUT
274000150714     C                   MOVEL     '15'          TBLCOD
274100150714     C     KTAB          setll     TABEL00F
274200150714     C     KTAB          reade     TABEL00F
274300150714     C                   DOW       not %Eof(TABEL00F)
274400150714     C     TBLFLG        IFEQ      *BLANKS
274500150714     C                   ADD       1             X
274600150714     C                   MOVEL     TBLKEY        C15(X)
274700150714     C                   MOVEL     TBLUNI        DS15
274800150714     C                   MOVEL     §15COD        ISO(X)
274900150715     C                   MOVEL     §15CIE        CIE(X)
275000150714     C                   END
275100150714     C     KTAB          READE     TABEL00F
275200150714     C                   END
275300130904     C*
275400150827     c* carico nomi delle regioni Italiane
275500150827     c                   z-add     1             TBLKUT
275600150827     c                   movel     '14'          TBLCOD
275700150827     c                   z-add     1             x
275800150827     c     ktab          setll     tabel00f
275900150827     c     ktab          reade     tabel00f
276000150827     c                   dow       not %eof(tabel00f)
276100150827     c                   add       1             x
276200150827     c                   movel     tbluni        reg(x)
276300150827     c     ktab          reade     tabel00f
276400150827     c                   enddo
276500150827     c
276600130904     C*------------------
276700130904     C* Apro file transito o FiVAB pilotato dalla tabella
276800130904      * Caricamento Tabella PESI DPD
276900130904     c                   clear                   XX
277000130904     c                   clear                   DPP
277100130904     c     'DPP'         chain     tntbe000                           31
277200130904      *
277300130904     c                   DOW       *in31 = *off
277400130904      * Se il S.I. è indicato deve essere uguale al mio
277500130904     c                   IF        TBEatb = *BLANKS  and
277600130904     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
277700130904     c                   add       1             xx
277800130904     c                   movel     TBEke1        DPPK(XX)
277900130904     c                   movel     TBEuni        DDPP
278000130904     c                   movel     §DPPpkb       DPP(XX)
278100141211     c                   movel     §DPPvlb       vlb(XX)
278200130904     c                   endif
278300130904     c     'DPP'         reade     tntbe000                               31
278400130904     c                   enddo
278500130904      *
278600130904      * Recupero data e ora
278700130904     C                   TIME                    WORA
278800130904     C                   TIME                    W0140
278900130904      * UDATE IN GGMMAAAA
279000130904     C                   MOVE      W0140         WDTGIO
279100130904      * UDATE IN AAAAMMGG
279200130904     C     *eur          MOVEL     WDTGIO        DATA_eur
279300130904     C     *iso          MOVEL     DATA_eur      dateu
279400130904      *
279500150925     c                   z-add     0             vers_CAP_DPD      5 0
279600150925     c* recupero l'ultima versione di cappario DPD
279700150925     c                   clear                   tisie2ds
279800150925     c                   eval      sie2dri=*all'9'
279900150925     c                   call      'TISIE2R'
280000150925     c                   parm                    tisie2ds
280100150925     c* se trovata versione reperisco  descrizioni deposito
280200150925    2c                   if        sie2ver > 0
280300150925     c                   z-add     sie2ver       vers_CAP_DPD
280400150925     c                   end
280500150922      *
280600130904     C                   ENDSR
280700130904      *  ------------------------------------------------------------------ */
280800130904      *   Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
280900130904      *  ------------------------------------------------------------------ */
281000130904     C     eMail_alert   BEGSR
281100151217     C*>>>>>>>>
281200151217     c                   if        invia_mail_alert  ='N'
281300151217     c                   leaveSR
281400151217     c                   end
281500151217     C*>>>>>>>>
281600130904     C*
281700130904     C                   eval      wrkEml =
281800130906     C                             'Andrea.Bertocchi@brt.it'
281900130904     C*
282000150720     C                   eval      wrkOgg='PROBLEMI UPLOAD DPD GEODATA'
282100130904     C*
282200130906     C                   call(e)   'TIS701C1'
282300130904     C                   parm                    wrkEml          253
282400130906     C                   parm                    wrkEmlcc        253
282500130904     C                   parm                    wrkOgg           44
282600130904     C                   parm                    wrkMsg         5000
282700130906     C                   parm                    wrkEsito          1
282800130904      *
282900130904     C                   eval      wrkEml =
283000130905     C                             'CedAlert@brt.it'
283100130906     C*  ??????????????????
283200130906     c                   goto      endmsg
283300130906     C*
283400130906     C                   call(e)   'TIS701C1'
283500130904     C                   parm                    wrkEml          253
283600130906     C                   parm                    wrkEmlcc        253
283700130904     C                   parm                    wrkOgg           44
283800130904     C                   parm                    wrkMsg         5000
283900130906     C                   parm                    wrkEsito          1
284000130904      *
284100130906     C     endmsg        ENDSR
284200150715      *  ================================================================== */
284300150715      *  Controlla la trasmissione   se completa
284400150715      *  ------------------------------------------------------------------ */
284500150715     c     Check_Trasm   Begsr
284600150715
284700150715     C                   clear                   se_errore
284800150715     C                   clear                   Riga_iNIZIo       1
284900150715     C                   clear                   Riga_fINe         1
285000150715      ** primo  record
285100150715     c     *start        setll     tivin00r
285200150715     c     rileggi       tag
285300150715     c                   read      tivin00r
285400150715      *
285500150715     c                   dow       not %eof(tivin00r)
285600150715     c                   movel(p)  VINDTA        dati
285700150715      *
285800150715      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
285900150715      * le leggo e le escludo.
286000150715     c                   if        dati = *blank
286100150715     C                   eval      vinFLG = '1'
286200150715     c                   update    Tivin000
286300150715     c                   goto      rileggi
286400150715     c                   end
286500150715      *
286600150715     c                   if        %subst(dati:1:5) ='#FILE'
286700150715     c                               and
286800150715     c                             %subst(dati:1:1)<>' '
286900150715     C                   eval        Riga_iNIZIo ='X'
287000150715     c                   LEAVE
287100150715     c                   end
287200150715      *
287300150715      *                /*---------------------- */
287400150715     c                   exsr      Decod_Record
287500150715      *                /*---------------------- */
287600150715      *
287700150715     c                   read      tivin00r
287800150715     c                   enddo
287900150715      **
288000150715      ** ultimo record
288100150715     c     *hival        setll     tivin00r
288200150715     c     leggiAncora   tag
288300150715     c                   readp     tivin00r
288400150715     c                   DOW       not %eof(tivin00r)
288500150715      *
288600150715     c                   movel(p)  VINDTA        dati
288700150715     c                   if        dati = *blank
288800150715      * le ultime righe potrebbero essere vuote prima di finire il messaggio
288900150715      * le leggo e le escludo.
289000150715     C                   eval      vinFLG = '1'
289100150715     c                   update    Tivin000
289200150715     c                   goto      leggiAncora
289300150715     c                   end
289400150715      *
289500150715     c                   if        %subst(dati:1:4) ='#END'  AND
289600150715     c                             %subst(dati:1:1)<>' '
289700150715     C                   eval       Riga_fINe ='X'
289800150715     c                   LEAVE
289900150715     c                   end
290000150715      *
290100150715      *                /*---------------------- */
290200150715     c                   exsr      Decod_Record
290300150715      *                /*---------------------- */
290400150715     c                   readp     tivin00r
290500150715     c                   end
290600150715
290700150715      *      Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
290800150715      *      lo stesso numero trasmissione allora si deve segnalare l'errore
290900150715      *      e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
291000150715      *  -----> Errore
291100150715     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
291200150715     C                   eval      se_errore = 'S'
291300150715     c                   end
291400150715
291500150715     c                   endSR
291600150716      *
291700150716      * *------------------------*
291800150716     C     Syntax        begSR
291900150716      * *------------------------*
292000150716      *
292100150716     c                   movel     dati          TxtInOut
292200150716     c                   eval      ElencoChar = SETA
292300150716     c                   eval      TipoElenco = *blank
292400150716     c                   eval      CharSost   = *blank
292500150716     c                   eval      UpperCase  = '1'
292600150716     c                   eval      ChkNull    = *blank
292700150716     c                   eval      CharNull   = *blank
292800150716     c                   eval      Esito_check= *blank
292900150716      *
293000150716     c                   call      'XCHKCHAR'
293100150716     c                   parm                    TxtInOut
293200150716     c                   parm                    ElencoChar
293300150716     c                   parm                    TipoElenco
293400150716     c                   parm                    CharSost
293500150716     c                   parm                    UpperCase
293600150716     c                   parm                    ChkNull
293700150716     c                   parm                    CharNull
293800150716     c                   parm                    Esito_check
293900150716      *
294000150716      *   se è stato modificato qualcosa nella riga di FLAT
294100150716     c                   if        Esito_check = '1'
294200150716     c                   movel     TxtInOut      dati
294300150716     c                   end
294400150716      *
294500150716     C                   ENDSR
294600150916      *  ------------------------------------------------------------------ */
294700150916      *       X non bloccare in nessun caso il traduttore CLIENTI
294800150916      *  ------------------------------------------------------------------ */
294900150916     C     *pssr         BEGSR
295000150916     C
295100150916     C                   eval      esito = '2'
295200150916     C*
295300150916     C                   eval      wrkMsg='Il File DPD GEODATA è arrivato inc'+
295400150916     C                             'ompleto. Controllare UPLOAD ($X) '+
295500151102     C                             'GEODATA  con stato 2: ' + NomeFile
295600151103     c                             + ' da riga : ' + %editc(NRR_shipm:'Z') + ' -
295700151103     c                                 riga in errore : '
295800151103     c                             + %editc(Riga_errata:'Z')
295900151103     c
296000150916      **    avviso tramite MAIL
296100150916     C                   exSR      eMail_alert
296200150916      **
296300150916     C                   exSR      sped_Errata
296400150916      **
296500150916     C                   COMMIT
296600150916      **
296700150916     C                   ENDSR     '*CANCL'
296800150916      * ?------------------------------------------------------------------ */
296900150916      *?    Flagga il TIVIN con Spedizione ERRATA
297000150916      * ?------------------------------------------------------------------ */
297100150916     C     SPED_Errata   BEGSR
297200150916      *
297300150916     c                   clear                   altra_shipm       1
297400150916     C                   eval      Err_dettaglio = 'S'
297500150916      *
297600150916      *   Scrive su tutti i records l'errore
297700150916     c     NRR_shipm     setll     tivin00r
297800150916     c                   read      Tivin00r
297900150916      *
298000150916      *  Lettura sequenziale
298100150916     c                   dow       not %Eof(Tivin00r)
298200150916     c                   if        %subst(VINdta:1:8) ='SHIPMENT'  or
298300150916     c                             %subst(VINdta:1:4) ='#END'
298400150916     c                   if          altra_shipm ='S'
298500150916     c                   leave
298600150916     c                   end
298700150916     c                   end
298800150916     c                   eval       altra_shipm ='S'
298900150916      *
299000150916     C                   eval      vinMSG  = 'PROBLEMI SPEDIZIONE +
299100150916     C                             >> su DATI DETTAGLIO !!'
299200151103     c                   if        nrr_vin = Riga_errata
299300151103     C                   eval      vinMSG  = %Trim(vinMSG) + '  <-- Riga Errata'
299400151103     c                   end
299500150916      *
299600150916     C                   eval      vinFLG = '2'
299700150916     c                   eval      esito  = '1'
299800150916     c                   eval      se_errore ='S'
299900150916     c                   eval      err_bloccante ='S'
300000150916      *
300100150916     c                   update    tivin000
300200150916     c                   read      Tivin00r
300300150916      *
300400150916     c                   endDO
300500150916      *
300600150916     C                   ENDSR
300700150916      * ?------------------------------------------------------------------ */
300800151030     c     wri_wfieu05   begsr
300900151030      **
301000151030     c                   movel     dateu         F05udate
301100151030     c                   move      vabrmn        F05PARCEL
301200151030     c                   movel     F05PARCEL     F05DEPOT
301300151030     c                   move      vabLNP        F05lnp
301400151030     c                   z-add     gg_incr       F05GIO
301500151030     c                   move      SND_SCOUNTRYCDF05NAZ
301600151030     c                   move      SND_SZIPCODE  F05CAP
301700151030     c                   movel     VABCMR        F05CMR
301800151030     c                   movel     SHP_CDATE     F05DATCMR
301900151030     C                   write     fieu005
302000151030      **
302100151030     C                   ENDSR
302200130904      *------------------------------------------------------------------------- -
302300130904      *   Skiere definizione Obbligatorierà dei campi
302400130904      *     M=Mandatory    C=Condition    O=Optional
302500130904      *     M=Obbligatorio C=Condizionale O=Facoltativo
302600130904      * e Skiere definizione Alfanumericità o Numericità dei campi A/N
302700130904      *------------------------------------------------------------------------- -
302800150702**  SK_HED_O
302900150702MMM
303000150702**  SK_HED_N
303100150702AAA
303200130905**  SK_SHP_O
303300150702MMMCMOOOOOOMOCMMOMOOMOOOMOOOOOOOCO
303400130905**  SK_SHP_N
303500150702ANAAANAAAAANNNANNAAANNNNNAAAAAAANA
303600130905**  SK_SND_O
303700150702MMCOCOCOMOOOOOOMCMMOCOCOOOOC
303800130905**  SK_SND_N
303900150702ANANAAAAAAAAAAAAAAAAAAAANNNA
304000130905**  SK_RCV_O
304100150702MMOCOCOCMOOOOOOMCMMOOOOOOOOOO
304200130905**  SK_RCV_N
304300150702ANAAAAAAAAAAAAAAAAAAAAANAAAAA
304400130905**  SK_PRC_O
304500141007MMCCOOOOOOMOOOOCCOOOOOCOOOOOCO
304600130905**  SK_PRC_N
304700141007ANAANAAAAANAAAANNNNAANAAAAAAAA
304800130904**  SK_MSG_O
304900130904MMOOMOMMOO
305000130904**  SK_MSG_N
305100130904ANAANNANAA
305200150907**  PAROLE
305300150914VIA
305400150907V.LE
305500150914VIALE
305600150907C.SO
305700150914CORSO
305800150907C.DA
305900150907CDA
306000150914CONTRADA
306100150907LOC.
306200150914LOCALITA
306300150907P.ZA
306400150907PZZA
306500150907PIAZZA
306600150914PIAZZALE
306700150914STR.
306800150907STRADA
306900150907Z.I.
307000150907ZONA
307100150907FRAZ.
307200150907FRAZIONE
307300150914V.LO
307400150914VICOLO
307500150914LARGO
307600150907PODERE
307700150907LUNGOMARE
307800150907LUNGOLAGO
