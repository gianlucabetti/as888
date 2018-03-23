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
002000171020      *
002100171020     FdpCDP12i  IF   E           K DISK
002200171020      *
002300150922      *--
002400160404     Ffnblp01l  IF   E           K DISK
002500150921     Ffnarb01l  IF   E           K DISK
002600170925      *----
002700150717     FTNCLD00F  IF   E           K DISK
002800130904     FFiVAT02L  if   E           K DISK    rename(FIVAT000:FIVAT02)
002900150717     FFiVAB04L  uf   E           K DISK    rename(FIVAB000:FIVAB04) commit
003000130904      *
003100150714     FFiVAB01L  UF A E           K DISK    commit
003200150714     FFiVAT01L  Uf A E           K DISK    commit
003300150908      *
003400150908     FEdiVAT4L  if   E           K DISK    rename(EDIVAT00:EDIVAT4)
003500150908     FEdiVAB3L  UF A E           K DISK    commit
003600150908     FEdiVAT1L  Uf A E           K DISK    commit
003700151030     Fwfieu05F  if A E             DISK
003800160121      *
003900160121     FTIDP200F  UF   E           K DISK
004000160229     Fwfieu00F  uf A E           k DISK
004100171009     D*------------------------------------------------------------------------*
004200150916     d VINds           ds
004300150916     d  nrr_VIN              397    400b 0
004400150916     d NRR_shipm       s                   like(nrr_VIN)
004500151103     d Riga_errata     s                   like(nrr_VIN)
004600150916      ** -------------------------------------------------------- *
004700150720      *  VERSIONE ULTIMA da COMPARARE
004800150720      *
004900150720     D Ultima_VERSIONE...
005000171009     D                 C                   '03.10'
005100171020      ** ---------------------------------------------------------*
005200171020     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
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
007000130904      *  * ------------------------------------------------------ *
007100130904      *     * NOMI INTESTAZIONE CAMPI x segnalare l'errore su VINMSG
007200130904      *  * ------------------------------------------------------ *
007300160210     D Testa_Sk        s            256    DIM(60)
007400160210     D H_HED           s            256    DIM(60)
007500160210     D H_SHP           s            256    DIM(60)
007600160210     D H_SND           s            256    DIM(60)
007700160210     D H_RCV           s            256    DIM(60)
007800160210     D H_PRC           s            256    DIM(60)
007900160210     D H_MSG           s            256    DIM(60)
008000130904      *
008100130904      ** ------------------------------- *
008200130904      *  obbligatorietà e numericità dei campi
008300130904      ** ------------------------------- *
008400130904     D OBBL            s              1    DIM(60)
008500130904     D NUME            s              1    DIM(60)
008600150716     D UTIL            s              1    DIM(60)
008700150722     D MSGER           s             30    DIM(60)
008800130904      ** ----------------------------------------------- *
008900130904      *  Ds descrittive del dettaglio della spedizione :
009000130904      ** ----------------------------------------------- *
009100150702     D Fieu05RHED    E DS                  prefix(HED_)                          3 campi
009200150702      **
009300130905     D Fieu05RSHP    E DS                  prefix(SHP_)                         28 campi
009400130904      **
009500130905     D Fieu05RSND    E DS                  PREFIX(SND_)                         25 campi
009600130904      **
009700130905     D Fieu05RRCV    E DS                  PREFIX(RCV_)                         27 campi
009800130904      **
009900130905     D Fieu05RPRC    E DS                  PREFIX(PRC_)                         22 campi
010000130904      **
010100130905     D Fieu05RMSG    E DS                  PREFIX(MSG_)                         10 campi
010200130904      *----------------------------------------------------*
010300130904      * Schiere per conversione peso DPD
010400130904      *----------------------------------------------------*
010500130904     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
010600130904     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
010700141211     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
010800150924
010900150924     d Wdata8          DS
011000150924     d  dadata                 1      8  0
011100150924     d  adata                  9     16  0
011200150924     d  GioLav                17     21  0
011300130904      *-------------------
011400130904     D Tipo_error      S              1  0
011500150716     D Parcel14        S             14
011600150716     D DepotParcel     S              4
011700171009     D Lung_Sdepot     S              3  0
011800130904      *-------------------
011900130904     D W015A           S             15
012000130904     D W0140           S             14  0
012100130904     D WORA            S              6  0
012200130904     D XX              S              3  0 INZ
012300130904     D WDTGIO          S              8  0
012400130904     D DATEU           S              8  0
012500130904     D DATA_eur        S               D   DATFMT(*eur)
012600150923     D DATA_iso        S               D   DATFMT(*iso)
012700130904     D KAAA            S                   LIKE(NUFAAA)
012800130904     D KCNU            S                   LIKE(NUFCNU)
012900130904     D KFIL            S                   LIKE(NUFFIL)
013000130904     D NUM_Sped        s                   LIKE(vabnsp)
013100130904     D svkpjbu         s                   like(kpjbu)
013200150701     D nr_SPED         s              7S 0 DIM(999)
013300150717     D kg0_SPED        s              3S 0 DIM(999)
013400150916     D RifNUM          s             15S 0 DIM(999)
013500150925     D tariffa_VTG     s                   like(vabctr)
013600151012     D tariffa_LHS     s                   like(vabctr)
013700130904      *-------------------
013800130904     D KPJBA         E DS
013900130904     D DDPP          E DS
014000150923     D DLPD          E DS
014100130904     D OG143         E DS
014200151016     D fieuLPDds     E DS
014300160404     D fnlsi7ds      E DS
014400130904      ** ------------------------------------------------------------------ */
014500171010     D prmCPD10F     e DS                  extname(DPCDP10F)
014600171010     d                                     prefix(PRM_)
014700171010     D prmLEG10F     e DS                  extname(DPLEG10F)
014800171010     D TisieDEPds    E DS
014900171010      ** ------------------------------------------------------------------ */
015000170922     d dPNDflo       e ds
015100130904     D DVate         E DS
015200150713     D DVats         E DS
015300150714      * Tabella codici nazioni
015400150714     D C15             S              3    DIM(500)
015500150714     D ISO             S              3    DIM(500)
015600150715     D CIE             S              3    DIM(500)
015700150827      * Tabella Regioni
015800150827     D REG             S             25    DIM(55)
015900130904      ** ------------------------------------------------------------------ */
016000130904      * Numeratore Bolle (302)
016100130904     D trul33ds      E DS
016200130904     D Ds3C          E DS
016300150714     D Ds15          E DS
016400130904     D TISI95DS      E DS
016500171010     D DlegFLO       E DS
016600130904      *  ================================================================== */
016700130904      *     * Campi da tradurre con interi
016800130904     D atoi            PR            10I 0
016900130904     D                                     EXTPROC('atoi')
017000130904     D  Char                           *
017100130904     D                                     VALUE
017200130904     D                                     OPTIONS(*STRING)
017300130904      *     *--------------------------------------------------------------*
017400130904      *     ( Descrizione Campi x scomposizione FLAT record )
017500130904      *     *--------------------------------------------------------------*
017600130904      *
017700130904      *     * Campi x decodifica * (INPUT  del Record)
017800130904     D  Dati           s           1500
017900130904     D Separa_campi    s              1
018000130904     D Decimal_Separ   s              1
018100130904      *     * Campi decodificati * (OUTPUT del Record)
018200130904      *       Schiere di Output / Campi di Output
018300130904     d  Sk             s              3u 0
018400130904     D    NR_campi     s              3u 0
018500130904     D  Campi_Record   ds
018600160210     D    Dato_sk                   256    DIM(100)
018700130904     D  Campi_Lunghi   ds
018800130904     D    LunDato_sk                  3u 0 DIM(100)
018900130904     D Campi_Numerici  ds
019000130904     D    DatoNum_sk                  1    DIM(100)
019100130904     D Campi_Decimali  ds
019200130904     D    Decimal_sk                  3u 0 DIM(100)
019300130904      *     *--------------------------------------------------------------*
019400150715     d MSG_ServizioMail_Predict...
019500150713     d                 s            100A
019600150715     d MSG_MAIL_come_Predict...
019700150619     d                 s              1A
019800150715     d MSG_ServizioSMS_Predict...
019900150619     d                 s             70A
020000150715     d MSG_SMS_come_Predict...
020100150619     d                 s              1A
020200150715     d EMAIL_no_Predict...
020300150715     d                 s              1A
020400150715     d WRT_MAIL_SMS...
020500150715     d                 s              1A
020600150619      *-----
020700130904     D  position       s              3  0 INZ(0)
020800130904
020900130904     D  se_errore      s              1    inz(' ')
021000130904     D  Msg_Err        s            132    inz(' ')
021100130904     D  Msg_Vin_80     s             80    inz(' ')
021200151102     D  NomeFILE       s            150    inz(' ')
021300130904      *  * ------------------------------------------------------ *
021400151217     d invia_mail_alert...
021500151217     d                 s              1    inz('N')
021600151217      *  * ------------------------------------------------------ *
021700130904     d non_inviato_prima...
021800130904     d                 s              1
021900130904      **
022000130904     D Obbligatorio    C                   'M'
022100130904     D Condizionato    C                   'C'
022200130904     D Facoltativo     C                   'O'
022300130904     D  Numerico       C                   'N'
022400130904     d Normal_Parcel   c                   101
022500130904     d  Small_Parcel   c                   136
022600130904     d Return_Sender   c                   298
022700130904     d Return_Parcel   c                   300
022800130904     d Normal_Parcel_327...
022900130904     d                 c                   327
023000130904     d Normal_Parcel_329...
023100130904     d                 c                   329
023200130904     d Small_Parcel_328...
023300130904     d                 c                   328
023400141211     d Gomme_365       c                   365
023500141211     d Gomme_366       c                   366
023600141211     d Gomme_367       c                   367
023700160121     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
023800150716      * ?------------------------------------------------------------------ */
023900150716      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
024000150716      * ?------------------------------------------------------------------ */
024100150716     D TxtInOut        S           2048
024200150716     D ElencoChar      S            256
024300150716     D TipoElenco      S              1
024400150716     D CharSost        S              1
024500150716     D UpperCase       S              1
024600150716     D ChkNull         S              1
024700150716     D CharNull        S              1
024800150716     D Esito_check     S              1
024900150716      * ?------------------------------------------------------------------ */
025000150716      * ?controllo x CEDINTESA
025100150716     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
025200150916     D                                     3456789 .,-()/=''+:?! %&*;<>#@_°£$')
025300150716     d bart_it         C                   CONST('@Bartolini.it;')
025400150716     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
025500150716     D   Digits        C                   '0123456789'
025600150827
025700150827     D digitsplus      c                   '0123456789 /-'
025800150907     D solosimboli     c                   ' .-_/*+:,§''=\|!?%&$£"ç°#@'
025900150828     D ITALIA          c                   'ITALIA'
026000150828     D ITALY           c                   'ITALY'
026100150828     D IT              c                   'IT'
026200150716     D*------------------
026300150716      * ?set caratteri Maiuscoli/Minuscoli
026400150716     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
026500150716     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
026600130904      *  ================================================================== */
026700130904      *   Ciclo principale
026800130904      *  ********************************************************************/
026900160408     c                   SETON                                        LR
027000130904      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
027100130904     C*
027200130904     C                   if        not %open(tivin00r)
027300130904     C                   open      tivin00r
027400130904     C                   endif
027500130904      **
027600130904      *  ------------------------------------------------------------------ */
027700130904     C*  Controllo dati arrivati da DPD
027800130904      *  ------------------------------------------------------------------ */
027900130904      *  - Occorre fare un primo test sull'integrità della trasmissione
028000130904      *  - controllando che la trasmissione sia completa.
028100130904
028200130904      *                /*---------------------- */
028300130904     c                   exsr      check_Trasm
028400130904      *                /*---------------------- */
028500130904      **
028600130904      **  Errore di trasmissione x tutti i records
028700130904      **   --> file in errore
028800130904     c                   if        se_errore = 'S'
028900150720      *                 /* ------------------ */
029000150720     c                   exsr      TUTTO_ERRATO
029100150720      *                 /* ------------------ */
029200130904     c                   else
029300150720      *  --------------------------------------------- */
029400150720     C*  Se TUTTO OK esegue l'importazione delle Bolle */
029500150720      *  --------------------------------------------- */
029600130904      *                /*---------------------- */
029700130904     c                   exsr      Importa_Msg
029800130904      *                /*---------------------- */
029900130904     c                   end
030000130904
030100150714     c                   COMMIT
030200150714
030300130904     C                   if        %open(tivin00r)
030400130904     C                   close     tivin00r
030500130904     C                   endif
030600130904      *
030700160408     c                   RETURN
030800150720      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
030900150720      *  Tutto il Messaggio in ERRORE                                       */
031000150720      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
031100150720     c     TUTTO_errato  Begsr
031200150720      *
031300150720      ** Messaggio da riportare su ogni record x tutta la trasmissione
031400150720     C                   eval      Msg_Err = 'FILE GEODATA DPD ERRATO +
031500150720     C                              >> o VERSIONE SUPERIORE al TRADUTTORE-
031600150720     C                               << CONTROLLARE !!'
031700150720      *
031800150720      *    Scrive su tutti i records il tipo di errore
031900150720     c     *start        setll     tivin00r
032000150720     c                   read      tivin00r
032100150720     c                   dow       not %eof(tivin00r)
032200150720     C                   eval      vinMSG = Msg_Err
032300150720     C                   eval      vinFLG = '2'
032400150720     c                   eval      esito  = '2'
032500150720     c                   update    tivin000
032600150720     c                   read      tivin00r
032700150720     c                   endDO
032800150720      *
032900150720      **    avviso tramite MAIL
033000150720     C*
033100150720     C                   eval      wrkMsg='Il File DPD GEODATA è arrivato inc'+
033200150720     C                             'ompleto. Controllare UPLOAD ($X) '+
033300150720     C                             'oppure VERSIONE SUPERIORE a quella previst-
033400150720     c                             a nel TRADUTTORE.  -  ' +
033500151102     C                             'GEODATA  con stato 2: '+  nomefile +
033600151102     C                             ' TUTTI i records NON SONO STATI TRADOTTI !'
033700150720     C*
033800150720     C                   exSR      eMail_alert
033900150720      *
034000150720     c                   Endsr
034100150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
034200150715      *  Importa i records della trasmissione                               */
034300150715      *  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
034400150715     c     Importa_Msg   Begsr
034500150715
034600150715      * per controllare se almeno un record è stato importato sul VAB
034700150715     c                   clear                   Almeno_Uno        1
034800150715
034900150715     c     *start        setll     Tivin00r
035000150715     c                   read      Tivin00r
035100150715
035200150715     c                   dow       not %eof(Tivin00r)
035300150715
035400150715      * solo i record sflaggati da rielaborare
035500150715     c                   IF        vinFLG = *blank and vinDTA <> *blank
035600150715      ** Controlli formali sui campi
035700150715     c                   clear                   se_errore
035800150715     C                   clear                   Msg_Vin_80
035900150715
036000150715      ** Decodifica record a campi variabili
036100150715      *                /*---------------------- */
036200150715     c                   exsr      Decod_Record
036300150715      *                /*---------------------- */
036400150715
036500150715      **  Se presente un errore nel record emette una segnalazione msg
036600150715     c                   if        se_errore ='S'
036700150715     C                   eval      vinMSG = Msg_Vin_80
036800150715     c                   end
036900150715      *  Sempre Record OK
037000150715     C                   eval      vinFLG = '1'
037100150916     c                   endIF
037200150715
037300150715     c                   if        err_bloccante ='S'
037400150715     C                   eval      vinFLG = '2'
037500150715     c                   eval      esito  = '1'
037600150715     c                   end
037700151022
037800151022     c                   if        gia_in_BLP ='S'
037900151022     c                                and %subst(vindta:1:6) = 'PARCEL'
038000151022     C                   eval      vinMSG = %trim(Vinmsg) + ' ' + 'GEO_in_BLP'
038100151022     c                   elseif    cancellato ='S'
038200151022     c                                and %subst(vindta:1:6) = 'PARCEL'
038300151022     C                   eval      vinMSG = %trim(Vinmsg) + ' ' + 'GEO_doppio'
038400151022     c                   elseif      dati_CONSEGNA ='N'
038500151022     c                   eval      vinmsg = %Trim(vinmsg) + ' ' +
038600151022     c                                  'NO_dati_CONSEGNA'
038700151022     c                   end
038800150715
038900150715     c                   update    Tivin000
039000150720      **
039100150720      **  Se la VERSIONE del messaggio è successiva a questo TRADUTTORE
039200150720      **   deve uscire bloccando tutti i records e avvisando con EMAIL
039300150720      **   di controllare SENZA TRADURRE NULLA.
039400150720     c                   if        HED_VERSION > Ultima_VERSIONE
039500150720     c                   exsr      Tutto_ERRATO
039600150720     c                   leave
039700150720     c                   end
039800150720      **
039900150715     c                   read      Tivin00r
040000150715     C                   ENDdo
040100150715      **
040200150715     c                   endSR
040300130904      *  ------------------------------------------------------------------ */
040400130904      *     Decodifica record a campi variabili
040500130904      *  ------------------------------------------------------------------ */
040600130904     c     Decod_Record  Begsr
040700130904
040800130904      *   Sposta il record a tracciato libero su un campo di lavoro       =
040900150716     c                   clear                   dati
041000150716     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
041100150716     c                   exsr      Syntax
041200150716      **
041300130904     c                   EVAL      err_bloccante =' '
041400130904      **
041500130904      *                /*---------------------- */
041600130904     c                   exsr      Split_Rec
041700130904      *                /*---------------------- */
041800130904      **
041900130904      **  Records di Intestazione e di fine Msg
042000130904     c                   if        %subst(dati:1:1) ='#'
042100130904      *                /*---------------------- */
042200130904     c                   exsr      Rec_Testate
042300130904      *                /*---------------------- */
042400130904     c                   Else
042500130904      ** conta le righe
042600130904      *                /*---------------------- */
042700130904     c                   exsr      Rec_Dettaglio
042800130904      *                /*---------------------- */
042900130904     c                   End
043000130904      **
043100130904     c                   Endsr
043200130904      *  ------------------------------------------------------------------ */
043300130904      *       Record di Testata e di Fine Msg
043400130904      *  ------------------------------------------------------------------ */
043500130904     c     Rec_Testate   Begsr
043600151022      **
043700151022     c                   clear                   vinmsg
043800130904      **
043900130904      ** esempio di record :
044000130904      *     #FILE;GEODATA_SHPNOT_1550_N00_D20130729_T170546
044100130904      *     #ENCODING;ISO 8859-1
044200171009      *     #VERSION;3.10
044300150702      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;
044400150702      *     #DEF;GEODATA:SHIPMENT;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MP
044500150702      *     #DEF;GEODATA:SENDER;NUMORDER;SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPN
044600150702      *     #DEF;GEODATA:RECEIVER;NUMORDER;RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPN
044700150702      *     #DEF;GEODATA:PARCEL;NUMORDER;PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRA
044800150702      *     #DEF;GEODATA:MSG;NUMORDER;NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE;
044900130904      **     .....
045000130904      **      .....
045100130904      *     #END;
045200130904      **
045300130904      *    Prima riga
045400130904     c     'FILE'        SCAN      dati:2        position
045500130904     c                   if        %Found and position < 5
045600130904     C                   move      'S'           Riga_iNIZIo
045700151102     c                   eval      NomeFILE = dati
045800150715     C                   move      *blank        Riga_fINe
045900130904     c                   ElSe
046000130904     c                   End
046100130904      **
046200130904      *    Ultima riga
046300130904     c     'END'         SCAN      dati:2        position
046400130904     c                   if        %Found and position < 5
046500130904     C                   move      'S'           Riga_fINe
046600150715      **
046700150721      * CONTROLLA COME SE DOVESSE FARE UN NUOVO DETTAGLIO
046800150721      **  x l'ultimo giro prima di andare in LR
046900150721     C                   exsr      New_DETTAGLIO
047000150715      **------
047100130904     c                   End
047200130904      **
047300130904      *    DEFINIZIONI  (testate dei campi)
047400130904     c     'DEF'         SCAN      dati:2        position
047500130904      **
047600130904     c                   if        %Found and position < 5
047700130904      **  IMPOSTA I NOMI DEI CAMPI SU SKIERE
047800150702      *     #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;
047900150702      *     #DEF;GEODATA:SHIPMENT;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MP
048000150702      *     #DEF;GEODATA:SENDER;NUMORDER;SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPN
048100150702      *     #DEF;GEODATA:RECEIVER;NUMORDER;RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPN
048200150702      *     #DEF;GEODATA:PARCEL;NUMORDER;PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRA
048300150702      *     #DEF;GEODATA:MSG;NUMORDER;NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE;
048400130904      **
048500130904      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
048600150702      **   eliminando il primo elemento che è sempre "#DEF"
048700150702      *  quindi muove dal 2°campo nella relativa schiera di testata nome campi
048800150702      **
048900130904     c                   IF        %subst(dati:14:6) ='HEADER'                     H_HED
049000150702     c                   movea     Dato_sk(2)    H_HED
049100130904      *
049200130904     c                   ELSEif    %subst(dati:14:8) ='SHIPMENT'                   H_SHP
049300150702     c                   movea     Dato_sk(2)    H_SHP
049400130904      *
049500130904     c                   ELSEif    %subst(dati:14:6) ='SENDER'                     H_SND
049600150702     c                   movea     Dato_sk(2)    H_SND
049700130904      *
049800130904     c                   ELSEif    %subst(dati:14:8) ='RECEIVER'                   H_RCV
049900150702     c                   movea     Dato_sk(2)    H_RCV
050000130904      *
050100130904     c                   ELSEif    %subst(dati:14:6) ='PARCEL'                     H_PRC
050200150702     c                   movea     Dato_sk(2)    H_PRC
050300130904      *
050400130904     c                   ELSEif    %subst(dati:14:3) ='MSG'                        H_MSG
050500150702     c                   movea     Dato_sk(2)    H_MSG
050600130904      *
050700130904     c                   End
050800130904      **
050900130904     c                   End
051000130904      **
051100130904     c                   Endsr
051200130904      *  ------------------------------------------------------------------ */
051300130904      *       Riga Bolla Import da controllare e tradurre
051400130904      *  ------------------------------------------------------------------ */
051500130904     c     Rec_Dettaglio Begsr
051600130904      **
051700130904     c                   clear                   Testa_sk
051800130904     c                   clear                   Msg_Err
051900151103      ** ------------
052000151103     c                   eval      Riga_errata = Riga_errata + 1
052100130904      ** ------------
052200160210     c                   IF        %subst(Dato_sk(1):1:+LunDato_sk(1))
052300130904     c                               ='HEADER'
052400130904     c                   exsr      sr_HEADER
052500130904      ** ------------
052600130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
052700130904     c                               ='SHIPMENT'
052800150717     c                   exsr      new_DETTAGLIO
052900130904     c                   exsr      sr_SHIPMENT
053000130904     ** ------------
053100130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
053200130904     c                               ='SENDER'
053300130904     c                   exsr      sr_SENDER
053400130904      ** ------------
053500130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
053600130904     c                               ='RECEIVER'
053700130904     c                   exsr      sr_RECEIVER
053800130904      ** ------------
053900130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
054000130904     c                               ='PARCEL'
054100130904     c                   exsr      sr_PARCEL
054200130904      ** ------------
054300130904     c                   elseIF    %subst(Dato_sk(1):1:+LunDato_sk(1))
054400130904     c                               ='MSG'
054500130904     c                   exsr      sr_MSG
054600130904      **
054700130904     c                   end
054800130904      ** ------------
054900130904     c                   Endsr
055000130904      *  ------------------------------------------------------------------ */
055100130904      *      HEADER
055200130904      *  ------------------------------------------------------------------ */
055300130904     c     sr_HEADER     Begsr
055400130904      **
055500150720     c                   clear                   Fieu05RHED
055600130904     c                   movea     H_HED         Testa_sk
055700150702     c                   movea     SK_HED_O      OBBL
055800150702     c                   movea     SK_HED_N      NUME
055900150716     c                   clear                   UTIL
056000150722     c                   clear                   MSGER
056100150703      **
056200150703      **  #DEF;GEODATA:HEADER;VERSION;CLASSIFICATION;;
056300150703      **
056400150703      * cerca in quale elemento si trova il dato
056500160210     c                   clear                   trova_campo     256
056600150702     c                   eval      trova_campo = 'VERSION'
056700150702     c                   z-add     1             sk
056800150702     c     trova_campo   lookup    Testa_sk(sk)                           21
056900150717     c   21              eval      UTIL(sk) ='S'
057000150702     c   21              eval      HED_VERSION    = %subst(Dato_sk(sk):1:+
057100150702     c                                              LunDato_sk(sk))
057200150720      **
057300150720      * Attenzione se il Messaggio ricevuto è successivo all'ultima versione
057400150720      **  da tradurre con questo programma blocca il messaggio e AVVISA.
057500150720     c   21              if        HED_VERSION > Ultima_VERSIONE
057600150720     c                   eval      se_errore ='S'
057700150720     c                   eval      err_bloccante ='S'
057800150720     C                   eval      Msg_Vin_80 = 'VERSIONE SUCCESSIVA al FORMAT-
057900150720     c                                          O previsto in TRADUZIONE !!!'
058000150720     c                   LEAVEsr
058100150720     c                   end
058200150702      *
058300150715     c                   eval      EMAIL_no_Predict = *blank
058400150715     c                   eval      WRT_MAIL_SMS     = *blank
058500130904      **
058600150716     c                   exsr      ctrl_campi
058700150717      **
058800150717     c                   Endsr
058900150717      *  ------------------------------------------------------------------ */
059000150717      *      Rottura di Spedizione - prima della successiva
059100150717      *  ------------------------------------------------------------------ */
059200150717     c     New_DETTAGLIO Begsr
059300150717      **
059400150717      **  Aggiorna il Peso sulle spedizioni della bolla precedente
059500150717      **   Se era una multicollo e NON tutti i colli avevano il peso sul Parcel
059600150717      **    prende il peso totale rimasto lo divide per i colli che non avevano
059700150717      **     il peso ottenendo un peso medio da dichiarare sulla spedizione.
059800150717     c                   if        nr > 1 and Kg_rimasti > 0
059900150717     c                   xfoot     kg0_SPED      quanti_senzaKG    3 0
060000150717     c                   if        quanti_senzaKG > 0
060100150717     c                   exsr      SPALMA_KG
060200150717     c                   end
060300150717     c                   end
060400150717      **
060500150717      * Inoltre x particolari comunicazioni come PREDICT
060600150717     c                   IF          WRT_MAIL_SMS  = 'S'
060700150916     c                   if         MSG_MAIL_come_Predict ='S'
060800150916     c                               or
060900150916     c                              MSG_SMS_come_Predict  ='S'
061000150916     c                               or
061100150916     c                              EMAIL_no_Predict      ='S'
061200150916      *
061300160217      *** NON SI VUOLE CREARE IL RESO (non modifico le vecchie spec.NON si sa mai)
061400160217      ***   SALTIAMO semplicemente la scrittura  anche dei MSG su VAT
061500160217      ***
061600160217     c                   If        Sped_Reso <>'S'
061700160217      ***                          ==========
061800150717     C                   exsr      WRITE_MSG
061900160217      ***                          ==========
062000160217     C                   endIF
062100160217      ***
062200150916     c                   end
062300150916      *
062400150717     c                   END
062500150717     c                   eval      WRT_MAIL_SMS     = *blank
062600150717     c                   eval      EMAIL_no_Predict = *blank
062700150717      **------
062800150717     c                   eval      WRT_MAIL_SMS = *blank
062900150717     c                   eval      MSG_ServizioMail_Predict = *blank
063000150717     c                   eval      MSG_ServizioSMS_Predict  = *blank
063100150717     c                   eval      MSG_MAIL_come_Predict = *blank
063200150717     c                   eval      MSG_SMS_come_Predict  = *blank
063300150717      **
063400150921     c                   clear                   edivab00
063500150717     c                   clear                   fivab000
063600150717     c                   clear                   nr_SPED
063700150717     c                   clear                   kg0_SPED
063800150717     c                   clear                   nr                5 0
063900150717     c                   clear                   Kg_rimasti        9 1
064000150923     c                   clear                   su_EDIVAB         1
064100151022     c                   clear                   dati_CONSEGNA     1
064200150716      **
064300130904     c                   Endsr
064400150717      *  ------------------------------------------------------------------ */
064500150717      *      SPALMA il peso residuo sui colli rimasti senza
064600150717      *  ------------------------------------------------------------------ */
064700150717     c     SPALMA_KG     Begsr
064800150717      **
064900150717     c     Kg_rimasti    div       quanti_senzaKGPeso_medio        7 1
065000150717      **
065100150717     c                   do        nr            wrt               5 0
065200150717     c                   if        nr_sped(wrt) = 0
065300150717     c                   leave
065400150717     c                   end
065500150717      **
065600150717     c                   if        kg0_sped(wrt) = 1
065700150717     c                   z-add     nr_sped(wrt)  vabNSP
065800150717     c     kvab04        chain     FiVAB04L
065900150717     c                   if        %Found(FIVAB04L)
066000150717     c                   z-add     peso_medio    vabPKB
066100150916      **
066200150916      *
066300150916      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
066400150916     c                   if        vabPKB > 31,5
066500150916     c                   eval           vabPKB = 30
066600150916     c                   EndIF
066700150916      **
066800150717     C                   UPDATE    fiVAB04
066900150717     c                   end
067000150717     c                   end
067100150717     c                   enddo
067200150717      *-----
067300150717     c                   Endsr
067400130904      *  ------------------------------------------------------------------ */
067500130904      *      SHIPMENT
067600130904      *  ------------------------------------------------------------------ */
067700130904     c     sr_SHIPMENT   Begsr
067800130904      **
067900130905     c                   clear                   fieu05RSHP
068000130904     c                   movea     H_SHP         Testa_sk
068100150702     c                   movea     SK_SHP_O      OBBL
068200150702     c                   movea     SK_SHP_N      NUME
068300150716     c                   clear                   UTIL
068400150722     c                   clear                   MSGER
068500150916     c                   eval      NRR_shipm = NRR_VIN
068600151103     c                   eval      Riga_errata=NRR_VIN
068700150703      **
068800150703      *  #DEF;GEODATA:SHIPMENT;NUMORDER;MPSID;CUSTOMSREF;MPSIDCCKEY;MPSCOMP;MPSCREF1;
068900150703      *  MPSCREF2;MPSCREF3;MPSCREF4;MPSBILLREF;MPSCOUNT;MPSVOLUME;MPSWEIGHT;SDEPOT;
069000150703      *  CDATE;CTIME;HARDWARE;RDEPOT;DSORT;SPTDATE;SPTTIME;SPTREALDATE;SPTREALTIME;
069100150703      *  DELMODALLOW;ROUTINGPLANVERSION;SPARTNERREF1;SPARTNERREF2;SPARTNERCODE;OSORT;
069200150703      *  SSORT;MSORT;COLREQUESTFLAG;ROUTINGPLACE;;
069300150921      **
069400160210     c                   clear                   trova_campo
069500150921     c                   eval      trova_campo = 'CDATE'
069600150921     c                   z-add     1             sk
069700150921     c     trova_campo   lookup    Testa_sk(sk)                           21
069800150921     c   21              eval      SHP_CDATE = atoi(%TRIM(Dato_sk(sk)))
069900150921     c   21              eval      UTIL(sk) ='S'
070000150921      **
070100160210     c                   clear                   trova_campo
070200171009     c                   clear                   Lung_SDEPOT
070300150922     c                   eval      trova_campo = 'SDEPOT'
070400150922     c                   z-add     1             sk
070500150922     c     trova_campo   lookup    Testa_sk(sk)                           21
070600171103     c                   clear                   Depot4
070700150922     c   21              eval      SHP_SDEPOT = %subst(Dato_sk(sk):1:+
070800150922     c                                              LunDato_sk(sk))
070900171009     c   21              eval      Lung_SDEPOT = %Len(%trim(SHP_SDEPOT))
071000150922     c   21              eval      UTIL(sk) ='S'
071100171020     c   21              if        Lung_SDEPOT = 4
071200171020     c                   movel     SHP_SDEPOT    Depot4            4
071300171020     c                   exsr      DEP_CAPDPD
071400171020     c                   end
071500150703      **
071600160210     c                   clear                   trova_campo
071700150703     c                   eval      trova_campo = 'MPSCREF1'
071800150703     c                   z-add     1             sk
071900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
072000150703     c   21              eval      SHP_MPSCREF1 = %subst(Dato_sk(sk):1:+
072100150703     c                                              LunDato_sk(sk))
072200150717     c   21              eval      UTIL(sk) ='S'
072300150703      **
072400160210     c                   clear                   trova_campo
072500150703     c                   eval      trova_campo = 'MPSCOUNT'
072600150703     c                   z-add     1             sk
072700150703     c     trova_campo   lookup    Testa_sk(sk)                           21
072800150703     c   21              eval      SHP_MPSCOUNT = atoi(%TRIM(Dato_sk(sk)))
072900150717     c   21              z-add     SHP_MPSCOUNT  multicollo
073000150717     c   21              eval      UTIL(sk) ='S'
073100150722     c   21              if        SHP_MPSCOUNT > 1
073200150722     c                   eval      MSGER(sk) ='Sped.MULTICOLLO'
073300150722     c                   end
073400150703      **
073500160210     c                   clear                   trova_campo
073600150703     c                   eval      trova_campo = 'MPSVOLUME'
073700150703     c                   z-add     1             sk
073800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
073900150703     c   21              eval      SHP_MPSVOLUME = atoi(%TRIM(Dato_sk(sk)))
074000150703      **
074100160210     c                   clear                   trova_campo
074200150703     c                   eval      trova_campo = 'MPSWEIGHT'
074300150703     c                   z-add     1             sk
074400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
074500150703     c   21              eval      SHP_MPSWEIGHT = atoi(%TRIM(Dato_sk(sk)))
074600150716     c   21              eval      UTIL(sk) ='S'
074700150717     c   21SHP_MPSWEIGHT div       100           Kg_rimasti
074800150716      **
074900150716     c                   exsr      ctrl_campi
075000150716      **
075100130904     c                   Endsr
075200130904      *  ------------------------------------------------------------------ */
075300130904      *      SENDER
075400130904      *  ------------------------------------------------------------------ */
075500130904     c     sr_SENDER     Begsr
075600130904      **
075700130905     c                   clear                   fieu05RSND
075800130904     c                   movea     H_SND         Testa_sk
075900150702     c                   movea     SK_SND_O      OBBL
076000150702     c                   movea     SK_SND_N      NUME
076100150716     c                   clear                   UTIL
076200150722     c                   clear                   MSGER
076300150703      **
076400150703      *  #DEF;GEODATA:SENDER;NUMORDER;SCUSTACCNUMBER;SCUSTSUBACCNUMBER;SCOMPNAME;
076500150703      *  SUNIQCUSTID;SNAME1;SNAME2;SSTREET;SPROPNUM;SADD2;SADD3;SFLOOR;SBUILDING;
076600150703      *  SDEPARTMENT;SCOUNTRYCODE;SSTATE;SZIPCODE;STOWN;SCONTACT;SPHONE;SFAX;
076700150703      *  SEMAIL;SCOMMENT;SGLN;SGPSLAT;SGPSLONG;SACCOUNTOWNER;;
076800130904      **
076900160210     c                   clear                   trova_campo
077000150703     c                   eval      trova_campo = 'SNAME1'
077100150703     c                   z-add     1             sk
077200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
077300150703     c   21              eval      SND_SNAME1 = %subst(Dato_sk(sk):1:+
077400150703     c                                              LunDato_sk(sk))
077500150716     c   21              eval      UTIL(sk) ='S'
077600150703      **
077700160210     c                   clear                   trova_campo
077800150703     c                   eval      trova_campo = 'SCOUNTRYCODE'
077900150703     c                   z-add     1             sk
078000150703     c     trova_campo   lookup    Testa_sk(sk)                           21
078100150703     c   21              eval      SND_SCOUNTRYCD
078200150703     c                                        = %subst(Dato_sk(sk):1:+
078300150703     c                                              LunDato_sk(sk))
078400150716     c   21              eval      UTIL(sk) ='S'
078500150722     c   21              if        SND_SCOUNTRYCD = *blank
078600150722     c                   eval      MSGER(sk) ='NO Naz.Mitt.Or.'
078700150722     c                   end
078800150703      **
078900160210     c                   clear                   trova_campo
079000150703     c                   eval      trova_campo = 'SZIPCODE'
079100150703     c                   z-add     1             sk
079200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
079300150703     c   21              eval      SND_SZIPCODE = %subst(Dato_sk(sk):1:+
079400150703     c                                              LunDato_sk(sk))
079500150716     c   21              eval      UTIL(sk) ='S'
079600150722     c   21              if        SND_SZIPCODE = *blank
079700150722     c                   eval      MSGER(sk) ='NO CAP Mitt.Or.'
079800150722     c                   end
079900150703      **
080000150716     c                   exsr      ctrl_campi
080100150716      **
080200130904     c                   Endsr
080300130904      *  ------------------------------------------------------------------ */
080400130904      *      RECEIVER
080500130904      *  ------------------------------------------------------------------ */
080600130904     c     sr_RECEIVER   Begsr
080700130904      **
080800130905     c                   clear                   fieu05RRCV
080900130904     c                   movea     H_RCV         Testa_sk
081000150702     c                   movea     SK_RCV_O      OBBL
081100150702     c                   movea     SK_RCV_N      NUME
081200150716     c                   clear                   UTIL
081300150722     c                   clear                   MSGER
081400130904      **
081500150703      *  #DEF;GEODATA:RECEIVER;NUMORDER;RCUSTID;RNAME1;RNAME2;RCOMPNAME;RCOMPNAME2;
081600150703      *  RPUDOID;RSTREET;RPROPNUM;RADD2;RADD3;RFLOOR;RBUILDING;RDEPARTMENT;
081700150703      *  RCOUNTRYCODE;RSTATE;RZIPCODE;RTOWN;RDOORCODE;RCONTACT;RCONTACTPHO1;
081800150703      *  RCONTACTPHO2;RINTERPHONENAME;RFAX;REMAIL;RCOMMENT;RGPSLAT;RGPSLONG;;
081900150703      **
082000160210     c                   clear                   trova_campo
082100150703     c                   eval      trova_campo = 'RNAME1'
082200150703     c                   z-add     1             sk
082300150703     c     trova_campo   lookup    Testa_sk(sk)                           21
082400150703     c   21              eval      RCV_RNAME1 = %subst(Dato_sk(sk):1:+
082500150703     c                                              LunDato_sk(sk))
082600150716     c   21              eval      UTIL(sk) ='S'
082700150703      **
082800160210     c                   clear                   trova_campo
082900150703     c                   eval      trova_campo = 'RNAME2'
083000150703     c                   z-add     1             sk
083100150703     c     trova_campo   lookup    Testa_sk(sk)                           21
083200150827     c   21              eval      RCV_RNAME2 = %subst(Dato_sk(sk):1:+
083300150703     c                                              LunDato_sk(sk))
083400150827     c   21              eval      UTIL(sk) ='S'
083500150827      **
083600160210     c                   clear                   trova_campo
083700150827     c                   eval      trova_campo = 'RCOMPNAME'
083800150827     c                   z-add     1             sk
083900150827     c     trova_campo   lookup    Testa_sk(sk)                           21
084000150827     c   21              eval      RCV_RCOMPNAME  = %subst(Dato_sk(sk):1:+
084100150827     c                                              LunDato_sk(sk))
084200150827     c   21              eval      UTIL(sk) ='S'
084300150703      **
084400160210     c                   clear                   trova_campo
084500150703     c                   eval      trova_campo = 'RSTREET'
084600150703     c                   z-add     1             sk
084700150703     c     trova_campo   lookup    Testa_sk(sk)                           21
084800150703     c   21              eval      RCV_RSTREET = %subst(Dato_sk(sk):1:+
084900150703     c                                              LunDato_sk(sk))
085000150716     c   21              eval      UTIL(sk) ='S'
085100150827      **
085200160210     c                   clear                   trova_campo
085300150827     c                   eval      trova_campo = 'RPROPNUM'
085400150827     c                   z-add     1             sk
085500150827     c     trova_campo   lookup    Testa_sk(sk)                           21
085600150827     c   21              eval      RCV_RPROPNUM  = %subst(Dato_sk(sk):1:+
085700150827     c                                              LunDato_sk(sk))
085800150827     c   21              eval      UTIL(sk) ='S'
085900150703      **
086000160210     c                   clear                   trova_campo
086100150703     c                   eval      trova_campo = 'RADD2'
086200150703     c                   z-add     1             sk
086300150703     c     trova_campo   lookup    Testa_sk(sk)                           21
086400150703     c   21              eval      RCV_RADD2  = %subst(Dato_sk(sk):1:+
086500150703     c                                              LunDato_sk(sk))
086600150716     c   21              eval      UTIL(sk) ='S'
086700150827      **
086800160210     c                   clear                   trova_campo
086900150827     c                   eval      trova_campo = 'RADD3'
087000150827     c                   z-add     1             sk
087100150827     c     trova_campo   lookup    Testa_sk(sk)                           21
087200150827     c   21              eval      RCV_RADD3  = %subst(Dato_sk(sk):1:+
087300150827     c                                              LunDato_sk(sk))
087400150827     c   21              eval      UTIL(sk) ='S'
087500150703      **
087600160210     c                   clear                   trova_campo
087700150703     c                   eval      trova_campo = 'RCOUNTRYCODE'
087800150703     c                   z-add     1             sk
087900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
088000150703     c   21              eval      RCV_RCOUNTRYCD
088100150703     c                                        = %subst(Dato_sk(sk):1:+
088200150703     c                                              LunDato_sk(sk))
088300150716     c   21              eval      UTIL(sk) ='S'
088400150703      **
088500160210     c                   clear                   trova_campo
088600150703     c                   eval      trova_campo = 'RSTATE'
088700150703     c                   z-add     1             sk
088800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
088900150703     c   21              eval      RCV_RSTATE = %subst(Dato_sk(sk):1:+
089000150703     c                                              LunDato_sk(sk))
089100150716     c   21              eval      UTIL(sk) ='S'
089200150703      **
089300160210     c                   clear                   trova_campo
089400150703     c                   eval      trova_campo = 'RZIPCODE'
089500150703     c                   z-add     1             sk
089600150703     c     trova_campo   lookup    Testa_sk(sk)                           21
089700150703     c   21              eval      RCV_RZIPCODE = %subst(Dato_sk(sk):1:+
089800150703     c                                              LunDato_sk(sk))
089900150716     c   21              eval      UTIL(sk) ='S'
090000150703      **
090100160210     c                   clear                   trova_campo
090200150703     c                   eval      trova_campo = 'RTOWN'
090300150703     c                   z-add     1             sk
090400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
090500150827     c   21              eval      RCV_RTOWN  = %subst(Dato_sk(sk):1:+
090600150703     c                                              LunDato_sk(sk))
090700150827     c   21              eval      UTIL(sk) ='S'
090800150828      **
090900160210     c                   clear                   trova_campo
091000150703     c                   eval      trova_campo = 'RCONTACT'
091100150703     c                   z-add     1             sk
091200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
091300150703     c   21              eval      RCV_RCONTACT = %subst(Dato_sk(sk):1:+
091400150703     c                                              LunDato_sk(sk))
091500150716     c   21              eval      UTIL(sk) ='S'
091600150703      **
091700160210     c                   clear                   trova_campo
091800150703     c                   eval      trova_campo = 'RCONTACTPHO1'
091900150703     c                   z-add     1             sk
092000150703     c     trova_campo   lookup    Testa_sk(sk)                           21
092100150703     c   21              eval      RCV_RCONTPHO1 = %subst(Dato_sk(sk):1:+
092200150703     c                                              LunDato_sk(sk))
092300150716     c   21              eval      UTIL(sk) ='S'
092400150703      **
092500160210     c                   clear                   trova_campo
092600150703     c                   eval      trova_campo = 'RFAX'
092700150703     c                   z-add     1             sk
092800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
092900150703     c   21              eval      RCV_RFAX   = %subst(Dato_sk(sk):1:+
093000150703     c                                              LunDato_sk(sk))
093100150716     c   21              eval      UTIL(sk) ='S'
093200150703      **
093300160210     c                   clear                   trova_campo
093400150703     c                   eval      trova_campo = 'REMAIL'
093500150703     c                   z-add     1             sk
093600150703     c     trova_campo   lookup    Testa_sk(sk)                           21
093700150703     c   21              eval      RCV_REMAIL = %subst(Dato_sk(sk):1:+
093800150703     c                                              LunDato_sk(sk))
093900150716     c   21              eval      UTIL(sk) ='S'
094000150715      * c'è l'indirizzo mail per eventuali avvisi se non ci sarà l'email x il predict
094100150715     c                   if        RCV_REMAIL <> *blank
094200150715     c                   eval      EMAIL_no_Predict = 'S'
094300150715     c                   eval      WRT_MAIL_SMS = 'S'
094400150715     c                   end
094500130904      **
094600150716     c                   exsr      ctrl_campi
094700150716      **
094800150828      **--------
094900150828      * AGGIUSTAMENTI  per contenuti NON IDONEI alla traduzione
095000150828      **--------
095100150828      ** CAP:
095200150828      ** se viene passato un numero inferiore di 5 cifre
095300150916     c**********         IF          RCV_RZIPCODE <> *blank
095400160225     c                   if        %len(%trim(RCV_RZIPCODE)) < 5   and
095500160225     c                             %len(%trim(RCV_RZIPCODE)) > 0
095600150828     c                   movel     '00000'       campo5            5            C.A.P.
095700150828     c                   eval      %subst(campo5
095800150828     c                             :5-%len(%trim(RCV_RZIPCODE))+1
095900150828     c                             :%len(%trim(RCV_RZIPCODE)) )
096000150828     c                                =  %subst(RCV_RZIPCODE
096100150828     c                             :1:%len(%trim(RCV_RZIPCODE)) )
096200150828     c                   eval       RCV_RZIPCODE = campo5
096300150828     c                   end
096400150916     c***********        endIF
096500150828      **
096600150828      **  se la Località inizia con il CAP lo tolgo e allineao a sinistra Città
096700150828     c                   if        %subst(RCV_RZIPCODE:1:5) =
096800150828     c                             %subst(RCV_RTOWN:1:5)
096900150828     c                   eval      %subst(RCV_RTOWN:1:5) = *blank
097000150828     c                   eval      RCV_RTOWN = %trim(RCV_RTOWN)
097100150828     c                   end
097200150828      **
097300150828     c                   if        %subst(RCV_RZIPCODE:1:5) =
097400150828     c                             %subst(RCV_RADD2:1:5)
097500150828     c                   eval      %subst(RCV_RADD2:1:5) = *blank
097600150828     c                   eval      RCV_RADD2 = %trim(RCV_RADD2)
097700150828     c                   end
097800150828      **
097900150828     c                   if        %subst(RCV_RZIPCODE:1:5) =
098000150828     c                             %subst(RCV_RADD3:1:5)
098100150828     c                   eval      %subst(RCV_RADD3:1:5) = *blank
098200150828     c                   eval      RCV_RADD3 = %trim(RCV_RADD3)
098300150828     c                   end
098400150828      **
098500150828     c                   if         RCV_RNAME1 = *BLANK  and
098600150828     c                              RCV_RNAME2 <>*BLANK
098700150828     c                   eval       RCV_RNAME1 = RCV_RNAME2
098800150828     c                   clear                   RCV_RNAME2
098900150828      **
099000150828     c                   elseif     RCV_RNAME1 = RCV_RNAME2
099100150828     c                   clear                   RCV_RNAME2
099200150828     c                   end
099300150828      **
099400150828     c                   if         RCV_RNAME1 = RCV_RCOMPNAME
099500150828     c                   clear                   RCV_RCOMPNAME
099600150828     c                   end
099700150828      **
099800150828     c                   if         RCV_RNAME1 = RCV_RSTREET
099900150828     c                   clear                   RCV_RSTREET
100000150828     c                   end
100100150828      **
100200150828     c                   if         RCV_RNAME1 = RCV_RTOWN
100300150828     c                   clear                   RCV_RTOWN
100400150828     c                   end
100500150828      **
100600150828     c                   if         RCV_RADD2  = RCV_RADD3
100700150828     c                   clear                   RCV_RADD3
100800150828     c                   end
100900150828      **
101000150828     c                   if         RCV_RTOWN  = RCV_RADD2
101100150828     c                   clear                   RCV_RADD2
101200150828     c                   end
101300150828      **
101400150828     c                   if         RCV_RTOWN  = RCV_RADD3
101500150828     c                   clear                   RCV_RADD3
101600150828     c                   end
101700150828      **
101800150828      ** se il contenuto  dei campi sono (-_./ o blank) pulisco la colonna
101900150828      **  oppure è il nome di una regione
102000150828      **  oppure c'è in 3 modi diversi ITALIA
102100150828    2c                   if        %check(solosimboli:RCV_RNAME1)=0
102200150828     c                               or %lookup(RCV_RNAME1:reg)>0
102300150828     c                               or RCV_RNAME1 = ITALIA
102400150828     c                               or RCV_RNAME1 = ITALY
102500150828     c                               or RCV_RNAME1 = IT
102600150828     c                   clear                   RCV_RNAME1
102700150828     c                   end
102800150828      **
102900150828    2c                   if        %check(solosimboli:RCV_RNAME2)=0
103000150828     c                               or %lookup(RCV_RNAME2:reg)>0
103100150828     c                               or RCV_RNAME2 = ITALIA
103200150828     c                               or RCV_RNAME2 = ITALY
103300150828     c                               or RCV_RNAME2 = IT
103400150828     c                   clear                   RCV_RNAME2
103500150828     c                   end
103600150828      **
103700150828    2c                   if        %check(solosimboli:RCV_RCOMPNAME)=0
103800150828     c                               or %lookup(RCV_RCOMPNAME:reg)>0
103900150828     c                               or RCV_RCOMPNAME = ITALIA
104000150828     c                               or RCV_RCOMPNAME = ITALY
104100150828     c                               or RCV_RCOMPNAME = IT
104200150828     c                   clear                   RCV_RCOMPNAME
104300150828     c                   end
104400150828      **
104500150828    2c                   if        %check(solosimboli:RCV_RSTREET)=0
104600150828     c                               or %lookup(RCV_RSTREET:reg)>0
104700150828     c                               or RCV_RSTREET = ITALIA
104800150828     c                               or RCV_RSTREET = ITALY
104900150828     c                               or RCV_RSTREET = IT
105000150828     c                   clear                   RCV_RSTREET
105100150828     c                   end
105200150828      **
105300150828    2c                   if        %check(solosimboli:RCV_RPROPNUM)=0
105400150828     c                               or %lookup(RCV_RPROPNUM:reg)>0
105500150828     c                               or RCV_RPROPNUM = ITALIA
105600150828     c                               or RCV_RPROPNUM = ITALY
105700150828     c                               or RCV_RPROPNUM = IT
105800150828     c                   clear                   RCV_RPROPNUM
105900150828     c                   end
106000150828      **
106100150828    2c                   if        %check(solosimboli:RCV_RADD2)=0
106200150828     c                               or %lookup(RCV_RADD2:reg)>0
106300150828     c                               or RCV_RADD2 = ITALIA
106400150828     c                               or RCV_RADD2 = ITALY
106500150828     c                               or RCV_RADD2 = IT
106600150828     c                   clear                   RCV_RADD2
106700150828     c                   end
106800150828      **
106900150828    2c                   if        %check(solosimboli:RCV_RADD3)=0
107000150828     c                               or %lookup(RCV_RADD3:reg)>0
107100150828     c                               or RCV_RADD3 = ITALIA
107200150828     c                               or RCV_RADD3 = ITALY
107300150828     c                               or RCV_RADD3 = IT
107400150828     c                   clear                   RCV_RADD3
107500150828     c                   end
107600150828      **
107700150828    2c                   if        %check(solosimboli:RCV_RTOWN)=0
107800150828     c                               or %lookup(RCV_RTOWN:reg)>0
107900150828     c                               or RCV_RTOWN = ITALIA
108000150828     c                               or RCV_RTOWN = ITALY
108100150828     c                               or RCV_RTOWN = IT
108200150828      **
108300150828     c                               or RCV_RTOWN = RCV_RZIPCODE
108400150828     c                   clear                   RCV_RTOWN
108500150828     c                   end
108600150828      **----
108700150908     c                   clear                   totparole         3 0
108800150908     c                   eval      totparole = %elem(Parole)
108900150908      *
109000150908      **  Controllo tab:"parole"
109100150908      *     Verifica se l'inizio del contenuto del campo corrisponde ad una delle
109200150908      *     parole elencate nella tabella in fondo al programma per individuare se
109300150908      *     si tratta dell'indirizzo.
109400150908      *
109500150908      ** Sulla RTOWN
109600150908     c                   do        totparole     P                 3 0
109700150908      *   se finito l'elenco ...esce
109800150908     c                   if        Parole(P) = *blank
109900150908     c                   leave
110000150908     c                   end
110100150908      *
110200150908      *  verifica l'inizio del campo
110300150908     c                   if        %subst(RCV_RTOWN:1:%len(%Trim(PAROLE(P)))+1)
110400150908     c                               =
110500150908     c                             %subst(PAROLE(P):1:%len(%Trim(PAROLE(P)))+1)
110600150908      *
110700150908     c                   if         RCV_RNAME2 = *blank
110800150908     c                   eval       RCV_RNAME2 = RCV_RSTREET
110900150908     c                   eval       RCV_RSTREET= RCV_RTOWN
111000150908     c                   clear                   RCV_RTOWN
111100150908     c                   else
111200150908     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
111300150908     c                             + ' ' + RCV_RTOWN
111400150908     c                   clear                   RCV_RTOWN
111500150908     c                   end
111600150908      *
111700150908     c                   end
111800150908      *
111900150908     c                   enddo
112000150908      **----
112100150908      ** Sulla RADD2
112200150908     c                   do        totparole     P
112300150908      *   se finito l'elenco ...esce
112400150908     c                   if        Parole(P) = *blank
112500150908     c                   leave
112600150908     c                   end
112700150908      *
112800150908      *  verifica l'inizio del campo
112900150908     c                   if        %subst(RCV_RADD2:1:%len(%Trim(PAROLE(P)))+1)
113000150908     c                               =
113100150908     c                             %subst(PAROLE(P):1:%len(%Trim(PAROLE(P)))+1)
113200150908      *
113300150908     c                   if         RCV_RNAME2 = *blank
113400150908     c                   eval       RCV_RNAME2 = RCV_RSTREET
113500150908     c                   eval       RCV_RSTREET= RCV_RADD2
113600150908     c                   clear                   RCV_RADD2
113700150908     c                   else
113800150908     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
113900150908     c                             + ' ' + RCV_RADD2
114000150908     c                   clear                   RCV_RADD2
114100150908     c                   end
114200150908      *
114300150908     c                   end
114400150908      *
114500150908     c                   enddo
114600150908      **----
114700150908      ** Sulla RADD3
114800150908     c                   do        totparole     P
114900150908      *   se finito l'elenco ...esce
115000150908     c                   if        Parole(P) = *blank
115100150908     c                   leave
115200150908     c                   end
115300150908      *
115400150908      *  verifica l'inizio del campo
115500150908     c                   if        %subst(RCV_RADD3:1:%len(%Trim(PAROLE(P)))+1)
115600150908     c                               =
115700150908     c                             %subst(PAROLE(P):1:%len(%Trim(PAROLE(P)))+1)
115800150908      *
115900150908     c                   if         RCV_RNAME2 = *blank
116000150908     c                   eval       RCV_RNAME2 = RCV_RSTREET
116100150908     c                   eval       RCV_RSTREET= RCV_RADD3
116200150908     c                   clear                   RCV_RADD3
116300150908     c                   else
116400150908     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
116500150908     c                             + ' ' + RCV_RADD3
116600150908     c                   clear                   RCV_RADD3
116700150908     c                   end
116800150908      *
116900150908     c                   end
117000150908      *
117100150908     c                   enddo
117200150828      **----
117300150828     c                   if        %subst(RCV_RADD2:1:3)='C/O'  or
117400150828     c                             %subst(RCV_RADD2:1:6)='PRESSO'
117500150828     c                   if         RCV_RNAME2 = *blank
117600150828     c                   eval       RCV_RNAME2 = RCV_RADD2
117700150828     c                   clear                   RCV_RADD2
117800150828     c                   else
117900150828     c                   eval      RCV_RNAME1  = %trim(RCV_RNAME1)
118000150828     c                             + ' ' + RCV_RADD2
118100150828     c                   clear                   RCV_RADD2
118200150828     c                   end
118300150828     c                   end
118400150828      **----
118500150828     c                   if        %len(%trim(RCV_RSTREET)) < 6 or
118600150828     c                              RCV_RSTREET = *BLANK
118700150828     c                   if         RCV_RADD2 <> *BLANK
118800150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
118900150828     c                              + ' ' +
119000150828     c                             %trim(RCV_RADD2)
119100150828     c                   clear                   RCV_RADD2
119200150828    2c                   endif
119300150828    2c                   endif
119400150828      **----
119500150828     c* Se la località è un numero la unisco all'indirizzo e la pulisco
119600150828    2c                   if        %check(digitsplus:RCV_RTOWN)=0
119700150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
119800150828     c                             + ' ' + RCV_RTOWN
119900150828     c                   clear                   RCV_RTOWN
120000150828      **
120100150828      * se la località inizia per numero, per non perderlo lo unisco all'indirizzo
120200150828   x2c                   else
120300150828      **
120400150828   2ac                   if        %subst(RCV_RSTREET:33:3)='   '
120500150828     c                   z-add     1             xx
120600150828    3c                   dow       %check(digitsplus:
120700150828    3c                             %subst(RCV_RTOWN:xx:1))=0
120800150828    4c                   if        xx=1
120900150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
121000150828     c                             + '-' + %subst(RCV_RTOWN:xx:1)
121100150828   x4c                   else
121200150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
121300150828     c                             + %subst(RCV_RTOWN:xx:1)
121400150828    4c                   endif
121500150828     c                   eval      %subst(RCV_RTOWN:xx:1) = *blank
121600150828     c
121700150828     c                   add       1             xx
121800150828    4c                   if        xx>35
121900150828     c                   leave
122000150828    4c                   endif
122100150828    3c                   enddo
122200150828      * alla fine compatta la località a sinistra
122300150828     c                   eval      RCV_RTOWN = %trim(RCV_RTOWN)
122400150828   2ac                   endif
122500150828     c
122600150828    2c                   endif
122700150828      **----
122800150828     c* Se la località è vuota e ADD2 o ADD3 sono pieni
122900150828    2c                   if        RCV_RTOWN = *blank   and
123000150828    2c                             RCV_RADD3 <>*blank
123100150828     c                   eval      RCV_RTOWN = RCV_RADD3
123200150828      *
123300150828    2c                   elseif    RCV_RTOWN = *blank   and
123400150828    2c                             RCV_RADD2 <>*blank
123500150828     c                   eval      RCV_RTOWN = RCV_RADD2
123600150828    2c                   endif
123700150828      **
123800150828      **----
123900150828      **  il numero civico se c'è lo accoda alla via
124000150828     c                   if        RCV_RPROPNUM <> *blank
124100150828     c                   eval      RCV_RSTREET = %trim(RCV_RSTREET)
124200150828     c                              + ' ' +
124300150828     c                             %trim(RCV_RPROPNUM)
124400150828     c                   end
124500150910      **----
124600150910      **  La COMPNAME potrebbe essere carica e servire
124700150910     c                   if        RCV_RCOMPNAME <> *blank
124800150910     c                   if        RCV_RNAME1 =     *blank
124900150910     c                   eval      RCV_RNAME1 = RCV_RCOMPNAME
125000150910     c                   elseif    RCV_RNAME2 =     *blank
125100150910     c                   eval      RCV_RNAME2 = RCV_RCOMPNAME
125200150910     c                   elseif    RCV_RSTREET =     *blank
125300150910     c                   eval      RCV_RSTREET = RCV_RCOMPNAME
125400150910     c                   elseif    RCV_RTOWN =       *blank
125500150910     c                   eval      RCV_RTOWN =   RCV_RCOMPNAME
125600150910     c                   end
125700150910     c                   end
125800150828      **----
125900150828      **
126000150831     c     end_rcv       Endsr
126100130904      *  ------------------------------------------------------------------ */
126200130904      *      PARCEL
126300130904      *  ------------------------------------------------------------------ */
126400130904     c     sr_PARCEL     Begsr
126500130904      **
126600130905     c                   clear                   fieu05RPRC
126700130904     c                   movea     H_PRC         Testa_sk
126800150702     c                   movea     SK_PRC_O      OBBL
126900150702     c                   movea     SK_PRC_N      NUME
127000150716     c                   clear                   UTIL
127100150722     c                   clear                   MSGER
127200130904      **
127300150703      *  #DEF;GEODATA:PARCEL;NUMORDER;PARCELNUMBER;PARCELNUMBERCCKEY;PARCELRANK;
127400150703      *  SENDPARCELREF1;SENDPARCELREF2;SENDPARCELREF3;SENDPARCELREF4;RECPARCELREF;
127500150703      *  SERVICECODE;PPARTNERREF1;PPARTNERREF2;PPARTNERREF3;PPARTNERREF4;DIMENSION;
127600150703      *  DECLAREDWEIGHT;MEASUREDWEIGHT;HINSAMOUNT;HINSCURRENCY;HINSCONTENT;HAZLQ;
127700150703      *  HZDPACKCODE;OPCODE;PCONTENT;ORIGINPARCELNUMBER;POWNERBU;PPARTNERCODE;ASCODE;;
127800150703      **
127900160210     c                   clear                   trova_campo
128000150703     c                   eval      trova_campo = 'PARCELNUMBER'
128100150703     c                   z-add     1             sk
128200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
128300150703     c   21              eval      PRC_PARCELNR = %subst(Dato_sk(sk):1:+
128400150703     c                                              LunDato_sk(sk))
128500150722      * se è arrivato un PARCEL minore di 14 caratteri è come se NON FOSSE ARRIVATO nulla
128600150722     c   21              if        LunDato_sk(sk) < 14
128700150722     c                   clear                   Dato_sk(sk)
128800150722     c                   end
128900150716     c   21              eval      UTIL(sk) ='S'
129000150703      **
129100160210     c                   clear                   trova_campo
129200150703     c                   eval      trova_campo = 'SENDPARCELREF1'
129300150703     c                   z-add     1             sk
129400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
129500150703     c   21              eval      PRC_SNDPRCREF1 = %subst(Dato_sk(sk):1:+
129600150703     c                                              LunDato_sk(sk))
129700150716     c   21              eval      UTIL(sk) ='S'
129800150703      **
129900160210     c                   clear                   trova_campo
130000150703     c                   eval      trova_campo = 'SERVICECODE'
130100150703     c                   z-add     1             sk
130200150703     c     trova_campo   lookup    Testa_sk(sk)                           21
130300150703     c   21              eval      PRC_SERVICECOD = atoi(%TRIM(Dato_sk(sk)))
130400150716     c   21              eval      UTIL(sk) ='S'
130500150703      **
130600160210     c                   clear                   trova_campo
130700150703     c                   eval      trova_campo = 'DECLAREDWEIGHT'
130800150703     c                   z-add     1             sk
130900150703     c     trova_campo   lookup    Testa_sk(sk)                           21
131000150703     c   21              eval      PRC_DECLWEIGHT = atoi(%TRIM(Dato_sk(sk)))
131100150722     c   21              if        PRC_DECLWEIGHT > 3150
131200150722     c                   eval      MSGER(sk) ='Peso Superiore 31,5kg'
131300150722     c                   end
131400150703      **
131500160210     c                   clear                   trova_campo
131600150703     c                   eval      trova_campo = 'MEASUREDWEIGHT'
131700150703     c                   z-add     1             sk
131800150703     c     trova_campo   lookup    Testa_sk(sk)                           21
131900150703     c   21              eval      PRC_MESWEIGHT = atoi(%TRIM(Dato_sk(sk)))
132000150716     c   21              eval      UTIL(sk) ='S'
132100150722     c   21              if        PRC_MESWEIGHT > 3150
132200150722     c                   eval      MSGER(sk) ='Peso Superiore 31,5kg'
132300150722     c                   end
132400150716      *
132500150716     c                   exsr      ctrl_campi
132600150716      *
132700150701      *-------------------------------------------------------------
132800150701      *  Controlla se la spedizione è già arrivata precedentemente
132900150701      *    sulle bolle in partenza o da confermare
133000150701     c                   clear                   GIA_PRESENTE      1
133100150921     c                   clear                   LNA_Sped_RESO     3 0
133200150701      **
133300150701      **  Imposta il Codice Parcel x verifica di precedente immissione
133400150701      **   PARCEL da controllare se già ricevuto e se NON è un Reso
133500150716     c                   eval         Parcel14  = PRC_PARCELNR
133600150701     c                   exsr      check_PARCEL
133700150701      **
133800150701      **  Qui fa scrivere la Spedizione sul VAB
133900150922      **                         =======================
134000150716     c                   if        gia_presente  = 'S'
134100150922      **                         =======================
134200160405      ***------***
134300160405     c                   if        vediPacco = *blanks
134400150701      *   Segnala che è già presente il Parcel quindi NON scrive la spedizione
134500150716     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(Parcel14) +
134600150701     c                                          ' già ricevuto precedentemente!'
134700160405     c                   elseIF    vediPacco = 'S'
134800160405      ***------>>
134900160405     c                   if        ERRvediPacco = *blanks
135000160405      *   Segnala aggiornamento di una spedizione VEDI PACCO
135100160405     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(Parcel14) +
135200160405     c                                          ' era un VEDI PACCO!'
135300160405     c                   elseIF    ERRvediPacco = 'S'
135400160405      *   Se ci sono stati ERRORI su aggiornamento VEDI PACCO
135500160405     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(Parcel14) +
135600160405     c                                     ' ERRORI aggiornando il VEDI PACCO!'
135700160405     c                   end
135800160405      ***------>>
135900160405     c                   end
136000160405      ***------***
136100160405     C                   eval      vinMSG = Msg_Vin_80
136200160405      **
136300150722     c                   elseif    PRC_PARCELNR = *blank or
136400150722     c                               %len(%trim(PRC_PARCELNR)) < 14
136500150722     C                   eval      Msg_Vin_80 = 'Parcel : NON RICEVUTO/-
136600150722     c                             MANCANTE/INCOMPLETO !!'
136700150722     C                   eval      vinMSG = Msg_Vin_80
136800150922      *
136900150722     c                   else
137000150922      *
137100150716      * Dopo il record con il PARCEL è possibile scrivere la spedizione
137200150716     C                   exsr      WRITE_PARCEL
137300150701     c                   end
137400150701      *
137500130904     c                   Endsr
137600130904      *  ------------------------------------------------------------------ */
137700130904      *      MESSAGES
137800130904      *  ------------------------------------------------------------------ */
137900130904     c     sr_MSG        Begsr
138000130904      **
138100130905     c                   clear                   fieu05RMSG
138200130904     c                   movea     H_MSG         Testa_sk
138300150716     c                   movea     SK_MSG_O      OBBL
138400150716     c                   movea     SK_MSG_N      NUME
138500150716     c                   clear                   UTIL
138600150722     c                   clear                   MSGER
138700130904      **
138800150703      *  #DEF;GEODATA:MSG;NUMORDER;NOTIFSENDERCOMP;NOTIFSENDERCONTACT;MSGTYPE;
138900150703      *  MSGDESTTYPE;MSGDESTINATION;MSGTRIGGER;MSGLANG;MSGSENDERURL;;
139000150703      **
139100160210     c                   clear                   trova_campo
139200150703     c                   eval      trova_campo = 'MSGTYPE'
139300150703     c                   z-add     1             sk
139400150703     c     trova_campo   lookup    Testa_sk(sk)                           21
139500150703     c   21              eval      MSG_MSGTYPE = atoi(%TRIM(Dato_sk(sk)))
139600150716     c   21              eval      UTIL(sk) ='S'
139700150703      **
139800160210     c                   clear                   trova_campo
139900150703     c                   eval      trova_campo = 'MSGDESTINATION'
140000150703     c                   z-add     1             sk
140100150703     c     trova_campo   lookup    Testa_sk(sk)                           21
140200150716     c   21              eval      UTIL(sk) ='S'
140300150703     c                   if        *in21
140400130904     c                   eval      MSG_MSGDESTIN = %subst(Dato_sk(sk):1:+
140500130904     c                                              LunDato_sk(sk))
140600150622      **  Qui fa scrivere eventuali  MSG sulla consegna
140700150715     c                   eval        WRT_MAIL_SMS = 'S'
140800150715      *-
140900150619      * carica il campo da riportare poi sul VAT
141000150619     c                   if        MSG_MSGTYPE = 1
141100150715     c                   eval      %subst(MSG_ServizioMail_Predict:1:35) =
141200150619     c                             %subst(MSG_MSGDESTIN:1:35)
141300150715     c                   eval      %subst(MSG_ServizioMail_Predict:36:35)=
141400150713     c                             %subst(MSG_MSGDESTIN:36:35)
141500150715     c                   eval      EMAIL_no_Predict = *blank
141600150715      *-
141700150619     c                   elseif    MSG_MSGTYPE = 3
141800150715     c                   eval      %subst(MSG_ServizioSMS_Predict:1:35) =
141900150619     c                             %subst(MSG_MSGDESTIN:1:35)
142000150619     c                   end
142100150703     c                   endif
142200150703      **
142300160210     c                   clear                   trova_campo
142400150703     c                   eval      trova_campo = 'MSGTRIGGER'
142500150703     c                   z-add     1             sk
142600150703     c     trova_campo   lookup    Testa_sk(sk)                           21
142700150716     c   21              eval      UTIL(sk) ='S'
142800150703     c                   if        *IN21
142900150703     c                   eval      MSG_MSGTRIGGER = atoi(%TRIM(Dato_sk(sk)))
143000150619      *
143100150619     c                   if        MSG_MSGTRIGGER = 904
143200150619     c                   if        MSG_MSGTYPE = 1
143300150715     c                   eval       MSG_MAIL_come_Predict ='S'
143400150619     c                   elseif    MSG_MSGTYPE = 3
143500150715     c                   eval       MSG_SMS_come_Predict ='S'
143600150619     c                   end
143700150703     c                   end
143800150619     c                   endif
143900130904      **
144000150716     c                   exsr      ctrl_campi
144100150701      *
144200130904     c                   Endsr
144300130904      *  ------------------------------------------------------------------ */
144400130904      *       Campi in errore
144500130904      *  ------------------------------------------------------------------ */
144600150702     c     ctrl_campi    Begsr
144700150702      **
144800150702     c                   do        60            sk
144900150702     c                   IF        OBBL(sk)= *blank
145000150702     c                   LEAVE
145100150702     c                   EndIF
145200150702      **
145300150716     c                   IF        UTIL(sk)<>'S'
145400150716     c                   ITER
145500150716     c                   EndIF
145600150722      **
145700150722      * Prima segnalazione
145800150722     c                   IF        MSGER(sk)<>*blank
145900150722     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
146000150722     c                                          + %trim(MSGER(sk))  + ';'
146100150722     c                   EndIF
146200150716      **
146300150702      ** o è OBBLIGATORIO o è Numerico ma NON contiene NUMERI
146400150702     c                   If        LunDato_sk(sk) =  0
146500150702     c                               and OBBL(sk) = Obbligatorio
146600150702     c                             or
146700150702     c                             DatoNum_sk(sk) = 'N'
146800150702     c                               and NUME(sk) = Numerico
146900150702      *  -----> Errore
147000150702     c                   exsr      Field_Error
147100150702     c                   End
147200150717      *
147300150702     c                   EnDDO
147400150717      *
147500150717      *  -----> Segnalazione solamente
147600150717     c                   if        multicollo > 1
147700150717      *
147800150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) +
147900150717     c                             '< Multicollo -->' + %editc(Multicollo:'Z') +
148000150717     c                             ' colli.'
148100150717     c                   clear                   multicollo        3 0
148200150717     C                   eval      se_errore   = 'S'
148300150717     c                   End
148400150702      *
148500150702     c                   Endsr
148600150702      *  ------------------------------------------------------------------ */
148700150702      *       Campi in errore
148800150702      *  ------------------------------------------------------------------ */
148900150702     c     Field_Error   Begsr
149000150716      *
149100130904     C                   eval      se_errore   = 'S'
149200150717      * Errore su VINMSG
149300150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
149400150717     c                                          + %trim(Testa_sk(sk))
149500130904      * msg video o in posta
149600130904     c                   If        Msg_Err = *blank
149700130904     c
149800130904     c                   Select
149900130904     c
150000130904     c                   When      LunDato_sk(sk) =  0
150100130904     c                               and OBBL(sk) = Obbligatorio
150200130904     c                   eval      Tipo_Error = 1
150300150717     C                   eval      Msg_Err =
150400150717     C                             ' >> Obbl.- Manca Dato !!'
150500130904     c
150600130904     c                   When      DatoNum_sk(sk) = 'N'
150700130904     c                               and NUME(sk) = Numerico
150800130904     c                   If        LunDato_sk(sk) =  0
150900130904     c                   eval      Tipo_Error = 2
151000150717     C                   eval      Msg_Err =
151100150717     C                             ' >> Obbl.Manca o NON Numero !!'
151200130904     c                   else
151300130904     c                   eval      Tipo_Error = 3
151400150717     C                   eval      Msg_Err =
151500150717     C                             ' >> NON Numero !!'
151600130904     c                   end
151700130904     c                   When      Tipo_Error = 4
151800150717     C                   eval      Msg_Err =
151900150717     C                             ' >> Conver.non riuscita !!'
152000130904     c                   EndSL
152100150717      *
152200150717     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) + Msg_err
152300150717     c*
152400130904     c                   End
152500130904      **
152600130904     c                   Endsr
152700130904      *  ------------------------------------------------------------------ */
152800130904      *       Suddivide i campi della riga con carattere divisorio (;)
152900130904      *  ------------------------------------------------------------------ */
153000130904     c     Split_Rec     Begsr
153100130904      **
153200130904     c                   clear                   Nr_campi
153300130904     c                   clear                   Campi_Record
153400130904     c                   clear                   Campi_Lunghi
153500130904     c                   clear                   Campi_Numerici
153600130904     c                   clear                   Campi_Decimali
153700130904     c                   movel     ';'           Separa_campi
153800130904     c                   movel     '.'           Decimal_Separ
153900130904      *
154000160210      * sostituito lo standard previsto con campi max 40 bytes l'uno
154100160210      * con un pgm identico che prevede campi da 256 bytes
154200160210     c******             call      'FIEU00R'
154300160210     c                   call      'FIEU05R0'
154400130904      *       Input
154500130904     c                   parm                    Separa_campi
154600130904     c                   parm                    Decimal_Separ
154700130904     c                   parm                    dati
154800130904      *       Output  su schiere di 100 elementi
154900130904     c                   parm                    Nr_campi
155000130904     c                   parm                    Campi_Record                   * i dati
155100130904     c                   parm                    Campi_Lunghi                   * lunghezza dati i
155200130904     c                   parm                    Campi_Numerici                 * se numerici
155300130904     c                   parm                    Campi_Decimali                 * quanti decimali i
155400130904      **
155500130904     c                   Endsr
155600160229      * ?-----------------------------------------------------------------  */
155700160229      *    Rileva la prima volta che arriva il GEODATA
155800160229      * ?-----------------------------------------------------------------  */
155900160229     C     check_WFIEU00 BEGSR
156000160229     C*
156100160229     c     Parcel14      chain     wfieu00f
156200160229     c                   if        %Found(wfieu00f)
156300160229     c                   if        F00DATGEO = 0
156400160229     c                   eval      F00DATGEO = dateu
156500160229     c                   eval      F00ORAGEO = wora
156600160229     c                   update    fieu000
156700160229     c                   end
156800160229     c                   else
156900160229     c                   clear                   fieu000
157000160229     c                   eval      F00PARCEL = parcel14
157100160229     c                   eval      F00DATGEO = dateu
157200160229     c                   eval      F00ORAGEO = wora
157300160229     c                   write     fieu000
157400160229     c                   end
157500160229     C*
157600160229     C                   ENDSR
157700130904      *  ================================================================== */
157800130904      *     CONTROLLA SE il parcel è arrivato in una precedente trasmissione
157900130904      *  ================================================================== */
158000130904     c     check_PARCEL  Begsr
158100160229      *
158200160229      * registra il 1° arrivo dello SCAN 05
158300160229     c                   exsr      check_WFIEU00
158400150629      **
158500150908      **  se già confermato in Partenza  NON PUO' RICARICARE la spedizione
158600170925      *--
158700151022     c                   clear                   cancellato        1
158800151022     c                   clear                   gia_in_BLP        1
158900160405     c                   clear                   vediPacco         1
159000160405     c                   clear                   errvediPacco      1
159100151023     c                   clear                   sono_gomme        1
159200160211     c                   clear                   salva_CMR        35
159300160218     c                   clear                   salva_SCL         1
159400170925      ******
159500170925      ** Sostituisce il FIAR4 tipo record "I"
159600170925     C     Parcel14      chain     fipnd03l
159700170925     c                   IF        %Found(fipnd03l)
159800170925      **
159900150921     c                   movel     Parcel14      DepotParcel
160000150921      **
160100150921      ** si tratta potenzialmente di un reso
160200150921     c                   if        DepotParcel = '0844'
160300150921     c                              or
160400150921     c                             DepotParcel = '0845'
160500150921      **
160600150921      * cerca la LNA della bolla originale
160700170925     c     kPNDbolla     chain     fnarb01l
160800150921     c                   if        %Found(fnarb01l)
160900150921     c                   z-add     arbLNA        LNA_Sped_RESO
161000150921     c                   end
161100150921      **
161200150921     c                   else
161300150921      **
161400150629      *   controlla che sia DPD con il Network e non con la linea
161500170925     c     PNDLNP        chain     azorg01l
161600150629     c                   if        %Found(azorg01l)
161700150629     c                   eval      OG143 = orgDE3
161800160405      **
161900170925     c                   if         §OGNTW = 'DPD'
162000170922      ** Non serve più agganciare il FISICO del FIAR4 perchè il dato è stato spostato
162100170922      ** sul FIPND00F
162200170922      **
162300170922      ** il flag lo deve impostare dal nuovo file
162400170922     c                   movel     PNDflo        dPNDflo
162500170922     c                   movel     §PNDTRD       wflag             1
162600160404      **
162700160405      ** Solo per records VEDI PACCO  trd=1
162800170925     c     kPNDbolla     chain     fnblp01l
162900160404     c                   if        %Found(fnblp01l) and blpFT1 = *blank
163000170922     c                             and wflag  = '1'
163100170922      **
163200160405     c                   eval        vediPacco = 'S'
163300160405      **
163400160404      ** se NON ancora partita e essendo VEDI PACCO,
163500160404      **  aggiorna i dati in partenza variando la bolla.
163600160404     c                   movel     kpjbu         svkpjbu
163700160404     c                   clear                   kpjbu
163800160404     C                   clear                   FNLSI7DS
163900160404     C                   eval      ILSI7AAS =  blpAAS
164000160404     C                   eval      ILSI7LNP =  blpLNP
164100160404     C                   eval      ILSI7NRS =  blpNRS
164200160404     C                   eval      ILSI7NSP =  blpNSP
164300160404      **
164400160404     C                   eval      ILSI7RSD =  RCV_RNAME1
164500160404     C                   eval      ILSI7RD2 =  RCV_RNAME2
164600160404     C                   eval      ILSI7IND =  RCV_RSTREET
164700160404     C                   eval      ILSI7CAD =  RCV_RZIPCODE
164800160404     C                   eval      ILSI7LOD =  RCV_RTOWN
164900160404     C                   movel     FNLSI7DS      KPJBU
165000160404     C                   call      'FNLSI7R'
165100160404     C                   parm                    KPJBA
165200160405     C                   parm                    FNLSI7DS
165300160404     C                   if        OLSI7ERR <>*blank
165400160405     c                   eval        ERRvediPacco ='S'
165500160404     C                   endif
165600160404     c                   movel     svkpjbu       kpjbu
165700160404      **
165800160404     c                   else
165900160404     c                   end
166000160404      **
166100150908     c                   eval        GIA_PRESENTE = 'S'
166200151022     c                   eval          gia_in_BLP = 'S'
166300160404      **
166400150908     c                   LEAVESR
166500150629     c                   end
166600160405     c                   end
166700150629     c                   end
166800150921     c                   end
166900150629      ** --------
167000130904      **  se non ancora confermato da Cliente
167100150908      ** --------
167200130904     C                   eval        vatTRC = 'E'
167300150716     C                   eval        vatNOT = Parcel14
167400130904     C     Kvat4E        setll     fivat02l
167500130904     C     vatTRC        reade     fivat02l
167600150717     c                   IF        not %EoF(fivat02l)
167700150717     c                   movel     vatNOT        dVATe
167800150908     C                   if        Parcel14= §vatEPRN
167900150716     c     vatLNP        chain     azorg01l
168000150629     c                   if        %Found(azorg01l)
168100150629     c                   eval      OG143 = orgDE3
168200150717      *
168300150717     c                   if        §OGNTW = 'DPD'
168400150908     c                   eval        vabCCM = vatCCM
168500150717     c                   move      Parcel14      vabRMN
168600150717     c     kvab01        chain     fivab01l
168700150717     c                   if        %Found(fivab01l)
168800150717      * Elimina il VAT
168900150717     C     kvat01        setll     fivat01l
169000150717     C     kvat01        reade     fivat01l
169100150717     c                   dow       not %EoF(fivat01l)
169200150717     c                   delete    fivat000
169300150717     C     kvat01        reade     fivat01l
169400150717     c                   enddo
169500151022     c                   move      'S'           cancellato
169600150717     c                   delete    fivab000
169700150717     c                   end
169800150717     c                   end
169900150629      *
170000150629     c                   end
170100150716     c                   end
170200150717     c                   end
170300150908      **
170400150908      **  se non ancora confermato CMR
170500150908     C                   eval        vatTRC = 'E'
170600150908     C                   eval        vatNOT = Parcel14
170700150908     C     Kvat4E        setll     edivat4l
170800150908     C     vatTRC        reade     edivat4l
170900150908     c                   IF        not %EoF(edivat4l)
171000150908     c                   movel     vatNOT        dVATe
171100150908     C                   if        Parcel14= §vatEPRN
171200150908     c     vatLNP        chain     azorg01l
171300150908     c                   if        %Found(azorg01l)
171400150908     c                   eval      OG143 = orgDE3
171500150908      *
171600150908     c                   if        §OGNTW = 'DPD'
171700150908     c                   eval        vabFGS = vatFGS
171800150908     c                   eval        vabCCM = vatCCM
171900150908     c                   move      Parcel14      vabRMN
172000150908     c     kvab03        chain     edivab3l
172100150908     c                   if        %Found(edivab3l)
172200150908      * Elimina il VAT
172300150908     C     kevat01       setll     edivat1l
172400150908     C     kevat01       reade     edivat1l
172500150908     c                   dow       not %EoF(edivat1l)
172600150908     c                   delete    edivat00
172700150908     C     kevat01       reade     edivat1l
172800150908     c                   enddo
172900150908      *
173000160211      * Se arrivava da un IGATEAVIS salva CMR x reimpostarlo sulla nuova bolla
173100160218     c                   if        VABcmr <> GEO_IN_SOSPESO
173200160211     c                   eval        salva_CMR  = VABcmr
173300160218     c                   eval        salva_SCL  = VABscl
173400160211     c                   end
173500160211      *
173600160302      *  deve cancellare anche dalla schiera degli eventuali multicollo
173700160302     c                   z-add     1             dlt               3 0
173800160302     c     vabnsp        lookup    nr_sped(dlt)                           21
173900160302     c   21              eval      nr_sped(dlt) = *all'9'
174000151022     c                   move      'S'           cancellato
174100150908     c                   delete    edivab00
174200150908     c                   end
174300150908     c                   end
174400150908      *
174500150908     c                   end
174600150908     c                   end
174700150908     c                   end
174800150622      **
174900151023      **  Controlla se si tratta di gomme per poterle evidenziare nel CMR
175000151023     c                   if        PRC_SERVICECOD = Gomme_365 or
175100151023     c                             PRC_SERVICECOD = Gomme_366 or
175200151023     c                             PRC_SERVICECOD = Gomme_367
175300151023      **   SI--> sono gomme
175400151023     c                   eval      sono_gomme ='S'
175500151023      **
175600151023     c                   end
175700151023      **
175800130904     c                   endSR
175900130904      *  ------------------------------------------------------------------ */
176000130904      *     CONTROLLA il parcel e se OK lo scrive
176100130904      *  ------------------------------------------------------------------ */
176200130904     c     write_PARCEL  Begsr
176300130904      *
176400130904      *  con il record in canna scrive direttamente il VAB ed il VAT
176500130904      *                /*---------------------- */
176600130904     c                   exsr      WRT_VAB_VAT
176700130904      *                /*---------------------- */
176800130904
176900130904      *  Problemi nella decodifica dei campi VAB/VAT
177000130904     c                   if        se_errore ='S'
177100130904     C                   eval      vinMSG = Msg_Vin_80
177200130904     c                   end
177300150716      *
177400130904      **** Meglio evidenziare i SERVICE CODE NON previsti
177500130904     c                   if        PRC_SERVICECOD <> Normal_Parcel and
177600130904     c                             PRC_SERVICECOD <>  Small_Parcel and
177700130904     c                             PRC_SERVICECOD <> Return_Sender and
177800141211     c                             PRC_SERVICECOD <> Return_Parcel and
177900141215     c                             PRC_SERVICECOD <> Gomme_365     and
178000141215     c                             PRC_SERVICECOD <> Gomme_366     and
178100150716     c                             PRC_SERVICECOD <> Gomme_367     and
178200150716     c                             PRC_SERVICECOD <> Normal_Parcel_327 and
178300150716     c                             PRC_SERVICECOD <> Normal_Parcel_329 and
178400150716     c                             PRC_SERVICECOD <> Small_Parcel_328
178500130904     C                   eval      Msg_Vin_80 = '/ Service Code :' +
178600130904     c                              %editc(PRC_SERVICECOD:'X') +
178700130904     c                              ' non previsto!'
178800130904     C                   eval      vinMSG = %trim(vinMSG) +  Msg_Vin_80
178900130904     c                   end
179000130904      **
179100130904     c                   endSR
179200130904      *  ================================================================== */
179300150710     C*  ESEGUO SCRITTURA fiVAT Riferimenti x il destinatario
179400130904      *  ================================================================== */
179500130904     C     WRT_VAB_VAT   BEGSR
179600130904      *
179700150710      * Clearizzo        in quanto ha gli stessi campi di FiVAB più
179800130904     c                   clear                   err_bloccante     1
179900130904     c                   clear                   Sped_RESO         1
180000150922      * Default
180100150925      ** aggancia i dati della tab.LPD  di default
180200151012     c                   z-add     190           Linea_Partenza    3 0
180300150925     c                   exsr      tabella_LPD
180400150925      **
180500150922      * con il Depot del Parcel ricava LINEA Cliente etc.etc.
180600171010     c                   exsr      LNP_DA_DOVE
180700171010      *
180800150716      *  Codice bolla
180900150716     C                   MOVEL     '1'           vabcbo                         NO C/assegno
181000150716      *
181100150716      * -----------------                  -------------------
181200150716      *  Mittente Originale
181300150917     C                   CLEAR                   TISI95DS
181400150716     C                   eval       VABRMO = SND_SNAME1
181500150716     C                   eval       VABCMO = SND_SZIPCODE
181600150716     c                   z-add     1             sk
181700150716     c     SND_SCOUNTRYCDlookup    CIE(sk)                                21
181800150716     C   21              eval       VABNMO = c15(sk)
181900150722     c                   if        vabnmo = *blank or
182000150722     c                             vabcmo = *blank
182100150722     c                   clear                   vabcmo
182200150722     c                   clear                   vabnmo
182300150916     c                   else
182400150916      ** nazione NON Italia
182500150916     C                   MOVEL     vabNMO        I95NAR
182600150916     C                   MOVEL     vabCMO        I95CAP
182700150916     C                   MOVEL     DATEU         I95DAT
182800150916     C                   MOVEL     '3'           I95TCN
182900150916     C                   CALL      'TISI95R'
183000150916     C                   PARM                    TISI95DS
183100150917     c                   end
183200150917      *
183300150916      *     Errore
183400150916     C     O95ERR        IFne      *BLANKS
183500150917     c     vabrmo        orEQ      *BLANKS
183600150916     c                   clear                   vabrmo
183700150916     c                   clear                   vabcmo
183800150916     c                   clear                   vabnmo
183900150916     C                   END
184000150916      *
184100150716      * -----------------                  -------------------
184200150716      *  Data  spedizione
184300160208     C                   MOVE      DATEU         VABMGS                         = oggi
184400150716      *
184500150827      * -----------------                  -------------------
184600150716      *  Dati  destinatario
184700150716     c                   movel     RCV_RNAME1    vabrsd                         Ragione Sociale
184800150716     C                   movel     RCV_RNAME2    vabrd2                         rag.soc.2
184900150716     c                   movel     RCV_RSTREET   vabind                         Indirizzo
185000150716     c                   movel     RCV_RTOWN     vablod                         Località
185100160216      **
185200160216      *  lasciamo nel CAP solo dei numeri
185300160216     c                   do        9             §n                3 0
185400160216      **
185500160216     c                   if        %check(digits:%subst(RCV_RZIPCODE:§n:1))<>0
185600160216     c                   eval      %subst(RCV_RZIPCODE:§n:1) = ' '
185700160216     c                   end
185800160216      **
185900160216     c                   end
186000160216      **
186100150828     c                   movel     RCV_RZIPCODE  vabcad                         C.A.P.
186200150925      **-----------------                  -------------------
186300150716      *  Se manca la provincia la calcolo
186400150716     c                   if        vabprd = *blank
186500150716     C                   CLEAR                   TISI95DS
186600150716     C                   MOVEL     vabCAD        I95CAP
186700150716     C                   MOVEL     DATEU         I95DAT
186800150716     C                   MOVEL     '3'           I95TCN
186900150716     C                   CALL      'TISI95R'
187000150716     C                   PARM                    TISI95DS
187100150716      *     Errore
187200150716     C     O95ERR        IFeq      *BLANKS
187300150716     C                   MOVEL     O95PRV        vabPRD
187400150716     C                   END
187500150716      *
187600150716     c                   end
187700150716      *  Tipo servizio
187800150716     c                   movel     'C'           vabtsp
187900150716      ** RMA
188000150722     C**********         eval       vabrma = %Trim(SHP_MPSCREF1)
188100130904      ** ------------------------------------------------------------------ */
188200130904      *     ( Il parcel non è + numerico )
188300130904     c
188400130904      *  il Parcel deve essere impostato nella DS VATE
188500150716      *  se comunque il Parcel ricevuto è numerico impostiamo nel RmittNUMERICO
188600150716      *  il nr.Parcel come avveniva prima che diventasse alfa il campo
188700130904      **
188800130904     c     digits        check     PRC_PARCELNR  position
188900130904      **
189000130904      **  presenti campi non numerici: (Ossia trovati)
189100150716      **   ma sono oltre il codice PARCEL sulla destra ... allora è OK
189200150716     C                   z-add     1             vabrmn
189300150716      **    in tal caso sono solo i blanks alla destra del numero
189400150716     c                   iF        %Found and position > 14
189500150716     c                             or not %Found
189600130904     c                   move      parcel14      vabrmn
189700130904     c                   end
189800160121      *
189900160121      **-----------------                  -------------------
190000160121      * ?Scrivo EDiVAB
190100160121     C                   If        su_EDIVAB = 'S'
190200160121      *
190300160121      *   Controlla se ricevuto già lo SCAN 05 di collo partito
190400160121      *    con il PARCEL di 14
190500160121     c                   clear                   SCAN05_OK         1
190600160121     c                   exsr      CHECK_SCAN05
190700160121      *
190800160121     c                   end
190900160121      *
191000150716      **-----------------                  -------------------
191100150717      *  Preleva il Nr.Spedizione
191200150717     C                   EXSR      SUB_keybol
191300150717      *
191400150716      *  Colli  - può esserci un multicollo   QUINDI impostiamo a (1)
191500150701     c                   z-add     1             vabncl
191600150716      *
191700150717      **-----------------                  -------------------
191800150716      * Il volume viene impostato solo se la spedizione è MONOCOLLO.
191900150716      * poiké è passato il valore intero ma è in cm cubi
192000150716      *    ossia il valore di 3000000= 3mc. divido x 1000000
192100150716     c                   if        SHP_MPSVOLUME <> 0  and
192200150716     c                             SHP_MPSCOUNT  = 1
192300160927     c**** SHP_MPSVOLUME div       1000000       vabVLB
192400150716     c                   end
192500150716      **-----------------                  -------------------
192600150716      *   Prioritariamente imposto il Peso
192700150716      *     misurato sul PARCEL da DPD
192800150716     c                   if        PRC_MESWEIGHT <> 0
192900150716     c     PRC_MESWEIGHT div       100           vabPKB
193000150722     c                   if        vabpkb =0
193100150722     c                   eval      vabpkb =0,1
193200150722     c                   end
193300150717     c                   sub       vabPKB        Kg_rimasti
193400150716      *
193500150716      * se non c'è
193600150716      *    metto quello dichiarato dal cliente
193700150716     c                   elseIF    PRC_DECLWEIGHT<> 0
193800150716     c     PRC_DECLWEIGHTdiv       100           vabPKB
193900150722     c                   if        vabpkb =0
194000150722     c                   eval      vabpkb =0,1
194100150722     c                   end
194200150717     c                   sub       vabPKB        Kg_rimasti
194300150716      *
194400150716      * se non c'è imposto quello della spedizione
194500150716      *                 il valore intero ma ha 2 decimali
194600150716      *           ossia il valore di 300 = 3kg. divido x 100
194700150717     c                   elseIF     SHP_MPSWEIGHT <> 0 and SHP_MPSCOUNT =1
194800150716     c     SHP_MPSWEIGHT div       100           vabPKB
194900150722     c                   if        vabpkb =0
195000150722     c                   eval      vabpkb =0,1
195100150722     c                   end
195200150916     c                   endIF
195300150716      *
195400150716      *  e se non c'è nemmeno questo vado alla vecchia con il SERVICE CODE
195500150916     c                   if        vabPKB  = 0 or vabPKB > 31,5
195600150717     c                   z-add     1             kg0_SPED(nr)
195700150717      *
195800130904      *  Service  Code:  101-Normal Parcel
195900130904      *                  136-Small Parcel
196000130904      *                  298-Return Sender
196100130904      *                  300-Return Parcel
196200130904      *                  327-Normal Parcel B2C
196300130904      *                  329-Normal Parcel B2C
196400130904      *                  328-Small  Parcel B2C
196500141211      *                  365-Gomme BtoB
196600141211      *                  366-Gomme BtoC
196700141211      *                  367-Gomme BtoC con COD
196800130904     c                   clear                   w015A
196900130904     c                   if        PRC_SERVICECOD = Normal_Parcel
197000130904     c                               or
197100130904     c                             PRC_SERVICECOD = Normal_Parcel_327
197200130904     c                               or
197300130904     c                             PRC_SERVICECOD = Normal_Parcel_329
197400130904     c                   eval      W015A = 'PESO01'
197500150716     c                   elseIF    PRC_SERVICECOD = Small_Parcel
197600130904     c                               or
197700130904     c                             PRC_SERVICECOD = Small_Parcel_328
197800130904     c                   eval      W015A = 'PESO02'
197900150716     c                   elseIF    PRC_SERVICECOD = Gomme_365
198000141211     c                               or
198100141215     c                             PRC_SERVICECOD = Gomme_366
198200141211     c                               or
198300141215     c                             PRC_SERVICECOD = Gomme_367
198400141211     c                   eval      W015A = 'PESO03'
198500141211     c                   end
198600141211      *
198700130904      *  Peso, ricavato dal tipo servizio del ROUTELABEL
198800130904     c                   z-add     1             xx
198900130904     c                   if        w015a <> *blank
199000130904     c     W015A         lookup    DPPK(xx)                               31
199100130904     c                   If        *in31 = *off
199200130904     c                   eval      W015A = 'PESO'
199300130904     c     W015A         lookup    DPPK(xx)                               31
199400130904     c                   Endif
199500150916     c                   If        *in31
199600130904     c                   z-add     DPP(xx)       vabPKB
199700161006     c                   z-add     VLB(xx)       vabVLB
199800150916     c                   End
199900130904     c                   End
200000141104      *
200100150716     c                   EndIF
200200150916      *
200300150916      * Se non ci dovesse essere il peso in assoluto imposta 11kg.
200400150916     c                   if        vabPKB > 31,5
200500150916     c                   eval           vabPKB = 30
200600150916     c                   EndIF
200700150722      *
200800130904      *  Se spedizione RESA imposto diversamente il VAB
200900130904      *     ma se DPD non invia correttamente il service Code
201000130904      *     identifichiamo il reso dal Depot Bartolini sul Parcel
201100130904      **
201200150716     c                   movel     PRC_PARCELNR  DepotParcel
201300150722      **
201400130904     c                   if        PRC_SERVICECOD  = Return_Sender
201500130904     c                               or
201600130904     c                             PRC_SERVICECOD  = Return_Parcel
201700130904     c                               or
201800150716     c                             (DepotParcel= '0844' or DepotParcel= '0845')
201900130904     C                   clear                   VABpkb
202000130904     C                   clear                   VABncl
202100150925      *
202200150925     c                   if        LNA_Sped_RESO =  0
202300150925     c                   eval      LNA_Sped_RESO =  190
202400150925     c                   end
202500151012      *
202600150923     c                   if        LNA_Sped_RESO <> 0
202700151012      *
202800150925     c                   z-add     LNA_Sped_RESO Linea_Partenza
202900151012     c                   eval      Sped_RESO = 'S'
203000151012      *
203100151012      ** aggancia i dati della tab.LPD x il RESO
203200150925     c                   exsr      tabella_LPD
203300151012      *
203400150923     c                   end
203500150925      *
203600150721     c                   movel     vabrsd        campo35          35
203700150721     C                   eval      VABrsd = '**RESO**' + campo35
203800150925      ***
203900150925     C                   MOVEL     '2'           vabcbo
204000130904     c                   ENDIF
204100150925      **
204200150925      * -----------------                  -------------------
204300150925      ** imposta il Depot di provenienza x testare se VTG
204400150925     c                   movel     parcel14      DepotParcel
204500150925     C                   movel(p)  'VTG'         cldCOD
204600150925     C                   move      DepotParcel   cldDEP
204700150925     c     key_VTG       setll     tncld00F
204800150925     c     key_VTG       reade     tncld00F
204900150925     c                   dow       not %EoF(tncld00F)
205000150925     c                   if        dateu >= cldDDA and
205100150925     c                             dateu <= cldADA
205200150925      **  Se è un collo VTG deve avere una tariffa differente
205300151012     c                   movel     tariffa_VTG   vabCTR
205400150925     c                   leave
205500150925     c                   end
205600150925     c     key_VTG       reade     tncld00F
205700150925     c                   endDO
205800150925      * -----------------                  -------------------
205900130904      *  FAX destinatario da contattare
206000130904     c                   if        RCV_RFAX   <> *blank
206100130904     c                   ENDIF
206200130904      *
206300130904      *  Controlla esistenza del Parcel sul file degli ORM dati a DPD per
206400130904      *   impostare la bolla in assegnato
206500130904      *  Controlla sul FIDPO x metterlo in assegnato
206600150716     C     parcel14      chain     fidpo06l
206700150716      *assegnato
206800130904     c                   if        %Found(fidpo06l)
206900130904     C                   MOVEL     '2'           vabcbo
207000130904      *
207100130904      *  SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
207200130904      *  in FERMO DEPOSITO.
207300130904     c                   eval      vabFFD = *blank
207400150716     C                   if          dpoPOE > 0
207500130904     C                   z-add     dpoPOE        ORMPOE
207600130904     C                   z-add     dpoNRS        ORMNSR
207700130904     C                   z-add     dpoNOR        ORMNOR
207800130904     C                   z-add     dpoNRV        ORMNRV
207900130904      *  lo vedo sull'ORM
208000130904     C     KORM          chain     fnORM01l
208100130904     c                   if        %Found(FnOrm01l) and ormffd ='S'
208200130904     c                   eval      vabFFD = 'S'
208300130904     c                   end
208400130904     c                   end
208500150716     c                   end
208600150831      ***
208700160217      *** NON SI VUOLE CREARE IL RESO (non modifico le vecchie spec.NON si sa mai)
208800160217      ***   SALTIAMO semplicemente la scrittura
208900160217      ***
209000160217     c                   If        Sped_Reso <>'S'
209100160217      ***                          ==========
209200160217      ***
209300150908      ***
209400150916      *   non facciamo scrivere la spedizione se:
209500150916      *         1) destinatario ed indirizzo = ' '
209600150916      *         2) località e cap = ' '
209700150916      ***
209800150916     c                   if        vabrsd =*blank and vabind = *blank
209900150916     c                                or
210000150916     c                             vablod =*blank and vabcad = *blank
210100151022      *  segnala mancanza dati fondamentali
210200151022     c                   eval        dati_CONSEGNA ='N'
210300150922      *  altrimenti scrive la spedizione
210400150916     c                   else
210500161215      *
210600161215      *** Provenienza  x distinguere i records dall'IGATEAVIS
210700161215     c                   movel     'G'           VABscl
210800150921      *
210900150921      * ?Scrivo EDiVAB o FiVAB
211000150921     C                   If        su_EDIVAB = 'S'
211100150923      *
211200150923      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
211300150923    2c                   if        SHP_CDATE<>0
211400151103     c                   TEST(D)                 shp_CDATE              99
211500151103     c   99*iso          movel     DATEU         DATA_iso
211600151103     c  n99*iso          movel     SHP_CDATE     DATA_iso
211700150923     c                   else
211800150923     c     *iso          movel     DATEU         DATA_iso
211900150923     c                   end
212000150929      *
212100150929      *  se Non c'è incremento
212200150929     c                   if        gg_incr = 0
212300150929     c                   exsr      test_gg_lav
212400150929      *
212500150929     c                   else
212600150928      *
212700150928      *  aggiunge x giorni (ma solo lavorativi)
212800150928     c                   do        gg_incr       giorni            3 0
212900150928     c                   adddur    1:*D          data_iso
213000150929     c                   exsr      test_gg_lav
213100150928     c                   enddo
213200150929     c                   endif
213300150928      *
213400150923     c                   move      data_iso      data_iso8         8 0
213500150928      *   Se la data risultante è inferiore alla UDATE forza UDATE
213600150928     c                   if        data_iso8 < dateu
213700160121     c                   move      dateu         data_iso8
213800150928     c                   end
213900150923      *
214000150923     c                   eval      vabcmr = 'GEO_' + %editc(data_iso8:'Y')
214100151023     c                   If        sono_gomme ='S'
214200151023     c                   eval      vabcmr = %trim(vabcmr) + '_GOMME'
214300151023     c                   end
214400150921      *
214500150921      * se trattasi di Reso (Evidenzio)
214600150923     c                   If        Sped_Reso = 'S'
214700150923     c                   clear                   campo35          35
214800150923     c                   eval      campo35 = 'RESI_' + vabCMR
214900150923     c                   eval      VABcmr  = campo35
215000150921     c                   end
215100150921      *
215200160121      *  Se non era arrivato già lo SCAN 05 come COLLO PARTITO fisicamente
215300160121      *   allora va mantenuto in sospensione in attesa dello SCAN 05.
215400160121     c                   if              SCAN05_OK <>'S'
215500160121     c                   eval      VABcmr  = GEO_in_sospeso
215600160121     c                   end
215700160211      *
215800160211      *
215900160211      * Significa che prima era arrivato sull'IGATEAVIS la spedizione
216000160211     c                   if        salva_CMR <> *blank
216100160211      * reimposta il CMR dell'IGATEAVIS
216200160211     c                   eval      vabCMR = salva_CMR
216300160218     c                   eval      vabSCL = salva_SCL
216400160211     c                   end
216500160121      *
216600150921     c                   z-add     DATEU         VABdcm
216700150921     c                   z-add     DATEU         VABdts
216800150921     c                   movel     WORA          VABhms
216900150921      ***
217000150921     c                   write     EDivab00
217100151103      *** al momento sospeso --> vedremo se ci servirà....
217200151103     c******             exsr      wri_wfieu05
217300150921     c                   ELSE
217400130904     c                   write     FiVAB000
217500150921      ***
217600150921     c                   ENDIF
217700150921      *
217800130904     c                   exsr      wrtvat
217900150916      *
218000150916     c                   end
218100130904      ***
218200160217     c                   endIF
218300160217      ***
218400160217      ***  saltato se SPED. di RESO  (stessa cosa per il MSG)
218500160217      ***
218600130904     c                   move      'S'           Almeno_Uno
218700130904      *
218800130904     c     Error_VAB     Endsr
218900130904     C*----------------------------------------------------------------
219000160121      *  Controlla se arrivato SCAN 05 su TIDP200F
219100160121     C*----------------------------------------------------------------
219200160121     C     CHECK_SCAN05  BEGSR
219300160121      *
219400160121      * Se lo ha trovato deve prendere il SERVICE CODE il PESO e la DATA
219500160121     c     parcel14      chain     TIDP200F
219600160121     c                   If        %Found(TIDP200F)
219700160121     c                   eval         SCAN05_OK ='S'
219800160121     c                   move      I2WEIGHT      PRC_MESWEIGHT
219900160121     c                   move      I2SERVICE     PRC_SERVICECOD
220000160121      ** calcola la data con i giorni di incremento da impostare sul nome del CMR
220100160121     c                   movel     I2DATTIM      I2DATAw           8 0
220200160121    2c                   if        I2DATAw >0
220300160121     c     *iso          movel     DATEU         DATA_iso
220400160121     c                   TEST(D)                 I2DATAw                99
220500160121     c  n99*iso          movel     I2DATAw       DATA_iso
220600160121     c                   movel     DATA_iso      SHP_CDATE
220700160121     c                   end
220800160121      *
220900160121     c                   delete    tidp2000
221000160121     c                   End
221100160121      *
221200160121     c                   Endsr
221300160121     C*----------------------------------------------------------------
221400160121     C*  test su gg lavorativi altrimenti incrementa al primo lavorativo
221500130904     C*----------------------------------------------------------------
221600150929     C     test_gg_lav   BEGSR
221700130904      *
221800150929      * -----------
221900150929      * Cerca definitivamente il D-day lavorativo
222000150929      *  esclude sia il sabato che la domenica
222100150929      * -----------
222200150929     c                   do        *hival
222300150929     c                   move      data_iso      dadata
222400150929     c                   move      data_iso      adata
222500150929     c                   CALL      'XSRLAV8'
222600150929     c                   PARM                    Wdata8
222700150929      * se corrisponde ho trovato il D-Day
222800150929     c                   if        giolav <> 0
222900150929     c                   leave
223000150929     c                   else
223100150929      * altrimenti continuo a cercare il primo giorno lavorativo valido.
223200150929     c     data_iso      adddur    1:*d          data_iso
223300150929     c                   iter
223400150929     c                   endif
223500150929     c                   enddo
223600150929      *
223700150929     c                   Endsr
223800171009     C*----------------------------------------------------------------
223900171009     C*  in base al Depot di provenienza determina Linea e Cliente
224000171009     C*----------------------------------------------------------------
224100171010     C     LNP_da_dove   BEGSR
224200171009      *
224300171009      ** imposta il default
224400171009     c                   z-add     1             gg_incr           3 0
224500171010     c                   z-add     1             giro_nr           3 0
224600171010     c                   clear                   Start_depot7
224700171009     c                   movel     SHP_SDEPOT    Start_depot7      7
224800171010      *
224900171103     c**** ritenta       tag
225000171010     c                   clear                   tisieDEPds
225100171010     c                   clear                   prmCPD10F
225200171010     c                   clear                   prmLEG10F
225300171010      *
225400171009      *  con il DEPOT di provenienza  aggancia x Versione il Cappario DPD
225500171010     c                   movel     Start_depot7  I_DEPOT7
225600171010      ** ------
225700171010      *  aggancia con il DRIVER i legami
225800171010     c                   call      'TISIEDEPR'
225900171010     c                   parm                    tisieDEPds
226000171010     c                   parm                    prmCPD10F
226100171010     c                   parm                    prmLEG10F
226200171009      ** ------
226300171010      ** -  se ritornato tutto OK
226400171010     c                   if        O_FLGVERS ='0' and O_FLGCAPP ='0' and
226500171010     c                             O_FLGLEGA ='0'
226600171009      *
226700171009      *   se trovato il Depot VALIDO
226800171010     c                   eval      DlegFLO = legFLO
226900171009      *
227000171010     c                   if        §legFLOLNP <> *blank  and
227100171010     c                             §legFLOLNP <> '000'
227200171010     c                   if        %check(digits:§legFLOLNP)=0
227300171010     c                   move      §legFLOLNP    linea_partenza    3 0
227400171010      *
227500171009      ** tab.definizione dati x Linea DPD
227600171009     c                   exsr      tabella_LPD
227700171010      *
227800171009     c                   end
227900171010     c                   end
228000171009      *
228100171010     c                   if        §legFLOGGI <> *blank and
228200171010     c                             §legFLOGGI <> '000'
228300171010     c                   if        %check(digits:§legFLOGGI)=0
228400171010     c                   move      §legFLOGGI    gg_incr           3 0
228500171009      *
228600171009     c                   end
228700171009     c                   end
228800171009      *
228900171010     c                   endIF
229000171010      * --------
229100171010     c                   if        §legFLOLNP = '000' or
229200171010     c                             §legFLOLNP = *blank
229300171103     c********           if          giro_nr = 1
229400171103     c*******            clear                   Start_depot7
229500171103     c******             eval        giro_nr = 2
229600171103     c*******            goto      ritenta
229700171103     c********           End
229800171009     c                   End
229900171009      *
230000171009     c                   Endsr
230100150922     C*----------------------------------------------------------------
230200150925     C*  Aggancia tab.LPD ed imposta i campi del VAB
230300150922     C*----------------------------------------------------------------
230400150925     C     tabella_LPD   BEGSR
230500150922      *
230600151016     c                   clear                   FIEULPDds
230700151016     c                   clear                   dLPD
230800151016     c                   movel(p)  linea_partenzaiLPDlnp
230900151016     c                   movel(p)  PRC_PARCELNR  iLPDparcel
231000151016      *
231100151016     c                   call      'FIEULPDR'
231200151016     c                   parm                    FIEULPDds
231300151016      *
231400150925      * ha trovato i dati necessari per instradare il record della spedizione
231500150925      * alla filiale di partenza
231600151016     c                   if        oLPDtip <> 'E'
231700151016     c                   eval         dLPD = oLPDUNI
231800150925     C                   eval      su_EDIVAB = §LPDWRK
231900150925     C                   eval      vabCCM    = §LPDKSC
232000150925     C                   eval      vabCTM    = §LPDCTM
232100151016     C                   eval      vabCTR    = OLPDTAR
232200151012     c                   movel     §LPDTARvtg    tariffa_VTG
232300151012      ** x i Resi
232400151012     c                   if        Sped_RESO = 'S'
232500151012     C                   eval      vabCCM    = §LPDKSCre
232600151016     C                   eval      vabCTR    = OLPDTRE
232700151012     c                   movel     §LPDTREvtg    tariffa_VTG
232800151012     c                   end
232900150925     C                   movel     linea_partenzavabLNP
233000150925     C                   movel     linea_partenzavabFGS
233100150925     c                   end
233200150925      *
233300151012      *  QUI la tariffa LHS non deve essere considerata poichè posso conoscere
233400151012      *  la linea di PARTENZA ma la linea di ARRIVO non la conosco.
233500151012      *
233600150925     c                   Endsr
233700150925     C*----------------------------------------------------------------
233800150925     C*  ESEGUO SCRITTURA FiVAT Riferimenti x il destinatario
233900150925     C*----------------------------------------------------------------
234000150925     C     WRTVAT        BEGSR
234100150925      *
234200130904      *   Parcel Nr. --> Riferimento Parcel DPD
234300130904     c                   clear                   fivat000
234400130904      **
234500130904     C                   movel     VABfgs        VATfgs
234600130904     C                   Z-ADD     VABCCM        VATCCM
234700130904     C                   Z-ADD     VABAAS        VATAAS
234800130904     C                   Z-ADD     VABLNP        VATLNP
234900130904     C                   Z-ADD     VABNRS        VATNRS
235000130904     C                   Z-ADD     VABNSP        VATNSP
235100130904      **
235200130905      ** ------------------------------------------------------------------
235300130905      *   E' già predisposto per il Multicollo: DIPENDE come arrivano i dati.
235400130905      ** ------------------------------------------------------------------
235500130904     C                   MOVEL     'E'           VATTRC
235600130904      **
235700130904      **  su nuovo tipo record (E)
235800130904     c                   clear                   DVate
235900130904      *
236000130904      *  IMPOSTA la DS VAT(E)
236100130904     C                   MOVEL     PRC_PARCELNR  §VATEPRN
236200130904     C                   move      *zeros        §VATESCD
236300130904     C                   move      PRC_SERVICECOD§VATESCD
236400130904     C                   move(p)   '%'           §VATEBID
236500171010      **
236600171010      **   se il depot è a 4 o a 7
236700171010     c                   movel     SHP_SDEPOT    §VATESDEP
236800171010      **
236900130904     C                   MOVEL     DVate         VATNOT
237000130904      **
237100150922     C                   If        su_EDIVAB = 'S'
237200150922     C                   MOVEL     VABCMR        VATCMR
237300150922     C                   Z-ADD     VABCNT        VATCNT
237400150922     C                   WRITE     EDiVAT00
237500150922     c                   else
237600130904     C                   WRITE     fiVAT000
237700150922     c                   end
237800150710      **
237900130905      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
238000130904      *   Contatto del Destinatario
238100150716      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
238200130904     C                   if        RCV_RCONTACT <> *Blanks
238300130904     c                   clear                   fivat000
238400130904      **
238500130904     C                   movel     VABfgs        VATfgs
238600130904     C                   Z-ADD     VABCCM        VATCCM
238700130904     C                   Z-ADD     VABAAS        VATAAS
238800130904     C                   Z-ADD     VABLNP        VATLNP
238900130904     C                   Z-ADD     VABNRS        VATNRS
239000130904     C                   Z-ADD     VABNSP        VATNSP
239100130904      **
239200130904     C                   MOVEL     'A'           VATTRC
239300130904     C                   MOVEL     RCV_RCONTACT  VATNOT
239400130904      **
239500150922     C                   If        su_EDIVAB = 'S'
239600150922     C                   MOVEL     VABCMR        VATCMR
239700150922     C                   Z-ADD     VABCNT        VATCNT
239800150922     C                   WRITE     EDiVAT00
239900150922     c                   else
240000130904     C                   WRITE     fiVAT000
240100150922     c                   end
240200150922      *
240300130904     c                   endIF
240400130904      *
240500150716      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
240600130904      *   Contatto Telefonico
240700150716      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
240800130904     C                   if        RCV_RCONTPHO1 <> *Blanks
240900130904     c                   clear                   fivat000
241000130904      **
241100130904     C                   movel     VABfgs        VATfgs
241200130904     C                   Z-ADD     VABCCM        VATCCM
241300130904     C                   Z-ADD     VABAAS        VATAAS
241400130904     C                   Z-ADD     VABLNP        VATLNP
241500130904     C                   Z-ADD     VABNRS        VATNRS
241600130904     C                   Z-ADD     VABNSP        VATNSP
241700130904      **
241800130904     C                   MOVEL     'B'           VATTRC
241900130904     C                   MOVEL     RCV_RCONTPHO1 VATNOT
242000130904      **
242100150922     C                   If        su_EDIVAB = 'S'
242200150922     C                   MOVEL     VABCMR        VATCMR
242300150922     C                   Z-ADD     VABCNT        VATCNT
242400150922     C                   WRITE     EDiVAT00
242500150922     c                   else
242600130904     C                   WRITE     fiVAT000
242700150922     c                   end
242800150922      *
242900130904     c                   endIF
243000150619      *
243100130905      ** -----+++++++++++++++++++++++++++++++++++++++++++-----
243200130905      *
243300130904     C                   ENDSR
243400150715      *  ------------------------------------------------------------------ */
243500150715      *    Cosa scrivere  x records MSG
243600150715      *  ------------------------------------------------------------------ */
243700150715     c     write_MSG     Begsr
243800151109      *
243900151109      *    S O S P E S O
244000151109      *               AL MOMENTO la scrittura dell'ALERT.
244100151109      *              ======================================
244200150715      *
244300150715      *   Servizio PREDICT Mail
244400150715     c                   if         MSG_MAIL_come_Predict ='S'
244500150715     c                               or
244600150715     c                              EMAIL_no_Predict      ='S'
244700150715      *
244800150715     c                   clear                   fivat000
244900150715      **
245000150715     C                   movel     VABfgs        VATfgs
245100150715     C                   Z-ADD     VABCCM        VATCCM
245200150715     C                   Z-ADD     VABAAS        VATAAS
245300150715     C                   Z-ADD     VABLNP        VATLNP
245400150715     C                   Z-ADD     VABNRS        VATNRS
245500150715     C                   MOVEL     'I'           VATTRC
245600150715      **
245700150715     c                   if         MSG_MAIL_come_Predict ='S'
245800150715     C                   eval      VATNOT =
245900150715     c                             %subst(MSG_ServizioMail_Predict:1:35)
246000150715     c                   elseif     EMAIL_no_Predict      ='S'
246100150715     C                   eval      VATNOT =
246200150715     c                             %subst(RCV_Remail:1:35)
246300150715     c                   end
246400150715      **
246500150715     c                   do        nr            wrt               5 0
246600150715     c                   if        nr_sped(wrt) = 0
246700150715     c                   leave
246800150715     c                   end
246900160302     c                   if        nr_sped(wrt) = *all'9'
247000160302     c                   iter
247100160302     c                   end
247200150715     c                   z-add     nr_sped(wrt)  vatNSP
247300150715      *
247400150922     C                   If        su_EDIVAB = 'S'
247500150922     C                   MOVEL     VABCMR        VATCMR
247600150922     C                   Z-ADD     VABCNT        VATCNT
247700150922     C                   WRITE     EDiVAT00
247800150922     c                   else
247900150715     C                   WRITE     fiVAT000
248000150922     c                   end
248100150922      *
248200150715     c                   enddo
248300150715      *-----
248400150715     c                   if        %subst(MSG_ServizioMail_Predict:36:35)
248500150715     c                                 <> *blank
248600150715     c                               or
248700150715     c                             %subst(RCV_Remail:36:35)
248800150715     c                                 <> *blank
248900150715     C                   MOVEL     'J'           VATTRC
249000150715      *-
249100150715     c                   if         MSG_MAIL_come_Predict ='S'
249200150715     C                   eval       VATNOT =
249300150715     c                             %subst(MSG_ServizioMail_Predict:36:35)
249400150715     c                   elseif     EMAIL_no_Predict      ='S'
249500150715     C                   eval      VATNOT =
249600150715     c                             %subst(RCV_Remail:36:35)
249700150715     c                   end
249800150715      **
249900150715     c                   do        nr            wrt               5 0
250000150715     c                   if        nr_sped(wrt) = 0
250100150715     c                   leave
250200150715     c                   end
250300160302     c                   if        nr_sped(wrt) = *all'9'
250400160302     c                   iter
250500160302     c                   end
250600150715     c                   z-add     nr_sped(wrt)  vatNSP
250700150715      *
250800150922     C                   If        su_EDIVAB = 'S'
250900150922     C                   MOVEL     VABCMR        VATCMR
251000150922     C                   Z-ADD     VABCNT        VATCNT
251100150922     C                   WRITE     EDiVAT00
251200150922     c                   else
251300150715     C                   WRITE     fiVAT000
251400150922     c                   end
251500150922      *
251600150715     c                   enddo
251700150715      *
251800150715     c                   endIF
251900150715      **----
252000150715     c                   endIF
252100150715      *
252200150715      *   Servizio PREDICT SMS
252300150715     c                   if         MSG_SMS_come_Predict ='S'  and
252400150715     c                             %subst(MSG_ServizioSMS_Predict:1:16)
252500150715     c                              <> *blank
252600150715     c                                 or
252700150715     c                             %subst(MSG_ServizioMail_Predict:1:35)
252800150715     c                              <> *blank
252900150715     c                                 or
253000150715     c                             EMAIL_no_Predict ='S'
253100150715      **
253200150715     c                   clear                   fivat000
253300150715     C                   movel     VABfgs        VATfgs
253400150715     C                   Z-ADD     VABCCM        VATCCM
253500150715     C                   Z-ADD     VABAAS        VATAAS
253600150715     C                   Z-ADD     VABLNP        VATLNP
253700150715     C                   Z-ADD     VABNRS        VATNRS
253800150715     C                   MOVEL     'S'           VATTRC
253900150715      **
254000150715     c                   clear                   dvatS
254100150715      **
254200150715      ** La DS dell'SMS ha i primi 16 chrs con il numero dell'SMS e i 2 flags 'S/N'
254300150715      *  nella 17 e 18 per pilotare l'invio dell'avviso con SMS e/o con MAIL.
254400150715     c                   if        MSG_SMS_come_Predict ='S' and
254500150715     c                             %subst(MSG_ServizioSMS_Predict:1:16)
254600150715     c                              <> *blank
254700150715     C                   eval       §VATSTEL =
254800150715     c                             %subst(MSG_ServizioSMS_Predict:1:16)
254900150715     C                   eval       §VATSSMP ='S'
255000150715     c                   end
255100150715      **
255200150715      ** se c'era anche la MAIL
255300150715     c                   IF        %subst(MSG_ServizioMail_Predict:1:35)
255400150715     c                              <> *blank
255500150715     C                   eval       §VATSMMP ='S'
255600150715     c                   else
255700150715      * se non c'è il PREDICT e ho la mail la si deve adoperare per comunicazioni LA o FFD
255800150715     c                   if        EMAIL_no_Predict ='S'
255900150715     C                   eval       §VATSMMP ='N'
256000150715     c                   end
256100150715     c                   end
256200150715      **
256300150715     c                   eval       VATNOT = dvatS
256400150715      **
256500150715     c                   do        nr            wrt               5 0
256600150715     c                   if        nr_sped(wrt) = 0
256700150715     c                   leave
256800150715     c                   end
256900160302     c                   if        nr_sped(wrt) = *all'9'
257000160302     c                   iter
257100160302     c                   end
257200150715     c                   z-add     nr_sped(wrt)  vatNSP
257300150715      *
257400150922     C                   If        su_EDIVAB = 'S'
257500150922     C                   MOVEL     VABCMR        VATCMR
257600150922     C                   Z-ADD     VABCNT        VATCNT
257700150922     C                   WRITE     EDiVAT00
257800150922     c                   else
257900150715     C                   WRITE     fiVAT000
258000150922     c                   end
258100150922      *
258200150715     c                   enddo
258300150715      *
258400150715     c                   end
258500150715      *
258600151109     c     fine_MSG      endSR
258700130904      *----------------------------------------------------------------
258800130904      * DETERMINO DATA E CHIAVE SPEDIZIONE
258900130904      *----------------------------------------------------------------
259000130904     C     SUB_keybol    BEGSR
259100130904      *
259200130904      **              **------------------**
259300130904      *    deve controllare sulla tabella "3C" se il numero spedizione
259400130904      *     deve essere mantenuto oppure no
259500130904     C                   clear                   Ds3C
259600130904     C                   Z-ADD     1             TBLKUT
259700130904     C                   MOVEL     '3C'          TBLCOD
259800130904     C                   movel(p)  VABccm        TBLKEY
259900130904     C     KTABel        CHAIN     TABEL00F                           30
260000130904     C                   IF        %Found(TABEL00F) and tblflg = *blank
260100130904     C                   MOVEL     TBLUNI        Ds3C
260200130904     C                   END
260300130904      *
260400130904      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
260500130904     c                   if        §3CFsp <> 'S'
260600130904      *
260700130904     C* NSP => Stacco un numeratore da AZNUM
260800130904     c                   movel     kpjbu         svkpjbu
260900130904     c                   clear                   kpjbu
261000130904     C                   clear                   TRUL33DS
261100130904     C                   eval      I33OPE = *zeros
261200130904     C                   eval      I33CNU = 302
261300130904     C                   eval      I33NUM = 1
261400130904     C                   movel     TRUL33DS      KPJBU
261500130904     C                   call      'TRUL33R'
261600130904     C                   parm                    KPJBA
261700130904     C                   movel     KPJBU         TRUL33DS
261800130904     C                   if        O33ERR = *zeros
261900130904     c                   z-add     o33nrf        NUM_SPED
262000130904     C                   endif
262100130904     c                   movel     svkpjbu       kpjbu
262200130904      ****
262300130904     c                   ELSE
262400130904      *   se si vuole mantenere il numero spedizione
262500130904      ****
262600130904     C                   MOVE      UYEAR         KAAA
262700130904     C                   Z-ADD     3             KCNU
262800130904     C                   Z-ADD     VABLNP        KFIL
262900130904     C     KNUF          CHAIN     FLNUF                              9999
263000130904     C                   IF        *IN99 = *OFF
263100130904     C                   ADD       1             NUFNUM
263200130904     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE E
263300130904     C                   UPDATE    FLNUF
263400130904     c                   END
263500130904      *
263600130904     c                   EndIF
263700130904      **
263800160408     ******  CONTROLLA se per caso la chiave completa della bolla sia già
263900160408     ******   presente o su EDIVAB o su FIVAB--> se SI, non deve scrivere
264000160408     ******   la spedizione ma deve provare a STACCARE un altro NR dal contatore
264100160408     ******   al max. eseguendo 100 tentativi... dopo di che esce dal programma
264200160408     ******   segnalando il problema del Contatore con NUMERI già presenti.
264300160408      **
264400130904      **              **------------------**
264500130904     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE E
264600130904      **              **------------------**
264700160408      **
264800150701      ** se mandano più PARCEL sotto la stessa spedizione,
264900150701      *  serve per riportare il Predict su più spedizioni.
265000150701     c                   add       1             nr
265100150701     c                   z-add     vabNSP        nr_SPED(nr)
265200130904      *
265300130904     C                   MOVE      UYEAR         KAAA
265400130904     C     2000          add       kaaa          VABAAS
265500130904      *
265600130904     C                   ENDSR
265700130904      *----------------------------------------------------*
265800130904      *   DEFINIZIONE CHIAVI                               *
265900130904      *----------------------------------------------------*
266000130904     C     *INZSR        BEGSR
266100130904      *
266200130904     c     *ENTRY        PLIST
266300130904     C                   parm                    esito             1
266400150916     C                   parm                    Err_dettaglio     1
266500130904     C                   eval      esito ='0'
266600150916     C                   clear                   Err_dettaglio
266700130904      *
266800150625     C     key_VTG       KLIST
266900150625     C                   KFLD                    cldCOD
267000150625     C                   KFLD                    cldDEP
267100150629      *
267200130904     C     KNUF          KLIST
267300130904     C                   KFLD                    KAAA
267400130904     C                   KFLD                    KCNU
267500130904     C                   KFLD                    KFIL
267600130904      *
267700130904     C     KTABel        KLIST
267800130904     C                   KFLD                    TBLKUT
267900130904     C                   KFLD                    TBLCOD
268000130904     C                   KFLD                    TBLKEY
268100150714      *
268200150714     C     KTAB          KLIST
268300150714     C                   KFLD                    TBLKUT
268400150714     C                   KFLD                    TBLCOD
268500130904      *
268600150923      *
268700150923     C     KTBE          KLIST
268800150923     C                   KFLD                    TBECOD
268900150923     C                   KFLD                    TBEKE1
269000150923     C                   KFLD                    TBEKE2
269100150923      *
269200130904     C     KORM          KLIST
269300130904     C                   KFLD                    ORMPOE
269400130904     C                   KFLD                    ORMNSR
269500130904     C                   KFLD                    ORMNOR
269600130904     C                   KFLD                    ORMNRV
269700130904     C*
269800130904     C     Kvat4E        KLIST
269900130904     C                   KFLD                    vatTRC
270000130904     C                   KFLD                    vatNOT
270100170925      ****
270200170925     C     kPNDbolla     KLIST
270300170925     C                   KFLD                    pndAAS
270400170925     C                   KFLD                    pndLNP
270500170925     C                   KFLD                    pndNRS
270600170925     C                   KFLD                    pndNSP
270700170925      *
270800150717     C     kVAB01        KLIST
270900150717     C                   KFLD                    vabCCM
271000150717     C                   KFLD                    vabRMN
271100150908      *
271200150908     C     kVAB03        KLIST
271300150908     C                   KFLD                    vabFGS
271400150908     C                   KFLD                    vabCCM
271500150908     C                   KFLD                    vabRMN
271600150717      *
271700150717     C     kVAB04        KLIST
271800150717     C                   KFLD                    vabAAS
271900150717     C                   KFLD                    vabLNP
272000150717     C                   KFLD                    vabNRS
272100150717     C                   KFLD                    vabNSP
272200150717      *
272300150717     C     kVAT01        KLIST
272400150717     C                   KFLD                    VABFGS
272500150717     C                   KFLD                    VABCCM
272600150717     C                   KFLD                    VABAAS
272700150717     C                   KFLD                    VABLNP
272800150717     C                   KFLD                    VABNRS
272900150717     C                   KFLD                    VABNSP
273000150908      *
273100150908     C     keVAT01       KLIST
273200150908     C                   KFLD                    VABFGS
273300150908     C                   KFLD                    VABCCM
273400150908     C                   KFLD                    VABCMR
273500150908     C                   KFLD                    VABCNT
273600150908     C                   KFLD                    VABAAS
273700150908     C                   KFLD                    VABLNP
273800150908     C                   KFLD                    VABNRS
273900150908     C                   KFLD                    VABNSP
274000171020      **
274100171020     c     Kcdp4         klist
274200171020     c                   kfld                    CDPATB
274300171020     c                   kfld                    CDPVER
274400171020     c                   kfld                    CDPDSTR
274500171009      **
274600150714     C*------------------
274700150714      *
274800150714     C* Caricamento Tabella 15
274900150714     C                   Z-ADD     0             X                 4 0
275000150714     C                   Z-ADD     1             TBLKUT
275100150714     C                   MOVEL     '15'          TBLCOD
275200150714     C     KTAB          setll     TABEL00F
275300150714     C     KTAB          reade     TABEL00F
275400150714     C                   DOW       not %Eof(TABEL00F)
275500150714     C     TBLFLG        IFEQ      *BLANKS
275600150714     C                   ADD       1             X
275700150714     C                   MOVEL     TBLKEY        C15(X)
275800150714     C                   MOVEL     TBLUNI        DS15
275900150714     C                   MOVEL     §15COD        ISO(X)
276000150715     C                   MOVEL     §15CIE        CIE(X)
276100150714     C                   END
276200150714     C     KTAB          READE     TABEL00F
276300150714     C                   END
276400130904     C*
276500150827     c* carico nomi delle regioni Italiane
276600150827     c                   z-add     1             TBLKUT
276700150827     c                   movel     '14'          TBLCOD
276800150827     c                   z-add     1             x
276900150827     c     ktab          setll     tabel00f
277000150827     c     ktab          reade     tabel00f
277100150827     c                   dow       not %eof(tabel00f)
277200150827     c                   add       1             x
277300150827     c                   movel     tbluni        reg(x)
277400150827     c     ktab          reade     tabel00f
277500150827     c                   enddo
277600150827     c
277700130904     C*------------------
277800130904     C* Apro file transito o FiVAB pilotato dalla tabella
277900130904      * Caricamento Tabella PESI DPD
278000130904     c                   clear                   XX
278100130904     c                   clear                   DPP
278200130904     c     'DPP'         chain     tntbe000                           31
278300130904      *
278400130904     c                   DOW       *in31 = *off
278500130904      * Se il S.I. è indicato deve essere uguale al mio
278600130904     c                   IF        TBEatb = *BLANKS  and
278700130904     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
278800130904     c                   add       1             xx
278900130904     c                   movel     TBEke1        DPPK(XX)
279000130904     c                   movel     TBEuni        DDPP
279100130904     c                   movel     §DPPpkb       DPP(XX)
279200141211     c                   movel     §DPPvlb       vlb(XX)
279300130904     c                   endif
279400130904     c     'DPP'         reade     tntbe000                               31
279500130904     c                   enddo
279600130904      *
279700130904      * Recupero data e ora
279800130904     C                   TIME                    WORA
279900130904     C                   TIME                    W0140
280000130904      * UDATE IN GGMMAAAA
280100130904     C                   MOVE      W0140         WDTGIO
280200130904      * UDATE IN AAAAMMGG
280300130904     C     *eur          MOVEL     WDTGIO        DATA_eur
280400130904     C     *iso          MOVEL     DATA_eur      dateu
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
302200171020      *  _____________________________________________________________      */
302300171020      **  decodifica il SENDER DEPOT ricevuto a 4 da Anangr.Cappario DPD
302400171020      *  _____________________________________________________________      */
302500171020     c     DEP_CAPDPD    begsr
302600171020      *
302700171020      * trascodifico la B.U.
302800171020      * recupero  versione di cappario
302900171020     C                   clear                   ISID1DRI
303000171020     c                   z-add     dateu         ISID1DRI
303100171020     C                   clear                   OSID1VER
303200171020     C                   clear                   OSID1VERD
303300171020     C                   clear                   OSID1DDE
303400171020     C                   clear                   OSID1DSC
303500171020     C                   clear                   OSID1BCID
303600171020     c                   call      'TISID1R'
303700171020     C                   PARM                    ISID1DRI
303800171020     C                   PARM                    OSID1VER
303900171020     C                   PARM                    OSID1VERD
304000171020     C                   PARM                    OSID1DDE
304100171020     C                   PARM                    OSID1DSC
304200171020     C                   PARM                    OSID1BCID
304300171020      * se TROVATA VERSIONE CAPPARIO
304400171020      * prova a decodificare il DEPOT con il nuovo cappario per reperire la BU
304500171020     c                   if        osid1ver > 0
304600171020     c                   move      *blank        CDPATB
304700171020     c                   z-add     OSID1VER      CDPVER
304800171020     c                   movel     Depot4        CDPDSTR
304900171020     c     Kcdp4         chain     dpcdp12i
305000171020      *   il Depot con la B.U.
305100171020     c                   if        %Found(dpcdp12i)
305200171020     c                   movel     CDPDPTN       SHP_SDEPOT
305300171103     c                   else
305400171103      * depot che non esiste
305500171103     c                   movel     *all'0'       SHP_SDEPOT
305600171103     c                   move      Depot4        SHP_SDEPOT
305700171103     c                   end
305800171020     c                   end
305900171020      ** ------
306000171020     c                   endsr
306100130904      *------------------------------------------------------------------------- -
306200130904      *   Skiere definizione Obbligatorierà dei campi
306300130904      *     M=Mandatory    C=Condition    O=Optional
306400130904      *     M=Obbligatorio C=Condizionale O=Facoltativo
306500130904      * e Skiere definizione Alfanumericità o Numericità dei campi A/N
306600130904      *------------------------------------------------------------------------- -
306700150702**  SK_HED_O
306800150702MMM
306900150702**  SK_HED_N
307000150702AAA
307100130905**  SK_SHP_O
307200150702MMMCMOOOOOOMOCMMOMOOMOOOMOOOOOOOCO
307300130905**  SK_SHP_N
307400150702ANAAANAAAAANNNANNAAANNNNNAAAAAAANA
307500130905**  SK_SND_O
307600150702MMCOCOCOMOOOOOOMCMMOCOCOOOOC
307700130905**  SK_SND_N
307800150702ANANAAAAAAAAAAAAAAAAAAAANNNA
307900130905**  SK_RCV_O
308000150702MMOCOCOCMOOOOOOMCMMOOOOOOOOOO
308100130905**  SK_RCV_N
308200150702ANAAAAAAAAAAAAAAAAAAAAANAAAAA
308300130905**  SK_PRC_O
308400141007MMCCOOOOOOMOOOOCCOOOOOCOOOOOCO
308500130905**  SK_PRC_N
308600141007ANAANAAAAANAAAANNNNAANAAAAAAAA
308700130904**  SK_MSG_O
308800130904MMOOMOMMOO
308900130904**  SK_MSG_N
309000130904ANAANNANAA
309100150907**  PAROLE
309200150914VIA
309300150907V.LE
309400150914VIALE
309500150907C.SO
309600150914CORSO
309700150907C.DA
309800150907CDA
309900150914CONTRADA
310000150907LOC.
310100150914LOCALITA
310200150907P.ZA
310300150907PZZA
310400150907PIAZZA
310500150914PIAZZALE
310600150914STR.
310700150907STRADA
310800150907Z.I.
310900150907ZONA
311000150907FRAZ.
311100150907FRAZIONE
311200150914V.LO
311300150914VICOLO
311400150914LARGO
311500150907PODERE
311600150907LUNGOMARE
311700150907LUNGOLAGO
