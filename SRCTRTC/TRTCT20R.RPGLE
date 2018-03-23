000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400180314      //*?************************************************************************
000500180314      //  Da UPLOAD    G E L                                                     *
000600180314????  //  TRASCODIFICA : MESSAGGI GBANC  -  STATI BOLLE EXPORT    FEDEX U.K.     *
000700180314????  //   il programma riporta il messaggio EDI sul FLAT RCVIFTSTA              *
000800180314????  //   riconvertendo la logica ricevuta con la nostra standard del flat file *
000900180314????  //   inoltre tramite il file di conversione degli stati EDSTS00F riporta i *
001000180314????  //   codici ricevuti ai nostri codici standard EEX.                        *
001100180314????  //   In pratica converte il file ricevuto come se avesse ricevuto un file  *
001200180314????  //   seguendo le regole imposte  dal nostro traduttore di EEX, generando   *
001300180314????  //   un flat file leggibile dalle nostre abituali procedure di EEX.        *
001400180314      //*?************************************************************************
001500180314      // Il pgm crea:  RCVIFTSTA                                                 *
001600180314      //*?************************************************************************
001700120829     Ftivin00r  uF   E             DISK    INFDS(VINRECDS) usropn commit
001800100722     FEDTAB01L  IF   E           K DISK
001900111215     FEDSTS01L  IF   E           K DISK
002000180314     FtrtcT20W  O  A E           K DISK    INFDS(SRECDS)
002100180314      //----------------------------------------------------*
002200180314      // Stringhe per conversione alfanumerico/numerico N.Documento
002300120828     D recT            S             35    DIM(16) CTDATA PERRCD(1)
002400180314      //----------------------------------------------------*
002500180314      // numero relativo di record
002600090209     D SRECDS          DS
002700090209     D  NRREC                397    400B 0
002800180314      //
002900180314      // numero relativo di record
003000090213     D VINRECDS        DS
003100090213     D  VNREC                397    400B 0
003200180314      // ---------------------------------------------------
003300100701     D KPJBA         E DS
003400100701     D     svkpjbu     s                   like(kpjbu)
003500100701     D UTEDSE0F      E DS
003600100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003700100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003800180314      // Ds scomposzione tipo capoconti
003900100701     D TCUDS           DS
004000100701     D  F34                    3      4
004100100701     D  F56                    5      6
004200180314      // ?   *--------------------------------------------------------------*
004300180314      // ?   ( Descrizione Campi x scomposizione FLAT record )
004400180314      // ?   *--------------------------------------------------------------*
004500180314      //
004600180314      // ?   * Campi x decodifica * (INPUT  del Record)
004700120829     D  Dati           s           1500
004800120828     D Separa_campi    s              1
004900120828     D Decimal_Separ   s              1
005000180314      // ?   * Campi decodificati * (OUTPUT del Record)
005100180314      //       Schiere di Output / Campi di Output
005200120828     D  Alfa40         s             40
005300120828     d  Sk             s              3u 0
005400120828     D    NR_campi     s              3u 0
005500120828     D  Campi_Record   ds
005600120828     D    Dato_sk                    40    DIM(100)
005700120828     D  Campi_Lunghi   ds
005800120828     D    LunDato_sk                  3u 0 DIM(100)
005900120828     D Campi_Numerici  ds
006000120828     D    DatoNum_sk                  1    DIM(100)
006100120828     D Campi_Decimali  ds
006200120828     D    Decimal_sk                  3u 0 DIM(100)
006300180314      // ?   *--------------------------------------------------------------*
006400120829     D X               S              3  0
006500120829     D Tipo_error      S              1  0
006600120828     D  se_errore      s              1    inz(' ')
006700120828     D  Msg_Err        s            132    inz(' ')
006800120828     D  Msg_Vin_80     s             80    inz(' ')
006801180314     D  Indir_Bartol   s             40    inz(' ')
006802180314     D  Indir_Parz     s             20    inz(' ')
006803180314     D  wrkOgg         s             44    inz(' ')
006804180314     D  wrkEml         s            253    inz(' ')
006805180314     D  wrkEmlcc       s            253    inz(' ')
006806180314     D  wrkMsg         s           5000    inz(' ')
006807180314     D  wrkEsito       s              1    inz(' ')
006808180314     D  errori_in_Wrt  s              1    inz(' ')
006809180314     D  Almeno_Uno     s              1    inz(' ')
006810180314     D  Almeno_Un_Due  s              1    inz(' ')
006811180314     D   WOGGI         s              8s 0
006812180314     D   WOGGI_CMR     s              8s 0
006813180314     D   WORA          s              6s 0
006900180314      // ?* ------------------------------------------------------ *
007000180314      // Records di controllo mandati nell'IGATEAVIS
007100120828     D Testa_Sk        s             10    DIM(50)
007200180314      //----------------------------------------------------*
007300100701     D WLBDA8          DS
007400100701     D  G02DAT                 1      8  0
007401180314
007500100701     D  G02INV                 9     16  0
007600100701     D  G02ERR                17     17
007700100701     D  G02TGI                18     22  0
007800180314      //----------------------------------------------------*
007900120829     D EDIDSPT       E DS
008000120829     D EDIDSPU       E DS
008100120829     D EDIDSPV       E DS
008200120829     D EDIDSPW       E DS
008300120829     D EDIDSPZ       E DS
008400180314     D EDFEDSTS      E DS
008500180314      //---------------------------------------------------------------------*
008600180314      // DS
008700180314      //---------------------------------------------------------------------*
008800180314      // .. per scompozione dati da spedire a seconda del tipo record
008900100712     D EDST00        E DS                  EXTNAME(EDST00DS)
009000100712     D EDST05        E DS                  EXTNAME(EDST05DS)
009100100712     D EDST10        E DS                  EXTNAME(EDST10DS)
009200100712     D EDST15        E DS                  EXTNAME(EDST15DS)
009300100712     D EDSD00        E DS                  EXTNAME(EDSD00DS)
009400100712     D EDSD05        E DS                  EXTNAME(EDSD05DS)
009500100712     D EDSD10        E DS                  EXTNAME(EDSD10DS)
009600100712     D EDSD15        E DS                  EXTNAME(EDSD15DS)
009700100712     D EDSD20        E DS                  EXTNAME(EDSD20DS)
009800180314      //---------------------------------------------------------------------*
009900180314      // .. per reperimento tipo record di EDIFTSTA da scrivere
010000180314      //                    .. 5 - 8  : tipo record
010100100712     D WDAT            DS          1950
010200100712     D  STAPRG                 1      4  0
010300100712     D  STATPR                 5      8
010400180314      //---------------------------------------------------------------------*
010500090209     D Valori_inErr    ds
010600090209     D  SKout_Errori                  1    DIM(50)
010700180314
010800090209     D Descr_Errore    ds
010900090209     D  SKout_DesErr                 50    DIM(50)
011000180314      //----------------------------------------------------*
011100091016     D EoFTivin        s              1
011101180314     D err_bloccante   s              1
011200120828     D allinea_shpNbr  s             15
011300120828     D shp15           S              1    DIM(15)
011400120828     D T               S              3  0
011401180314
011402180314     D al_char         S              3  0
011403180314     D dal_char        S              3  0
011404180314     D Tot_char        S              3  0
011500180314     D Snd_Msg_alert   S              1
011600180314      //----------------------------------------------------*
011700000223     D W0140           S             14  0
011800991129     D WORA            S              6  0
011900100701     D DataGGMMAAA     S              8  0
012000100701     D DATEU           S              8  0
012100100701     D DATA_eur        S               D   DATFMT(*eur)
012200100723     D Work_Data__35   S             35
012300100723     D Work_Format_3   S              3
012301180314     D Ora_Data                      14  0
012302180314     D Anno2           S              2  0
012307180314     D Data            S              6  0
012308180314     D WDATA8          S              8  0
012309180314     D User_Msg        S             10
012400180314      //------------------------------------------
012500180314      //*  segmenti Identificativi parti specifiche del messaggio
012600120104???? D STATUS_Trovato...
012700120104???? D                 s              1
012800180314      //---------------------------------------------------------------------*
012900120829???? D conta_dettagli...
013000100701???? D                 s              5s 0 INZ(0)
013100180314      //---------------------------------------------------------------------*
013200111214     d  Converte_Cod_Status...
013300111214     d                 s              1A
013400111221     d  Cod_IdentKEY   s             10A
013500180314      //
013600180314      //*?------------------------------------------------------------------ */
013700180314      //  Invio E-mail
013800090209     D Indirizzi       s            253
013900090209     D Oggetto         s             44
014000090209     D Messaggio       s           5000
014100180314      //
014200090209     d   DSmail        ds
014300090209     D Mail_Ind                      44
014400090209     d   IND_char                     1    dim(44)
014500180314      //
014600100630     D Lunghezza_Indirizzo...
014700100630     D                 s              5u 0
014800100630     D dalCarattere    s              5u 0
014900100630     D xTOTcaratteri   s              5u 0
015000180314      //---------------------------------------------------------------*
015100090209     D MsgDSP          S            256                                         Testo generico
015200180314      //---------------------------------------------------------------*
015300090209     D SndMg01         S             10                                         Message type
015400090209     D                                     INZ('*INFO')
015500090209     D SndMg02         S             10                                         Deluvery mode
015600090209     D                                     INZ('*BREAK')
015700090209     D SndMg03         S            256                                         Message text
015800090209     D SndMg04         S             10I 0                                      Length of text
015900090209     D                                     INZ(%SIZE(SndMg03))
016000090209     D SndMg05         S             10                                         User profile
016100090209     D                                     DIM(299)
016200090209     D SndMg06         S             10I 0                                      Number of user
016300090209     D SndMg07         S             10I 0                                      Message sent indic.
016400090209     D SndMg08         S             10I 0                                      Function requested
016500090209     D SndMg10         S              1                                         Show display
016600090209     D                                     INZ('N')
016700090209     D SndMg11         S             20                                         Qualified MSGQ name
016800090209     D SndMg12         S              4                                         Name type indicator
016900090209     D                                     INZ('*USR')
017000180314      // indice di schiera
017100090209     D Jx              S              5I 0
017200180314      //
017300090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
017400180314      //
017500120828     d bart_it         C                   CONST('@Brt.it;')
017600180314     d GBANC           C                   CONST('GBANC')
017700180314      // ?================================================================== */
017800180314      // ?   * Campi x decodifica * (INPUT  del Record)
017900180314      // ?================================================================== */
018000060710     D Digits          C                   '0123456789'
018100180314      //------------------
018200091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
018300091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
018400180314      // ?================================================================== */
018500180314      //   Ciclo principale
018600180314      // ?================================================================== */
018700130321     c                   clear                   manda_mail        1
018800180314      //*  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
018900180314
018901180314      /FREE
019000180314       if not %open(tivin00r);
019100180314         open tivin00r;
019200180314       endif;
019300180314
019400180314       setll *start Tivin00r;
019500180314       //*---------------------- */
019600180314       EXSR LEGGE_TIVIN_R;
019700180314       //*---------------------- */
019800180314       dow EoFTivin <> 'S';
019900120828
020000180314         // solo i record sflaggati da rielaborare
020100180314         IF vinFLG = *blank;
020200180314           //  Sempre Record OK
020300180314           vinFLG = '1';
020400180314           //** >>>>>>
020500180314           IF vinDTA <> *blank;
020600180314             //** >>>>>>
020700180314             //*  Ad ogni inizio Dettaglio incrementa il contatore dei dettagli
020800180314             If conta_dettagli = 0;
020900180314               eval conta_dettagli = conta_dettagli + 1;
021000180314
021100180314               // SOLO la prima volta Scrivere la Testata
021200180314               If conta_dettagli = 1;
021300180314                 EXSR TESTAMSG;
021400180314               ENDIF;
021500180314             endIF;
021600180314             //
021700180314             //* Decodifica record a campi variabili
021800180314             //*---------------------- */
021900180314             exsr Decod_Record;
022300180314             exsr Scrive_Dett;
022400180314             //*---------------------- */
022500120828
022600180314             //  x errore bloccante nella composizione del VAB/VAT
022700180314             if err_bloccante ='S';
022800180314               vinFLG = '2';
022900180314               esito  = '2';
023000180314????           Oggetto ='Problemi UPLOAD STATI GBANC';
023100180314               Messaggio = 'ATTENZIONE : traduzione GBANC -
023200180314       STATI su Bolle export - messaggio -
023300180314       errato ricevuto su UPLOAD.';
023400180314               if §PVinv <> 'N'
023500180314                     and manda_mail = 'S';
023600180314                 exsr invio_mail;
023700180314               ENDIF;
023800180314             ENDIF;
023900180314             //** >>>>>>
024000180314           endIF;
024100180314           //** >>>>>>
024200180314           //   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
024300180314           //     viene inviato un erroneo messaggio
024400180314           update Tivin000;
024500180314           //** ++++++
024600180314         endIF;
024700120828
024800180314         //*---------------------- */
024900180314         EXSR LEGGE_TIVIN_R;
025000180314         //*---------------------- */
025100180314       ENDdo;
025200180314       //*
025300180314       // ?
025400180314       //
025500180314       //  se c'erano errori bloccanti ma almeno un record è stato tradotto
025600180314       if (almeno_uno ='S' and esito ='1' ) or
025700180314             esito ='0';
025800180314         esito ='0';
025900180314         //
026000180314         //
026100180314         if almeno_un_due ='S';
026200180314           esito ='1';
026300180314         endif;
026400180314         //
026500180314       ENDIF;
026600180314       // chiude TIVIN
026700180314       if %open(tivin00r);
026800180314         close tivin00r;
026900180314       endif;
027000180314       //
027100180314       // se almeno un errore invia la mail di controllo
027200180314       if errori_in_Wrt = 'S';
027300180314         if §PVinv <> 'N'
027400180314               and manda_mail = 'S';
027500180314????       Oggetto ='Problemi UPLOAD stati GBANC';
027600180314           Messaggio = 'ATTENZIONE : traduzione GBANC -
027700180314       STATI consegna messaggio -
027800180314       ricevuto errato su UPLOAD.';
027900180314           exsr invio_mail;
028000180314         ENDIF;
028100180314       endif;
028200180314       //
028300180314       COMMIT;
028400180314       *INLR = *ON;
028500180314       // ?================================================================== */
028600180314       //?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
028700180314       // ?------------------------------------------------------------------ */
028800180314       BEGSR LEGGE_TIVIN_R;
028900180314         //
029000180314         clear EoFTivin;
029100180314         //  Lettura sequenziale
029200180314         read Tivin00r;
029300180314         //**
029400180314         if %Eof(tivin00r);
029401180314           EoFTivin = 'S';
029600180314         else;
029800180314           //* Controlli formali sui campi
029900180314           clear se_errore;
030000180314           clear Msg_Vin_80;
030100180314           clear err_bloccante;
030300180314      // ?  imposta il tipo di segmento:
030400180314           clear dati;
030401180314      /END-FREE
030500100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
030501180314      /FREE
030600180314           dati = %trim(dati) + ';';
030700180314           // ?              /*---------------------- */
030800180314           exsr Split_Rec;
030900180314           // ?              /*---------------------- */
031000180314           //*
031100180314         ENDIF;
031200180314         //
031300180314       ENDSR;
031400180314       // ?------------------------------------------------------------------ */
031500180314       //?      Suddivide i campi della riga con carattere divisorio (;)
031600180314       // ?------------------------------------------------------------------ */
031700180314       Begsr Split_Rec;
031800180314         //*
031900180314         clear Nr_campi;
032000180314         clear Campi_Record;
032100180314         clear Campi_Lunghi;
032200180314         clear Campi_Numerici;
032300180314         clear Campi_Decimali;
032301180314      /END-FREE
032400120828     c                   movel     ';'           Separa_campi
032500120828     c                   movel     '.'           Decimal_Separ
032600180314      //*
032700180314      //  Essendo un file CSV come DPD richiama il FIEU anzichè il TRTCT
032800120828     c                   call      'FIEU00R'
032900180314      // ?     Input
033000120828     c                   parm                    Separa_campi
033100120828     c                   parm                    Decimal_Separ
033200120828     c                   parm                    dati
033300180314      // ?     Output  su schiere di 100 elementi
033400120828     c                   parm                    Nr_campi
033500120828     c                   parm                    Campi_Record                   * i dati
033600120828     c                   parm                    Campi_Lunghi                   * lunghezza dati
033700120828     c                   parm                    Campi_Numerici                 * se numerici
033800120828     c                   parm                    Campi_Decimali                 * quanti decimali
033900180314      //*
033901180314      /FREE
034000180314       Endsr;
034100180314       //----------------------------------------------------------------
034200180314       //? TESTAMSG - SCRIVO TESTATA MESSAGGIO     TUTTO CHIODATO
034300180314       //----------------------------------------------------------------
034400180314       BEGSR TESTAMSG;
034500180314         //
034600180314         // Creo la TESTA  chiodando tutti i records poichè non ricevo nulla
034700180314         //  dal loro file.
034800180314         //
034900180314         // Eseguo scrittura dati record ST00   (UNB/BGM/DTM)
035000180314         EXSR WRST00;
035100180314         // Eseguo scrittura dati record ST05   (NAD)
035200180314         EXSR WRST05;
035300180314         //
035400180314       ENDSR;
035500180314       //----------------------------------------------------------------
035600180314       //? WRST00 - Scrittura record partner   TUTTO CHIODATO
035700180314       //----------------------------------------------------------------
035800180314       BEGSR WRST00;
035900180314         //
036000180314         //  Pulisco dati DS
036100180314         CLEAR EDST00;
036200180314         //
036300180314         eval TA0004 = GBANC;
036400180314         eval TA0010 = 'ITBAR005';
036500180314         eval TAA007 = 'ZZ';
036600180314         //  Data/Ora invio messagio:
036700180314         eval TA0017 = woggi;
036701180314      /END-FREE
036800120829     C                   movel     Wora          TA0019
036900180314      //  Codice documento - nome documento
036901180314      /FREE
037000180314         eval TA1001 = '784';
037100180314         eval TA1000 = 'STATI GBANC';
037200180314         eval TA1004 = '1';
037300180314         eval TA1225 = '9';
037400180314         //  Data messaggio:
037500180314         eval TA2005 = '137';
037501180314      /END-FREE
037600120829     C                   movel     woggi         TA2380
037601180314      /FREE
037700180314         eval TA2379 = '102';
037800180314         //--
037900180314         //  Scivo record IFTSTA
038000180314         CLEAR trtct20r;
038100180314         CLEAR WDAT;
038101180314      /END-FREE
038200120829     C                   MOVEL     EDST00        WDAT
038300120829     C                   MOVEL     'ST00'        STATPR
038400120829     C                   MOVEL     WDAT          FLATREC
038401180314      /FREE
038500180314         WRITE(E) TRTCT20R;
038501180314         *IN99 = %ERROR;
038600180314         //
038700180314       ENDSR;
038800180314       //----------------------------------------------------------------
038900180314       //? WRST05 - Scrittura record partner
039000180314       //----------------------------------------------------------------
039100180314       BEGSR WRST05;
039200180314         //
039300180314         //  Ship from:
039400180314         CLEAR EDST05;
039401180314      /END-FREE
039500120829     C                   MOVEL     'SF'          TE3035
039600180314     C                   MOVEL     'GBANC UK'    TE3036
039601180314      /FREE
039700180314         CLEAR TRTCT20R;
039800180314         CLEAR WDAT;
039801180314      /END-FREE
039900120829     C                   MOVEL     EDST05        WDAT
040000120829     C                   MOVEL     'ST05'        STATPR
040100120829     C                   MOVEL     WDAT          FLATREC
040101180314      /FREE
040200180314         WRITE TRTCT20R;
040300180314         //  Ship to:
040400180314         CLEAR EDST05;
040401180314      /END-FREE
040500120829     C                   MOVEL     'ST'          TE3035
040600120829     C                   MOVEL     'BRT ITALY'   TE3036
040601180314      /FREE
040700180314         CLEAR TRTCT20R;
040800180314         CLEAR WDAT;
040801180314      /END-FREE
040900120829     C                   MOVEL     EDST05        WDAT
041000120829     C                   MOVEL     'ST05'        STATPR
041100120829     C                   MOVEL     WDAT          FLATREC
041101180314      /FREE
041200180314         WRITE(E) TRTCT20R;
041201180314         *IN99 = %ERROR;
041300180314         //
041400180314       ENDSR;
041500180314       //----------------------------------------------------------------
041600180314       //  Decodifica la stringa e la scompone negli appositi CAMPI
041700180314       //----------------------------------------------------------------
041800180314       Begsr Decod_Record;
041900180314         //
042000180314         //** ------------------------------------
042100180314         //   Decodifica e scrive il FLAT file   *
042200180314         //** ------------------------------------
042300180314         //*   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
042400180314         clear EDFEDSTS;
042500180314         clear Msg_Err;
042600180314         //*
042700180314         // ? Nel 1° campo : c'è sempre il TIPO RECORD
042701180314      /END-FREE
042800120828     C                   z-add     1             sk
042801180314      /FREE
042900180314         If LunDato_sk(sk) >  0;
043000180314           TTYPEREC   = %subst(Dato_sk(sk):1:+
043100180314               LunDato_sk(sk));
043200180314         ENDIF;
043300180314         //*
043400180314         // ? Nel 2° campo : c'è sempre il DEPOT Mittente
043401180314      /END-FREE
043500120828     C                   add       1             sk
043501180314      /FREE
043600180314         If LunDato_sk(sk) >  0;
043700180314           TSNDDEPOT  = %subst(Dato_sk(sk):1:+
043800180314               LunDato_sk(sk));
043900180314         ENDIF;
044000180314         //*
044100180314         // ? Nel 3° campo : BARCODE                  (Obbligatorio)
044200180314         //*
044201180314      /END-FREE
044300120828     C                   add       1             sk
044301180314      /FREE
044400180314         If LunDato_sk(sk) =  0;
044500180314           // ?-----> Errore
044600180314           tipo_error = 1;
044700180314           exsr Field_Error;
044800180314         Else;
044900180314           TBARCODE = %subst(Dato_sk(sk):1:+
045000180314               LunDato_sk(sk));
045100180314         ENDIF;
045200180314         //*
045300180314         // ? Nel 4° campo :  RIFERIMENTO SPEDIZIONE   (Obbligatorio)
045400180314         //*
045500180314         clear allinea_shpNbr;
045600180314         clear shp15;
045700180314         clear T;
045800180314         //*
045801180314      /END-FREE
045900120828     C                   add       1             sk
045901180314      /FREE
046000180314         If DatoNum_sk(sk) = 'N' or
046100180314               LunDato_sk(sk) =  0;
046200180314           // ?-----> Errore
046300180314           tipo_error = 2;
046400180314           exsr Field_Error;
046500180314         Else;
046600180314           //
046601180314      /END-FREE
046700120828     c                   move      *all'0'       fld30            30
046701180314      /FREE
046800180314           %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
046900180314               )= %subst(Dato_sk(sk):1:LunDato_sk(sk));
046901180314      /END-FREE
047000120829     c                   move      fld30         TSHPNUMBER
047100120828
047200180314      //*  x eliminare gli zeri inutili davanti al
047300180314      //    numero di riferimento della Spedizione
047400120829     C                   MOVE      TSHPNUMBER    allinea_shpNbr
047500120828     C                   MOVEA     allinea_shpNbrshp15
047600120828     C                   DO        15            T
047601180314      /FREE
047700180314             IF shp15(T) = '0';
047800180314               shp15(T) = ' ';
047900180314             ELSE;
048000180314               leave;
048100180314             ENDIF;
048200180314           ENDDO;
048201180314      /END-FREE
048300120828     C                   movea(p)  shp15(T)      allinea_shpNbr
048400180314      //*
048401180314      /FREE
048500180314         ENDIF;
048600180314         //*
048700180314         // ? Nel 5° campo :  CODICE EVENTO obbligatorio
048800180314         //*
048801180314      /END-FREE
048900120828     C                   add       1             sk
048901180314      /FREE
049000180314         If LunDato_sk(sk) =  0;
049100180314           // ?-----> Errore
049200180314           tipo_error = 1;
049300180314           exsr Field_Error;
049400180314         Else;
049500180314           TCODE      = %subst(Dato_sk(sk):1:+
049600180314               LunDato_sk(sk));
049700180314         ENDIF;
049800180314         //*
049900180314         // ? Nel 6° campo :  TIPO COD EVENTO obbligatorio
050000180314         //*
050001180314      /END-FREE
050100120828     C                   add       1             sk
050101180314      /FREE
050200180314         If LunDato_sk(sk) =  0;
050300180314           // ?-----> Errore
050400180314           tipo_error = 1;
050500180314           exsr Field_Error;
050600180314         Else;
050700180314           TTYPECODE  = %subst(Dato_sk(sk):1:+
050800180314               LunDato_sk(sk));
050900180314         ENDIF;
051000180314         //*
051100180314         // ? Nel 7° campo : DATA EVENTO
051200180314         //*
051201180314      /END-FREE
051300120828     C                   add       1             sk
051301180314      /FREE
051400180314         If DatoNum_sk(sk) = 'N' or
051500180314               LunDato_sk(sk) =  0;
051600180314           // ?-----> Errore
051700180314           tipo_error = 2;
051800180314           exsr Field_Error;
051900180314         Else;
052000180314           //
052001180314      /END-FREE
052100120828     c                   move      *all'0'       fld30            30
052101180314      /FREE
052200180314           %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
052300180314               )= %subst(Dato_sk(sk):1:LunDato_sk(sk));
052301180314      /END-FREE
052400120828     c                   move      fld30         TDATE
052401180314      /FREE
052500180314         ENDIF;
052600180314         //*
052700180314         // ? Nel 8° campo : ORA EVENTO
052800180314         //*
052801180314      /END-FREE
052900120828     C                   add       1             sk
052901180314      /FREE
053000180314         If LunDato_sk(sk) >  0;
053100180314           TTIME      = %subst(Dato_sk(sk):1:+
053200180314               LunDato_sk(sk));
053300180314         ENDIF;
053400180314         //*
053500180314         // ? Nel 9° campo :  NOME DEL CONSEGNATARIO
053600180314         //*
053601180314      /END-FREE
053700120828     C                   add       1             sk
053701180314      /FREE
053800180314         If LunDato_sk(sk) >  0;
053900180314           TPODNAME   = %subst(Dato_sk(sk):1:+
054000180314               LunDato_sk(sk));
054100180314         ENDIF;
054200180314         //*
054300180314         // ? Nel 10° campo :   vuoto
054400180314         //*
054401180314      /END-FREE
054500120828     C                   add       1             sk
054501180314      /FREE
054600180314         If LunDato_sk(sk) >  0;
054700180314           TFREE1     = %subst(Dato_sk(sk):1:+
054800180314               LunDato_sk(sk));
054900180314         ENDIF;
055000180314         //*
055100180314         // ? Nel 11° campo :  vuoto
055200180314         //*
055201180314      /END-FREE
055300120828     C                   add       1             sk
055301180314      /FREE
055400180314         If LunDato_sk(sk) >  0;
055500180314           TFREE2     = %subst(Dato_sk(sk):1:+
055600180314               LunDato_sk(sk));
055700180314         ENDIF;
055800180314         //*
055900180314       endSR;
056000180314       // ?------------------------------------------------------------------ */
056100180314       //?      Campi in errore
056200180314       // ?------------------------------------------------------------------ */
056300180314       Begsr Field_Error;
056400180314         //*
056500180314         se_errore   = 'S';
056600180314         // msg video o in posta
056700180314         If Msg_Err = *blank;
056800180314
056900180314           Select;
057000180314
057100180314           When Tipo_Error = 1;
057200180314             Msg_Err =  %trim(Testa_sk(sk)) +
057300180314                 ' >> Obblig.- Manca Dato !!';
057400180314
057500180314           When Tipo_Error = 2;
057600180314             Msg_Err =  %trim(Testa_sk(sk)) +
057700180314                 ' >> Obblig.Mancante o NON Numerico !!';
057800180314
057900180314           When Tipo_Error = 3;
058000180314             Msg_Err =  %trim(Testa_sk(sk)) +
058100180314                 ' >> NON Numerico  !!';
058200180314
058300180314           When Tipo_Error = 4;
058400180314             Msg_Err =  %trim(Testa_sk(sk)) +
058500180314                 ' >> Conversione non riuscita !!';
058600180314           EndSL;
058700180314
058800180314         ENDIF;
058900180314         // Errore su VINMSG
059000180314         if Msg_Vin_80 <> ' ';
059100180314           Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
059200180314               + %trim(Testa_sk(sk));
059300180314         else;
059400180314           Msg_Vin_80 = %trim(Testa_sk(sk));
059500180314         ENDIF;
059600180314         //*
059700180314       Endsr;
059800180314       // ?================================================================== */
059900180314       //? Azzera_MSG   - Azzera dati messaggio
060000180314       // ?================================================================== */
060100180314       BEGSR Scrive_Dett;
060200180314         // ?                         --------------
060300180314         //*
060400180314         //*  Se presente un errore nel record emette una segnalazione msg
060500180314         //     altrimenti scrive il FLAT
060600180314????     if msg_err = *blank;
060700180314           // ?  QUANDO TUTTO OK.
060800180314           // ?              /*---------------------- */
060900180314           exsr Scrive_Flat;
061000180314           // ?              /*---------------------- */
061100180314           //
061200180314         ENDIF;
061300180314         //
061400180314????     if msg_err <>*blank;
061500180314           //  Problemi nella decodifica dei campi
061600180314           vinMSG = Msg_ERR;
061700180314           errori_in_Wrt = 'S';
061800180314           vinFLG = '2';
061900180314           Almeno_Un_Due ='S';
062000180314           //
062100180314         ENDIF;
062200180314         //
062201180314      /END-FREE
062300120828     c     end_WRidett   endSR
062400180314      // ?================================================================== */
062500180314      //? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
062600180314      // ?================================================================== */
062601180314      /FREE
062700180314       BEGSR Scrive_FLAT;
062800180314         //
062900180314         //  azzera tutte le variabili per poi scrivere le righe del FLAT file
063000180314         clear EDSD00;
063100180314         clear EDSD05;
063200180314         clear EDSD10;
063300180314         clear EDSD15;
063400180314         clear EDSD20;
063401180314      /END-FREE
063500120829     c                   clear                   EDSD00_ok         1
063600120829     c                   clear                   EDSD05_ok         1
063700120829     c                   clear                   EDSD10_ok         1
063800120829     c                   clear                   EDSD15_ok         1
063900120829     c                   clear                   EDSD20_ok         1
064000180314      //  CNI
064001180314      /FREE
064100180314         CLEAR EDSD00;
064101180314      /END-FREE
064200120829     C                   MOVE      conta_dettagliDA1490
064300120829     C                   MOVEL     allinea_shpNbrDA1004
064400180314     C                   MOVE      *zeros        DA1312
064500120829     c                   move      'S'           EDSD00_ok
064600180314      // STS/RFF/DTM
064700180314      //*     deve trascodificare lo stato da GBANC al nostro EEX
064701180314      /FREE
064800180314         CLEAR EDSD05;
064900180314         STATUS_Trovato = *blank;
065000180314         exsr DATI_STS;
065100180314         //*
065101180314      /END-FREE
065200120829     C                   MOVEL     'AGE'         DB1153
065300120829     C                   MOVEL     allinea_shpNbrDB1154
065400120829     c                   movel     TTIME         Ttime_ora         2
065500120829     c                   move      TTIME         Ttime_minuti      2
065600120829     c                   movel     Ttime_ora     Ttime_4           4
065700120829     c                   move      Ttime_minuti  Ttime_4
065800120829     c                   move      *zeros        position          2 0
065900180314      //
066000180314      //** ok tutti numeri nel campo time
066001180314      /FREE
066100180314         position = %Check(digits:Ttime_4);
066200180314         if position = 0;
066201180314      /END-FREE
066300120829     C                   MOVEL     '203'         DB2379
066400120829     C                   MOVE      Ttime_4       DB2380
066401180314      /FREE
066500180314         else;
066501180314      /END-FREE
066600120829     C                   MOVEL     '102'         DB2379
066601180314      /FREE
066700180314         ENDIF;
066701180314      /END-FREE
066800120829     C                   MOVEL     '7 '          DB2005
066900120829     C                   MOVEL     TDATE         DB2380
067000120829     c                   move      'S'           EDSD05_ok
067100180314      //  FTX
067101180314      /FREE
067200180314         CLEAR EDSD10;
067300180314         if DB9011 = '21 ' and TPODNAME <> *blank;
067301180314      /END-FREE
067400120828     C                   MOVEL     'AP '         DC3035
067500120829     C                   MOVEL     TPODNAME      DC3036
067600120829     c                   move      'S'           EDSD10_ok
067601180314      /FREE
067700180314         ENDIF;
067800180314         //  FTX
067900180314         CLEAR EDSD15;
068000180314         if DB9011 <>'21 ' and TPODNAME <> *blank;
068001180314      /END-FREE
068100120829     C                   MOVEL     'DIN'         DD4451
068200120829     C                   MOVEL     TPODNAME      DD4440
068300120829     c                   move      'S'           EDSD15_ok
068301180314      /FREE
068400180314         ENDIF;
068500180314         //  GID e PCI
068600180314         CLEAR EDSD20;
068601180314      /END-FREE
068700120829     C                   z-add     1             DE1496
068800120829     C                   z-add     1             DE7224
068900120828     C                   MOVEL     'PC'          DE7065
069000120828     C                   MOVEL     '24'          DE4233
069100120829     C                   MOVEL     TBARCODE      DE7102
069200120829     c                   move      'S'           EDSD20_ok
069300180314      //
069301180314      /FREE
069400180314         if TBARCODE       = *blank  or
069500180314               allinea_shpnbr = *blank  or
069600180314               TCODE          = *blank  or
069700180314               TTYPECODE      = *blank  or
069800180314               TDATE          = *zero;
069900180314           errori_in_Wrt = 'S';
070000180314           se_errore  = 'S';
070100180314         else;
070101180314           Almeno_Uno = 'S';
070300180314         ENDIF;
070400180314         // >>>>>>>>>>>>
070500180314         //    Per errore fa saltare la scrittura
070600180314         if se_errore  = 'S';
070601180314            err_bloccante ='S';
070602180314      /END-FREE
070800120828     c                   goto      Error_Flat
070801180314      /FREE
070900180314         ENDIF;
071000180314
071100180314         //  scrittura dettaglio flat
071200180314         if EDSD00_ok = 'S'  and
071300180314               EDSD05_ok = 'S'  and
071400180314               EDSD20_ok = 'S'  and
071500180314               STATUS_Trovato <> *blank;
071600180314           exsr WRSD00;
071700180314           exsr WRSD05;
071800180314           if EDSD10_ok = 'S';
071900180314             exsr WRSD10;
072000180314           ENDIF;
072100180314           if EDSD15_ok = 'S';
072200180314             exsr WRSD15;
072300180314           ENDIF;
072400180314           exsr WRSD20;
072500180314         ENDIF;
072600180314         //
072601180314      /END-FREE
072700120828     C     Error_Flat    Endsr
072800180314      //----------------------------------------------------------------
072900180314      //? WRSD00 - Scrittura record dettaglio
073000180314      //----------------------------------------------------------------
073001180314      /FREE
073100180314       BEGSR WRSD00;
073200180314         //
073300180314         CLEAR TRTCT20R;
073400180314         clear WDAT;
073401180314         WDAT = EDSD00;
073402180314         STATPR = 'SD00';
073403180314         FLATREC = WDAT;
073800180314         WRITE(E) TRTCT20R;
073801180314         *IN99 = %ERROR;
073900180314         //
074000180314       ENDSR;
074100180314       //----------------------------------------------------------------
074200180314       //? WRSD05 - Scrittura record dettaglio
074300180314       //----------------------------------------------------------------
074400180314       BEGSR WRSD05;
074500180314         //
074600180314         CLEAR TRTCT20R;
074700180314         clear WDAT;
074701180314         WDAT = EDSD05;
074702180314         STATPR = 'SD05';
074703180314         FLATREC = WDAT;
075100180314         WRITE(E) TRTCT20R;
075101180314         *IN99 = %ERROR;
075200180314         //
075300180314       ENDSR;
075400180314       //----------------------------------------------------------------
075500180314       //? WRSD10 - Scrittura record dettaglio
075600180314       //----------------------------------------------------------------
075700180314       BEGSR WRSD10;
075800180314         //
075900180314         CLEAR TRTCT20R;
076000180314         clear wdat;
076001180314         WDAT = EDSD10;
076002180314         STATPR = 'SD10';
076003180314         FLATREC = WDAT;
076400180314         WRITE(E) TRTCT20R;
076401180314         *IN99 = %ERROR;
076500180314         //
076600180314       ENDSR;
076700180314       //----------------------------------------------------------------
076800180314       //? WRSD15 - Scrittura record dettaglio
076900180314       //----------------------------------------------------------------
077000180314       BEGSR WRSD15;
077100180314         //
077200180314         CLEAR TRTCT20R;
077300180314         clear WDAT;
077301180314         WDAT = EDSD15;
077302180314         STATPR = 'SD15';
077303180314         FLATREC = WDAT;
077700180314         WRITE(E) TRTCT20R;
077701180314         *IN99 = %ERROR;
077800180314         //
077900180314       ENDSR;
078000180314       //----------------------------------------------------------------
078100180314       //? WRSD20 - Scrittura record dettaglio
078200180314       //----------------------------------------------------------------
078300180314       BEGSR WRSD20;
078400180314         //
078500180314         CLEAR TRTCT20R;
078600180314         clear WDAT;
078601180314         WDAT = EDSD20;
078602180314         STATPR = 'SD20';
078603180314         FLATREC = WDAT;
079000180314         WRITE(E) TRTCT20R;
079001180314         *IN99 = %ERROR;
079100180314         //
079200180314       ENDSR;
079300180314       //==================================================================*
079400180314       // Imposta gli indirizzi mail a cui inviare segnalazione di errori
079500180314       //==================================================================*
079600180314       begsr Imp_ADDR_mail;
079700180314         //
079800180314         clear Indirizzi;
079900180314         //
080000180314         //  Lunghezza indirizzi presenti
080100180314         Lunghezza_Indirizzo =
080200180314             %Len(%TrimR(Mail_Ind));
080201180314             al_char = 1;
080202180314             dal_char = 1;
080203180314             tot_char = 1;
080204180314      /END-FREE
080600180314      //
080700090213     c     successivo    tag
080800180314      //
080900180314      // prende il (;) più prossimo per dividere gli indirizzi a cui spedire
081000180314      //   e aggiunge il dominio Bartolini + il (;) separatore
081001180314      /FREE
081100180314         al_char = %Scan(';' :  Mail_Ind : dal_char);
081200180314         tot_char = al_char - dal_char;
081201180314         dalCarattere = dal_char;
081202180314         xTOTcaratteri = tot_char;
081600180314         clear Indir_Bartol;
081700180314         clear Indir_Parz;
081800180314         Indir_Parz =
081900180314             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri);
082000180314
082100180314         Indir_Bartol = %Trim(Indir_Parz) +
082200180314             %Trim(bart_it);
082300180314         Indirizzi = %Trim(Indirizzi) + Indir_Bartol;
082400180314         //
082500180314         if al_char = Lunghezza_Indirizzo;
082501180314      /END-FREE
082600090213     c                   goto      fine_indmail
082601180314      /FREE
082700180314         else;
082800180314           dal_char = ( al_char + 1 );
082801180314      /END-FREE
082900090213     c                   goto      successivo
082901180314      /FREE
083000180314         ENDIF;
083100180314
083101180314      /END-FREE
083200090213     C     fine_indmail  TAG
083300180314
083301180314      /FREE
083400180314       ENDSR;
083500180314       //==================================================================*
083600180314       // Manda un Msg x E-mail  ANCHE a CED
083700180314       //==================================================================*
083800180314       begsr Email_con_CED;
083900180314
084000180314         //   Alert_mail :
084500180314      // Valorizzo i campi della e-m@ail - indirizzo
084501180314
084502180314         clear wrkEml;
084503180314         clear Oggetto;
084504180314         clear Messaggio;
084600180314         wrkEml = Indirizzi;
084700180314         wrkOgg = Oggetto;
084800180314         wrkMsg = Messaggio;
084900180314         //
085000180314         //   il 1/2/2020 Fabrizio spiega che nel TIS701C viene chiodato l'invio a CED
085100180314         //      ed il TRTCT00R2 invia con il TIS701C
085101180314      /END-FREE
085200130204     C                   call(e)   'TRTCT00R2'
085300130204     C                   parm                    wrkEml
085400130204     C                   parm                    wrkOgg
085500130204     C                   parm                    wrkMsg
085600180314      //
085601180314      /FREE
085700180314       ENDSR;
085800180314       //==================================================================*
085900180314       // Manda un Msg x E-mail  NON a CED
086000180314       //==================================================================*
086100180314       begsr Invio_Mail;
086101180314
086300180314         //   Alert_mail :
086301180314
086302180314         clear wrkEml;
086303180314         clear wrkEmlcc;
086304180314         clear wrkEsito;
086305180314         clear Oggetto;
086306180314         clear Messaggio;
086307180314
087000180314      // Valorizzo i campi della e-m@ail - indirizzo
087100180314         wrkEml = Indirizzi;
087200180314         wrkOgg = Oggetto;
087300180314         wrkMsg = Messaggio;
087400180314         //
087500180314         //  Se richiesta di NICOLA di cambiare solo questi pgm di GBANC per NON mandare a CED
087600180314         //   le mails: il TIS701C1 al contrario del TIS701C (il 1/2/2013 Fabrizio spiega che
087700180314         //   nel TIS701C viene chiodato l'invio a CED)
087701180314      /END-FREE
087800130204     C                   call(e)   'TRTCT00R5'
087900130204     C                   parm                    wrkEml          253
088000130204     C                   parm                    wrkEmlCc        253
088100130204     C                   parm                    wrkOgg           44
088200130204     C                   parm                    wrkMsg         5000
088300130204     C                   parm                    wrkesito          1
088400180314      //
088401180314      /FREE
088500180314       ENDSR;
088600180314       // ?================================================================== */
088700180314       //? Imposta informazioni sullo STATO di Consegna
088800180314       // ?================================================================== */
088900180314       BEGSR DATI_STS;
089000180314         //
089100180314         //  Se abilitato alla conversione dei codici STATUS occorre passare per
089200180314         //   la tabella di conversione e riportare i nostri codici STANDARD
089300180314         //    al posto di quelli ricevuti dal Partner.
089400180314         if Converte_Cod_Status ='S';
089500180314           //*
089600180314           if Cod_IdentKEY = *BLANK;
089700180314             eval STSUNBCLI = GBANC;
089800180314           else;
089900180314             eval STSUNBCLI = Cod_IdentKEY;
090000180314           ENDIF;
090100180314           //*
090200180314           eval STSCODSTS = TtypeCODE;
090300180314           eval STSSUBSTS = Tcode;
090400180314           //*
090500180314           chain key_STS edSTS01l;
090600180314           if %Found(edSTS01l);
090700180314             STATUS_Trovato = 'S';
090800180314             eval DB9011    = STSCODSTSb;
090900180314             eval DB9013    = STSSUBSTSb;
091000180314           else;
091100180314             // deve sempre scrivere adesso per passare l'informazione anche se errata
091200180314             // e non trascodificata così il programma che importa gli status emette una
091300180314             // stampa che in Filiale evidenzia questo tipo di errori ricevuti.
091400180314             STATUS_Trovato = 'S';
091500180314             MSG_ERR ='STATO non Concordato' + Tcode +
091600180314                 '/' + TtypeCODE;
091700180314             eval DB9011    = TtypeCODE;
091800180314             eval DB9013    = Tcode;
091900180314           ENDIF;
092000180314           //*
092100180314         ENDIF;
092200180314         //*
092201180314      /END-FREE
092300120829     c     end_STS       endSR
092400180314      //================================================================
092500180314      //   DEFINIZIONE CHIAVI
092600180314      //================================================================
092601180314      /FREE
092700180314       BEGSR *INZSR;
092800180314         //
092801180314      /END-FREE
092900120829     c     Key_STS       KLIST
093000120829     c                   KFLD                    STSUNBCLI
093100120829     c                   KFLD                    STSCODSTS
093200120829     c                   KFLD                    STSSUBSTS
093201180314      *
093202180314     c     TBkey         klist
093203180314     c                   kfld                    TABCOD
093204180314     c                   kfld                    TABKEY
093205180314      *
093400120829     c     *ENTRY        PLIST
093500120829     C                   parm                    esito             1
093600180314      //
093700180314      // RECUPERO DATI DELL'UTENTE
093800120829     C                   Z-ADD     1             CODUT             1 0
093900120829     C                   CALL      'XPARUT'
094000120829     C                   PARM                    UTEDSE0F
094100120829     C                   MOVEL     RAGUT         RSUT             20
094200180314      // RICERCA CAPOCONTI
094300120829     C                   DO        50            X
094400120829     C                   MOVE      TCU(X)        TCUDS
094401180314      /FREE
094500180314           IF F56 = 'CG'
094600180314           AND F34 = '01';
094601180314      /END-FREE
094700120829     C                   Z-ADD     KCU(X)        KCI               4 0
094701180314      /FREE
094800180314           ENDIF;
094900180314         ENDDO;
095000180314         //
095001180314      /END-FREE
095100180314     c                   movel(p)  'EDPAB'       User_Msg
095200180314      //
095300180314      // IMPOSTO A ZERO VARIABILI DI PGM
095400180314     c                   move      *blank        Snd_Msg_alert
095500180314      //
095600180314      // RECUPERO DATA E ORA
095700180314     C                   TIME                    Ora_Data
095800120829     C                   MOVE      Ora_Data      G02DAT
095900180314     C                   MOVE      Ora_Data      WDATA8
096000180314     C                   MOVE      WDATA8        Anno2
096100180314     C                   MOVEL     WDATA8        DATA
096200120829     C                   MOVE      Anno2         DATA
096300120829     C                   MOVEL     *BLANK        G02ERR
096400120829     C                   CALL      'XSRDA8'
096500120829     C                   PARM                    WLBDA8
096600180314     C                   Z-ADD     G02INV        WOGGI
096700180314     C                   Z-ADD     G02INV        WOGGI_CMR
096800180314     C                   TIME                    WORA
096900180314      //  carica testate campi
097000180314      //
097001180314      /FREE
097100180314         clear Testa_Sk;
097101180314      /END-FREE
097200120829     c                   z-add     0             num               3 0
097300120829     c                   z-add     0             x
097301180314      /FREE
097400180314         num = %elem(recT);
097401180314      /END-FREE
097500120829     c                   do        num           x
097600120829     c                   movel     recT(x)       Testa_Sk(x)
097601180314      /FREE
097700180314         enddo;
097800180314         // Recupero data e ora
097801180314      /END-FREE
097900120829     C                   TIME                    W0140
098000180314      // UDATE IN GGMMAAAA
098100120829     C                   MOVE      W0140         DataGGMMAAA
098300120829     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
098400120829     C     *iso          MOVEL     DATA_eur      dateu
098500180314      //*
098600180314      //  azzera tutte le variabili generali del messaggio
098700180314
098801180314      /FREE
098802180314         clear Almeno_Uno;
098803180314         clear Almeno_Un_Due;
098900180314         esito  = '0';
099000180314
099100180314         //   Contatore dei dettagli
099200180314         conta_dettagli = 0;
099300180314         // Un errore nel dettaglio
099400180314         clear errori_in_Wrt;
099500180314
099600180314         //*--------------------------- */
099700180314         exsr CARICA_TAB_PT;
099800180314         //*--------------------------- */
099900180314
100000180314       ENDSR;
100100180314       // ?------------------------------------------------------------------ */
100200180314       //?    CARICA TABELLE SU SCHIERE
100300180314       // ?------------------------------------------------------------------ */
100400180314       BEGSR CARICA_TAB_PT;
100500180314         //
100900180314      //
101000180314      // Caricamento Tabella Partner esteri
101303180314         X = 0;
101304180314         TABCOD = 'PT';
101305180314         TABKEY = GBANC;
101400180314         CHAIN TBkey EDTAB01L;
101500180314         if %Found(EDTAB01L);
101602180314         edidsPT = TABUNI;
101700180314         ENDIF;
101800180314         //
101903180314         TABCOD = 'PU';
102000180314         CHAIN TBkey EDTAB01L;
102100180314         if %Found(EDTAB01L);
102202180314         edidsPU = TABUNI;
102300180314         ENDIF;
102400180314
102502180314         TABCOD = 'PV';
102600180314         CHAIN TBkey EDTAB01L;
102700180314         if %Found(EDTAB01L);
102802180314         edidsPV = TABUNI;
102900180314         ENDIF;
103000180314
103102180314         TABCOD = 'PZ';
103200180314         CHAIN TBkey EDTAB01L;
103300180314         if %Found(EDTAB01L);
103301180314         edidsPZ = TABUNI;
103500180314         ENDIF;
103600180314
103703180314         TABCOD = 'PW';
103800180314         CHAIN TBkey EDTAB01L;
103900180314         if %Found(EDTAB01L);
103901180314            edidsPW = TABUNI;
104100180314         ENDIF;
104200180314
104300180314         // ?imposta se deve passare per una tabella di conversione STATUS
104400180314         //    poichè inviano differenti STATUS codes rispetto al nostro Standard
104500180314         eval Converte_Cod_Status = §pvCNVSTS;
104600180314         eval Cod_IdentKEY = §pvIDENT;
104700180314
104800180314         // ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
104900180314         //  ricevuti:
104901180314         mail_ind = §PVema;
105200180314      // ?imposta gli indirizzi mail se interni a Bartolini o esterni
105300180314         if mail_ind <> *blank;
105400180314           exsr imp_ADDR_mail;
105500180314         ENDIF;
105600180314         //*
105700180314       ENDSR;
105800180314       // ?------------------------------------------------------------------ */
105900180314       //?      X non bloccare in nessun caso il traduttore CLIENTI
106000180314       // ?------------------------------------------------------------------ */
106100180314       BEGSR *pssr;
106200180314
106300180314         esito = '2';
106400180314????     Oggetto ='Problemi UPLOAD stati GBANC';
106500180314         Messaggio = 'ATTENZIONE : traduzione GBANC -
106600180314       STATI consegna messaggio -
106700180314       ricevuto errato su UPLOAD.';
106800180314         if §PVinv <> 'N';
106900180314           exsr Email_con_CED;
107000180314         ENDIF;
107100180314
107200180314       ENDSR;
107300180314       //==================================================================*
107301180314      /END-FREE
107400120828** Nomi campi record STATI
107500120828TTYPEREC    (TIPO REC -->(T))
107600120828TSNDDEPOT   (DEPOT MITTENTE)
107700120828TBARCODE    (BARCODE)
107800120828TSHPNUMBER  (RIFERIMENTO SPEDIZIONE)
107900180314TCODE       (CODICE EVENTO GBANC)
108000120828TTYPECODE   (TIPO ED1-ED2 ATR O DELIVERY)
108100120828TDATE       (DATA EVENTO)
108200120828TTIME       (ORA EVENTO)
108300120828TPODNAME    (NOME CONSEGNATARIO)
108400120828TFREE1      (FREE)
108500120828TFREE2      (FREE)
