000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500120828      *  Da UPLOAD    G E L                                                     *
000600120828????  *  TRASCODIFICA : MESSAGGI GEL  -  STATI BOLLE EXPORT    NET EXPR.EUROPE  *
000700120119????  *   il programma riporta il messaggio EDI sul FLAT RCVIFTSTA              *
000800120119????  *   riconvertendo la logica ricevuta con la nostra standard del flat file *
000900120119????  *   inoltre tramite il file di conversione degli stati EDSTS00F riporta i *
001000120119????  *   codici ricevuti ai nostri codici standard EEX.                        *
001100120119????  *   In pratica converte il file ricevuto come se avesse ricevuto un file  *
001200120828????  *   seguendo le regole imposte  dal nostro traduttore di EEX, generando   *
001300120119????  *   un flat file leggibile dalle nostre abituali procedure di EEX.        *
001400060612      **?************************************************************************
001500100712      * Il pgm crea:  RCVIFTSTA                                                 *
001600060609      **?************************************************************************
001700120829     Ftivin00r  uF   E             DISK    INFDS(VINRECDS) usropn commit
001800100722     FEDTAB01L  IF   E           K DISK
001900111215     FEDSTS01L  IF   E           K DISK
002000120829     FtrtcT13W  O  A E           K DISK    INFDS(SRECDS)
002100100701      *----------------------------------------------------*
002200100701      * Stringhe per conversione alfanumerico/numerico N.Documento
002300120828     D recT            S             35    DIM(16) CTDATA PERRCD(1)
002400940321      *----------------------------------------------------*
002500090209      * numero relativo di record
002600090209     D SRECDS          DS
002700090209     D  NRREC                397    400B 0
002800090209      *
002900090213      * numero relativo di record
003000090213     D VINRECDS        DS
003100090213     D  VNREC                397    400B 0
003200100701      * ---------------------------------------------------
003300100701     D KPJBA         E DS
003400100701     D     svkpjbu     s                   like(kpjbu)
003500100701     D UTEDSE0F      E DS
003600100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003700100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003800100701      * Ds scomposzione tipo capoconti
003900100701     D TCUDS           DS
004000100701     D  F34                    3      4
004100100701     D  F56                    5      6
004200120828      * ?   *--------------------------------------------------------------*
004300120828      * ?   ( Descrizione Campi x scomposizione FLAT record )
004400120828      * ?   *--------------------------------------------------------------*
004500120828      *
004600120828      * ?   * Campi x decodifica * (INPUT  del Record)
004700120829     D  Dati           s           1500
004800120828     D Separa_campi    s              1
004900120828     D Decimal_Separ   s              1
005000120828      * ?   * Campi decodificati * (OUTPUT del Record)
005100120828      *       Schiere di Output / Campi di Output
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
006300120828      * ?   *--------------------------------------------------------------*
006400120829     D X               S              3  0
006500120829     D Tipo_error      S              1  0
006600120828     D  se_errore      s              1    inz(' ')
006700120828     D  Msg_Err        s            132    inz(' ')
006800120828     D  Msg_Vin_80     s             80    inz(' ')
006900120828      * ?* ------------------------------------------------------ *
007000120828      * Records di controllo mandati nell'IGATEAVIS
007100120828     D Testa_Sk        s             10    DIM(50)
007200100722      *----------------------------------------------------*
007300100701     D WLBDA8          DS
007400100701     D  G02DAT                 1      8  0
007500100701     D  G02INV                 9     16  0
007600100701     D  G02ERR                17     17
007700100701     D  G02TGI                18     22  0
007800120829      *----------------------------------------------------*
007900120829     D EDIDSPT       E DS
008000120829     D EDIDSPU       E DS
008100120829     D EDIDSPV       E DS
008200120829     D EDIDSPW       E DS
008300120829     D EDIDSPZ       E DS
008400120829     D EDGELSTS      E DS
008500100712     D*---------------------------------------------------------------------*
008600100712     D* DS
008700100712     D*---------------------------------------------------------------------*
008800100712     D* .. per scompozione dati da spedire a seconda del tipo record
008900100712     D EDST00        E DS                  EXTNAME(EDST00DS)
009000100712     D EDST05        E DS                  EXTNAME(EDST05DS)
009100100712     D EDST10        E DS                  EXTNAME(EDST10DS)
009200100712     D EDST15        E DS                  EXTNAME(EDST15DS)
009300100712     D EDSD00        E DS                  EXTNAME(EDSD00DS)
009400100712     D EDSD05        E DS                  EXTNAME(EDSD05DS)
009500100712     D EDSD10        E DS                  EXTNAME(EDSD10DS)
009600100712     D EDSD15        E DS                  EXTNAME(EDSD15DS)
009700100712     D EDSD20        E DS                  EXTNAME(EDSD20DS)
009800100712     D*---------------------------------------------------------------------*
009900100712     D* .. per reperimento tipo record di EDIFTSTA da scrivere
010000100712     D*                    .. 5 - 8  : tipo record
010100100712     D WDAT            DS          1950
010200100712     D  STAPRG                 1      4  0
010300100712     D  STATPR                 5      8
010400100712     D*---------------------------------------------------------------------*
010500090209     D Valori_inErr    ds
010600090209     D  SKout_Errori                  1    DIM(50)
010700100630      **
010800090209     D Descr_Errore    ds
010900090209     D  SKout_DesErr                 50    DIM(50)
011000090205      *----------------------------------------------------*
011100091016     D EoFTivin        s              1
011200120828     D allinea_shpNbr  s             15
011300120828     D shp15           S              1    DIM(15)
011400120828     D T               S              3  0
011500120828      *
011600091016      *----------------------------------------------------*
011700000223     D W0140           S             14  0
011800991129     D WORA            S              6  0
011900100701     D DataGGMMAAA     S              8  0
012000100701     D DATEU           S              8  0
012100100701     D DATA_eur        S               D   DATFMT(*eur)
012200100723     D Work_Data__35   S             35
012300100723     D Work_Format_3   S              3
012400100318      *------------------------------------------
012500100316      **  segmenti Identificativi parti specifiche del messaggio
012600120104???? D STATUS_Trovato...
012700120104???? D                 s              1
012800090209      *---------------------------------------------------------------------*
012900120829???? D conta_dettagli...
013000100701???? D                 s              5s 0 INZ(0)
013100100701      *---------------------------------------------------------------------*
013200111214     d  Converte_Cod_Status...
013300111214     d                 s              1A
013400111221     d  Cod_IdentKEY   s             10A
013500111214      *
013600060608      **?------------------------------------------------------------------ */
013700090209      *  Invio E-mail
013800090209     D Indirizzi       s            253
013900090209     D Oggetto         s             44
014000090209     D Messaggio       s           5000
014100090209      *
014200090209     d   DSmail        ds
014300090209     D Mail_Ind                      44
014400090209     d   IND_char                     1    dim(44)
014500090209      *
014600100630     D Lunghezza_Indirizzo...
014700100630     D                 s              5u 0
014800100630     D dalCarattere    s              5u 0
014900100630     D xTOTcaratteri   s              5u 0
015000090209     C*---------------------------------------------------------------*
015100090209     D MsgDSP          S            256                                         Testo generico
015200090209     C*---------------------------------------------------------------*
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
017000111214      * indice di schiera
017100090209     D Jx              S              5I 0
017200090209      *
017300090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
017400090209      *
017500120828     d bart_it         C                   CONST('@Brt.it;')
017600120828     d GEL_DE          C                   CONST('GEL_DE')
017700060612      * ?================================================================== */
017800060612      * ?   * Campi x decodifica * (INPUT  del Record)
017900100319      * ?================================================================== */
018000060710     D Digits          C                   '0123456789'
018100091019     D*------------------
018200091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
018300091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
018400060612      * ?================================================================== */
018500060612      *   Ciclo principale
018600090205      * ?================================================================== */
018700130321     c                   clear                   manda_mail        1
018800060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
018900060612     C*
019000060612     C                   if        not %open(tivin00r)
019100060612     C                   open      tivin00r
019200060612     C                   endif
019300090205      **
019400120828     c     *start        setll     Tivin00r
019500120828      * ?              /*---------------------- */
019600120828     c                   EXSR      LEGGE_TIVIN_R
019700120828      * ?              /*---------------------- */
019800120828     c                   dow             EoFTivin <> 'S'
019900120828
020000120828      * solo i record sflaggati da rielaborare
020100120828     c                   IF              vinFLG = *blank
020200120828      *  Sempre Record OK
020300120828     C                   eval      vinFLG = '1'
020400120828      *** >>>>>>
020500120828     c                   IF              vinDTA <> *blank
020600120828      *** >>>>>>
020700120828      **  Ad ogni inizio Dettaglio incrementa il contatore dei dettagli
020800120829     c                   If                    conta_dettagli = 0
020900120829     c                   eval        conta_dettagli = conta_dettagli + 1
021000120828      *
021100120828      * SOLO la prima volta Scrivere la Testata
021200120829     c                   If           conta_dettagli = 1
021300120828     C                   EXSR      TESTAMSG
021400120828     c                   end
021500120828     c                   endIF
021600120828      *
021700120828      ** Decodifica record a campi variabili
021800120828      * ?              /*---------------------- */
021900120828     c                   exsr      Decod_Record
022000120828      * ?              /*---------------------- */
022100120828
022200120828      * ?              /*---------------------- */
022300120828     c                   exsr      Scrive_Dett
022400120828      * ?              /*---------------------- */
022500120828
022600120828      *  x errore bloccante nella composizione del VAB/VAT
022700120828     c                   if             err_bloccante ='S'
022800120828     C                   eval      vinFLG = '2'
022900120828     c                   eval      esito  = '2'
023000120828???? C                   EVAL      Oggetto ='Problemi UPLOAD STATI GEL'
023100120828     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
023200120828     C                             STATI su Bolle export - messaggio -
023300120828     C                             errato ricevuto su UPLOAD.'
023400120828     c                   if            §PVinv <> 'N'
023500130321     c                               and manda_mail = 'S'
023600120828     c                   exsr      invio_mail
023700120828     c                   end
023800120828     c                   end
023900120828      *** >>>>>>
024000120828     c                   endIF
024100120828      *** >>>>>>
024200120828      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
024300120828      *     viene inviato un erroneo messaggio
024400120828     c                   update    Tivin000
024500120828      *** ++++++
024600120828     c                   endIF
024700120828
024800120828      * ?              /*---------------------- */
024900120828     c                   EXSR      LEGGE_TIVIN_R
025000120828      * ?              /*---------------------- */
025100120828     C                   ENDdo
025200120828      **
025300060612      * ?              /*---------------------- */
025400060612      *
025500060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
025600090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
025700090225     c                             esito ='0'
025800060710     C                   eval      esito ='0'
025900090225      *
026000090224      *
026100090213     c                   if        almeno_un_due ='S'
026200090213     C                   eval      esito ='1'
026300060614     C                   endif
026400120828      *
026500100723     C                   end
026600100708      * chiude TIVIN
026700100708     C                   if        %open(tivin00r)
026800100708     C                   close     tivin00r
026900100708     C                   endif
027000121210      *
027100121210      * se almeno un errore invia la mail di controllo
027200121210     c                   if        errori_in_Wrt = 'S'
027300121210     c                   if        §PVinv <> 'N'
027400130321     c                               and manda_mail = 'S'
027500121210???? C                   EVAL      Oggetto ='Problemi UPLOAD stati GEL'
027600121210     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
027700121210     C                             STATI consegna messaggio -
027800121210     C                             ricevuto errato su UPLOAD.'
027900121210     c                   exsr      invio_mail
028000121210     c                   end
028100121210     C                   endif
028200060614      *
028300121106     c                   COMMIT
028400060612     c                   SETON                                        LR
028500060612      * ?================================================================== */
028600100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
028700100709      * ?------------------------------------------------------------------ */
028800100709     C     LEGGE_TIVIN_R BEGSR
028900100709      *
029000100709     c                   clear                   EoFTivin
029100100709      *  Lettura sequenziale
029200100709     c                   read      Tivin00r
029300120828      ***
029400100709     c                   if        %Eof(tivin00r)
029500100709     c                   move      'S'           EoFTivin
029600100709     c                   else
029700120828      *
029800120828      ** Controlli formali sui campi
029900120828     c                   clear                   se_errore
030000120828     C                   clear                   Msg_Vin_80
030100120828     c                   clear                   err_bloccante     1
030200100709      *
030300100709      * ?  imposta il tipo di segmento:
030400100709     c                   clear                   dati
030500100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
030600120829     c                   eval      dati = %trim(dati) + ';'
030700120828      * ?              /*---------------------- */
030800120828     c                   exsr      Split_Rec
030900120828      * ?              /*---------------------- */
031000120828      **
031100120828     c                   end
031200100709      *
031300100709     C                   ENDSR
031400120828      * ?------------------------------------------------------------------ */
031500120828      *?      Suddivide i campi della riga con carattere divisorio (;)
031600120828      * ?------------------------------------------------------------------ */
031700120828     c     Split_Rec     Begsr
031800120828      **
031900120828     c                   clear                   Nr_campi
032000120828     c                   clear                   Campi_Record
032100120828     c                   clear                   Campi_Lunghi
032200120828     c                   clear                   Campi_Numerici
032300120828     c                   clear                   Campi_Decimali
032400120828     c                   movel     ';'           Separa_campi
032500120828     c                   movel     '.'           Decimal_Separ
032600120828      **
032700120828      *  Essendo un file CSV come DPD richiama il FIEU anzichè il TRTCT
032800120828     c                   call      'FIEU00R'
032900120828      * ?     Input
033000120828     c                   parm                    Separa_campi
033100120828     c                   parm                    Decimal_Separ
033200120828     c                   parm                    dati
033300120828      * ?     Output  su schiere di 100 elementi
033400120828     c                   parm                    Nr_campi
033500120828     c                   parm                    Campi_Record                   * i dati
033600120828     c                   parm                    Campi_Lunghi                   * lunghezza dati
033700120828     c                   parm                    Campi_Numerici                 * se numerici
033800120828     c                   parm                    Campi_Decimali                 * quanti decimali
033900120828      **
034000120828     c                   Endsr
034100120829      *----------------------------------------------------------------
034200120829      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO     TUTTO CHIODATO
034300120829      *----------------------------------------------------------------
034400120829     C     TESTAMSG      BEGSR
034500120829      *
034600120829      * Creo la TESTA  chiodando tutti i records poichè non ricevo nulla
034700120829      *  dal loro file.
034800120829      *
034900120829      * Eseguo scrittura dati record ST00   (UNB/BGM/DTM)
035000120829     C                   EXSR      WRST00
035100120829      * Eseguo scrittura dati record ST05   (NAD)
035200120829     C                   EXSR      WRST05
035300120829      *
035400120829     C                   ENDSR
035500120829      *----------------------------------------------------------------
035600120829      *? WRST00 - Scrittura record partner   TUTTO CHIODATO
035700120829      *----------------------------------------------------------------
035800120829     C     WRST00        BEGSR
035900120829      *
036000120829      *  Pulisco dati DS
036100120829     C                   CLEAR                   EDST00
036200120829      *
036300120829     C                   eval        TA0004 = GEL_DE
036400120829     C                   eval        TA0010 = 'ITBAR005'
036500120829     C                   eval        TAA007 = 'ZZ'
036600120829      *  Data/Ora invio messagio:
036700120829     C                   eval        TA0017 = woggi
036800120829     C                   movel     Wora          TA0019
036900120829      *  Codice documento - nome documento
037000120829     C                   eval        TA1001 = '784'
037100120829     C                   eval        TA1000 = 'STATI GEL'
037200120829     C                   eval        TA1004 = '1'
037300120829     C                   eval        TA1225 = '9'
037400120829      *  Data messaggio:
037500120829     C                   eval        TA2005 = '137'
037600120829     C                   movel     woggi         TA2380
037700120829     C                   eval        TA2379 = '102'
037800120829      *--
037900120829      *  Scivo record IFTSTA
038000120829     C                   CLEAR                   trtct13r
038100120829     C                   CLEAR                   WDAT
038200120829     C                   MOVEL     EDST00        WDAT
038300120829     C                   MOVEL     'ST00'        STATPR
038400120829     C                   MOVEL     WDAT          FLATREC
038500120829     C                   WRITE     TRTCT13R                             99
038600120829      *
038700120829     C                   ENDSR
038800120829      *----------------------------------------------------------------
038900120829      *? WRST05 - Scrittura record partner
039000120829      *----------------------------------------------------------------
039100120829     C     WRST05        BEGSR
039200120829      *
039300120829      *  Ship from:
039400120829     C                   CLEAR                   EDST05
039500120829     C                   MOVEL     'SF'          TE3035
039600120829     C                   MOVEL     'GEL GERMANY' TE3036
039700120829     C                   CLEAR                   TRTCT13R
039800120829     C                   CLEAR                   WDAT
039900120829     C                   MOVEL     EDST05        WDAT
040000120829     C                   MOVEL     'ST05'        STATPR
040100120829     C                   MOVEL     WDAT          FLATREC
040200120829     C                   WRITE     TRTCT13R
040300120829      *  Ship to:
040400120829     C                   CLEAR                   EDST05
040500120829     C                   MOVEL     'ST'          TE3035
040600120829     C                   MOVEL     'BRT ITALY'   TE3036
040700120829     C                   CLEAR                   TRTCT13R
040800120829     C                   CLEAR                   WDAT
040900120829     C                   MOVEL     EDST05        WDAT
041000120829     C                   MOVEL     'ST05'        STATPR
041100120829     C                   MOVEL     WDAT          FLATREC
041200120829     C                   WRITE     TRTCT13R                             99
041300120829      *
041400120829     C                   ENDSR
041500120829      *----------------------------------------------------------------
041600120829      *  Decodifica la stringa e la scompone negli appositi CAMPI
041700120829      *----------------------------------------------------------------
041800120828     c     Decod_Record  Begsr
041900120828      *
042000120828      *** ------------------------------------
042100120828      *   Decodifica e scrive il FLAT file   *
042200120828      *** ------------------------------------
042300120828      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
042400120828     c                   clear                   edGELSTS
042500120828     c                   clear                   Msg_Err
042600120828      **
042700120828      * ? Nel 1° campo : c'è sempre il TIPO RECORD
042800120828     C                   z-add     1             sk
042900120828     c                   If        LunDato_sk(sk) >  0
043000120828     c                   eval      TTYPEREC   = %subst(Dato_sk(sk):1:+
043100120828     c                                              LunDato_sk(sk))
043200120828     c                   End
043300120828      **
043400120828      * ? Nel 2° campo : c'è sempre il DEPOT Mittente
043500120828     C                   add       1             sk
043600120828     c                   If        LunDato_sk(sk) >  0
043700120828     c                   eval      TSNDDEPOT  = %subst(Dato_sk(sk):1:+
043800120828     c                                              LunDato_sk(sk))
043900120828     c                   End
044000120828      **
044100120828      * ? Nel 3° campo : BARCODE                  (Obbligatorio)
044200120828      **
044300120828     C                   add       1             sk
044400120828     c                   If        LunDato_sk(sk) =  0
044500120828      * ?-----> Errore
044600120828     c                   eval      tipo_error = 1
044700120828     c                   exsr      Field_Error
044800120828     c                   Else
044900120828     c                   eval      TBARCODE = %subst(Dato_sk(sk):1:+
045000120828     c                                              LunDato_sk(sk))
045100120828     c                   End
045200120828      **
045300120828      * ? Nel 4° campo :  RIFERIMENTO SPEDIZIONE   (Obbligatorio)
045400120828      **
045500120828     C                   clear                   allinea_shpNbr
045600120828     C                   clear                   shp15
045700120828     C                   clear                   T
045800120828      **
045900120828     C                   add       1             sk
046000120828     c                   If        DatoNum_sk(sk) = 'N' or
046100120828     c                             LunDato_sk(sk) =  0
046200120828      * ?-----> Errore
046300120828     c                   eval      tipo_error = 2
046400120828     c                   exsr      Field_Error
046500120828     c                   Else
046600120828      *
046700120828     c                   move      *all'0'       fld30            30
046800120828     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
046900120828     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
047000120829     c                   move      fld30         TSHPNUMBER
047100120828
047200120828      **  x eliminare gli zeri inutili davanti al
047300120828     C*    numero di riferimento della Spedizione
047400120829     C                   MOVE      TSHPNUMBER    allinea_shpNbr
047500120828     C                   MOVEA     allinea_shpNbrshp15
047600120828     C                   DO        15            T
047700120828     C     shp15(T)      IFEQ      '0'
047800120828     c                   eval      shp15(T) = ' '
047900120828     c                   ELSE
048000120828     c                   leave
048100120828     C                   ENDIF
048200120828     C                   ENDDO
048300120828     C                   movea(p)  shp15(T)      allinea_shpNbr
048400120828      **
048500120828     c                   End
048600120828      **
048700120828      * ? Nel 5° campo :  CODICE EVENTO obbligatorio
048800120828      **
048900120828     C                   add       1             sk
049000120828     c                   If        LunDato_sk(sk) =  0
049100120828      * ?-----> Errore
049200120828     c                   eval      tipo_error = 1
049300120828     c                   exsr      Field_Error
049400120828     c                   Else
049500120828     c                   eval      TCODE      = %subst(Dato_sk(sk):1:+
049600120828     c                                              LunDato_sk(sk))
049700120828     c                   End
049800120828      **
049900120828      * ? Nel 6° campo :  TIPO COD EVENTO obbligatorio
050000120828      **
050100120828     C                   add       1             sk
050200120828     c                   If        LunDato_sk(sk) =  0
050300120828      * ?-----> Errore
050400120828     c                   eval      tipo_error = 1
050500120828     c                   exsr      Field_Error
050600120828     c                   Else
050700120828     c                   eval      TTYPECODE  = %subst(Dato_sk(sk):1:+
050800120828     c                                              LunDato_sk(sk))
050900120828     c                   End
051000120828      **
051100120828      * ? Nel 7° campo : DATA EVENTO
051200120828      **
051300120828     C                   add       1             sk
051400120829     c                   If        DatoNum_sk(sk) = 'N' or
051500120829     c                             LunDato_sk(sk) =  0
051600120829      * ?-----> Errore
051700120829     c                   eval      tipo_error = 2
051800120829     c                   exsr      Field_Error
051900120829     c                   Else
052000120829      *
052100120828     c                   move      *all'0'       fld30            30
052200120828     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
052300120828     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
052400120828     c                   move      fld30         TDATE
052500120828     c                   End
052600120828      **
052700120828      * ? Nel 8° campo : ORA EVENTO
052800120828      **
052900120828     C                   add       1             sk
053000120829     c                   If        LunDato_sk(sk) >  0
053100120829     c                   eval      TTIME      = %subst(Dato_sk(sk):1:+
053200120829     c                                              LunDato_sk(sk))
053300120828     c                   End
053400120828      **
053500120828      * ? Nel 9° campo :  NOME DEL CONSEGNATARIO
053600120828      **
053700120828     C                   add       1             sk
053800120828     c                   If        LunDato_sk(sk) >  0
053900120828     c                   eval      TPODNAME   = %subst(Dato_sk(sk):1:+
054000120828     c                                              LunDato_sk(sk))
054100120828     c                   End
054200120828      **
054300120828      * ? Nel 10° campo :   vuoto
054400120828      **
054500120828     C                   add       1             sk
054600120828     c                   If        LunDato_sk(sk) >  0
054700120828     c                   eval      TFREE1     = %subst(Dato_sk(sk):1:+
054800120828     c                                              LunDato_sk(sk))
054900120828     c                   End
055000120828      **
055100120828      * ? Nel 11° campo :  vuoto
055200120828      **
055300120828     C                   add       1             sk
055400120828     c                   If        LunDato_sk(sk) >  0
055500120828     c                   eval      TFREE2     = %subst(Dato_sk(sk):1:+
055600120828     c                                              LunDato_sk(sk))
055700120828     c                   End
055800120828      **
055900120828     c                   endSR
056000120828      * ?------------------------------------------------------------------ */
056100120828      *?      Campi in errore
056200120828      * ?------------------------------------------------------------------ */
056300120828     c     Field_Error   Begsr
056400120828      **
056500120828     C                   eval      se_errore   = 'S'
056600120828      * msg video o in posta
056700120828     c                   If        Msg_Err = *blank
056800120828     c
056900120828     c                   Select
057000120828     c
057100120828     c                   When      Tipo_Error = 1
057200120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
057300120828     C                             ' >> Obblig.- Manca Dato !!'
057400120828     c
057500120828     c                   When      Tipo_Error = 2
057600120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
057700120828     C                             ' >> Obblig.Mancante o NON Numerico !!'
057800120828     c
057900120828     c                   When      Tipo_Error = 3
058000120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
058100120828     C                             ' >> NON Numerico  !!'
058200120828     c
058300120828     c                   When      Tipo_Error = 4
058400120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
058500120828     C                             ' >> Conversione non riuscita !!'
058600120828     c                   EndSL
058700120828     c
058800120828     c                   End
058900120828      * Errore su VINMSG
059000120828     c                   if        Msg_Vin_80 <> ' '
059100120828     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
059200120828     c                                          + %trim(Testa_sk(sk))
059300120828     c                   else
059400120828     C                   eval      Msg_Vin_80 = %trim(Testa_sk(sk))
059500120828     c                   End
059600120828      **
059700120828     c                   Endsr
059800120828      * ?================================================================== */
059900120828     C*? Azzera_MSG   - Azzera dati messaggio
060000120828      * ?================================================================== */
060100120828     C     Scrive_Dett   BEGSR
060200120828      * ?                         --------------
060300120828      **
060400120828      **  Se presente un errore nel record emette una segnalazione msg
060500120828      *     altrimenti scrive il FLAT
060600120829???? c                   if        msg_err = *blank
060700120828      * ?  QUANDO TUTTO OK.
060800120828      * ?              /*---------------------- */
060900120828     c                   exsr      Scrive_Flat
061000120828      * ?              /*---------------------- */
061100121207      *
061200121207     c                   eND
061300120828      *
061400121207???? c                   if        msg_err <>*blank
061500120828      *  Problemi nella decodifica dei campi
061600120829     C                   eval      vinMSG = Msg_ERR
061700120828     c                   eval      errori_in_Wrt = 'S'
061800120829     C                   eval      vinFLG = '2'
061900120829     c                   eval      Almeno_Un_Due ='S'
062000121207      *
062100120828     c                   end
062200120828      *
062300120828     c     end_WRidett   endSR
062400120828      * ?================================================================== */
062500120828     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
062600120828      * ?================================================================== */
062700120828     C     Scrive_FLAT   BEGSR
062800120828      *
062900120828      *  azzera tutte le variabili per poi scrivere le righe del FLAT file
063000120828     c                   clear                   EDSD00
063100120828     c                   clear                   EDSD05
063200120828     c                   clear                   EDSD10
063300120828     c                   clear                   EDSD15
063400120828     c                   clear                   EDSD20
063500120829     c                   clear                   EDSD00_ok         1
063600120829     c                   clear                   EDSD05_ok         1
063700120829     c                   clear                   EDSD10_ok         1
063800120829     c                   clear                   EDSD15_ok         1
063900120829     c                   clear                   EDSD20_ok         1
064000120828      *  CNI
064100120828     C                   CLEAR                   EDSD00
064200120829     C                   MOVE      conta_dettagliDA1490
064300120829     C                   MOVEL     allinea_shpNbrDA1004
064400120828     C                   MOVE      *zeros        DA1312
064500120829     c                   move      'S'           EDSD00_ok
064600120828      * STS/RFF/DTM
064700120829      **     deve trascodificare lo stato da GEL al nostro EEX
064800120828     C                   CLEAR                   EDSD05
064900120829     C                   eval      STATUS_Trovato = *blank
065000120829     c                   exsr      DATI_STS
065100120829      **
065200120829     C                   MOVEL     'AGE'         DB1153
065300120829     C                   MOVEL     allinea_shpNbrDB1154
065400120829     c                   movel     TTIME         Ttime_ora         2
065500120829     c                   move      TTIME         Ttime_minuti      2
065600120829     c                   movel     Ttime_ora     Ttime_4           4
065700120829     c                   move      Ttime_minuti  Ttime_4
065800120829     c                   move      *zeros        position          2 0
065900120829      *
066000120829     c*** ok tutti numeri nel campo time
066100120829     c                   eval      position = %Check(digits:Ttime_4)
066200120829     c                   if        position = 0
066300120829     C                   MOVEL     '203'         DB2379
066400120829     C                   MOVE      Ttime_4       DB2380
066500120829     c                   else
066600120829     C                   MOVEL     '102'         DB2379
066700120829     c                   end
066800120829     C                   MOVEL     '7 '          DB2005
066900120829     C                   MOVEL     TDATE         DB2380
067000120829     c                   move      'S'           EDSD05_ok
067100120828      *  FTX
067200120828     C                   CLEAR                   EDSD10
067300120829     c                   if        DB9011 = '21 ' and TPODNAME <> *blank
067400120828     C                   MOVEL     'AP '         DC3035
067500120829     C                   MOVEL     TPODNAME      DC3036
067600120829     c                   move      'S'           EDSD10_ok
067700120829     c                   end
067800120828      *  FTX
067900120828     C                   CLEAR                   EDSD15
068000120829     c                   if        DB9011 <>'21 ' and TPODNAME <> *blank
068100120829     C                   MOVEL     'DIN'         DD4451
068200120829     C                   MOVEL     TPODNAME      DD4440
068300120829     c                   move      'S'           EDSD15_ok
068400120829     c                   end
068500120829      *  GID e PCI
068600120828     C                   CLEAR                   EDSD20
068700120829     C                   z-add     1             DE1496
068800120829     C                   z-add     1             DE7224
068900120828     C                   MOVEL     'PC'          DE7065
069000120828     C                   MOVEL     '24'          DE4233
069100120829     C                   MOVEL     TBARCODE      DE7102
069200120829     c                   move      'S'           EDSD20_ok
069300120828      *
069400120829     c                   if        TBARCODE       = *blank  or
069500120829     c                             allinea_shpnbr = *blank  or
069600120829     c                             TCODE          = *blank  or
069700120829     c                             TTYPECODE      = *blank  or
069800120829     c                             TDATE          = *zero
069900120829     c                   eval      errori_in_Wrt = 'S'
070000120829     C                   eval      se_errore  = 'S'
070100120829     c                   else
070200120828     c                   move      'S'           Almeno_Uno
070300120829     c                   end
070400120828     C* >>>>>>>>>>>>
070500120829      *    Per errore fa saltare la scrittura
070600120828     C                   if        se_errore  = 'S'
070700120828     c                   move      'S'           err_bloccante
070800120828     c                   goto      Error_Flat
070900120828     c                   end
071000120829      *
071100120829     C*  scrittura dettaglio flat
071200120829     c                   if           EDSD00_ok = 'S'  and
071300120829     c                                EDSD05_ok = 'S'  and
071400120829     c                                EDSD20_ok = 'S'  and
071500120829     C                              STATUS_Trovato <> *blank
071600120829     c                   exsr      WRSD00
071700120829     c                   exsr      WRSD05
071800120829     c                   if           EDSD10_ok = 'S'
071900120829     c                   exsr      WRSD10
072000120829     c                   end
072100120829     c                   if           EDSD15_ok = 'S'
072200120829     c                   exsr      WRSD15
072300120829     c                   end
072400120829     c                   exsr      WRSD20
072500120829     c                   end
072600120829     C*
072700120828     C     Error_Flat    Endsr
072800120828      *----------------------------------------------------------------
072900120828      *? WRSD00 - Scrittura record dettaglio
073000120828      *----------------------------------------------------------------
073100120828     C     WRSD00        BEGSR
073200120828      *
073300120828     C                   CLEAR                   TRTCT13R
073400120828     C                   clear                   WDAT
073500120828     C                   MOVEL     EDSD00        WDAT
073600120828     C                   MOVEL     'SD00'        STATPR
073700120828     C                   MOVEL     WDAT          FLATREC
073800120828     C                   WRITE     TRTCT13R                             99
073900120828      *
074000120828     C                   ENDSR
074100120828      *----------------------------------------------------------------
074200120828      *? WRSD05 - Scrittura record dettaglio
074300120828      *----------------------------------------------------------------
074400120828     C     WRSD05        BEGSR
074500120828      *
074600120828     C                   CLEAR                   TRTCT13R
074700120828     C                   clear                   WDAT
074800120828     C                   MOVEL     EDSD05        WDAT
074900120828     C                   MOVEL     'SD05'        STATPR
075000120828     C                   MOVEL     WDAT          FLATREC
075100120828     C                   WRITE     TRTCT13R                             99
075200120828      *
075300120828     C                   ENDSR
075400120828      *----------------------------------------------------------------
075500120828      *? WRSD10 - Scrittura record dettaglio
075600120828      *----------------------------------------------------------------
075700120828     C     WRSD10        BEGSR
075800120828      *
075900120828     C                   CLEAR                   TRTCT13R
076000120828     C                   clear                   wdat
076100120828     C                   MOVEL     EDSD10        WDAT
076200120828     C                   MOVEL     'SD10'        STATPR
076300120828     C                   MOVEL     WDAT          FLATREC
076400120828     C                   WRITE     TRTCT13R                             99
076500120828      *
076600120828     C                   ENDSR
076700120828      *----------------------------------------------------------------
076800120828      *? WRSD15 - Scrittura record dettaglio
076900120828      *----------------------------------------------------------------
077000120828     C     WRSD15        BEGSR
077100120828      *
077200120828     C                   CLEAR                   TRTCT13R
077300120828     C                   clear                   WDAT
077400120828     C                   MOVEL     EDSD15        WDAT
077500120828     C                   MOVEL     'SD15'        STATPR
077600120828     C                   MOVEL     WDAT          FLATREC
077700120828     C                   WRITE     TRTCT13R                             99
077800120828      *
077900120828     C                   ENDSR
078000120828      *----------------------------------------------------------------
078100120828      *? WRSD20 - Scrittura record dettaglio
078200120828      *----------------------------------------------------------------
078300120828     C     WRSD20        BEGSR
078400120828      *
078500120828     C                   CLEAR                   TRTCT13R
078600120828     C                   clear                   WDAT
078700120828     C                   MOVEL     EDSD20        WDAT
078800120828     C                   MOVEL     'SD20'        STATPR
078900120828     C                   MOVEL     WDAT          FLATREC
079000120828     C                   WRITE     TRTCT13R                             99
079100120828      *
079200120828     C                   ENDSR
079300090213     c*==================================================================*
079400090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
079500090213     c*==================================================================*
079600090213     c     Imp_ADDR_mail begsr
079700090213      *
079800090213     c                   clear                   Indirizzi
079900090213      *
080000090213      *  Lunghezza indirizzi presenti
080100100630     c                   eval      Lunghezza_Indirizzo =
080200100630     c                                       %Len(%TrimR(Mail_Ind))
080300090213     c                   z-add     1             al_char           3 0
080400090213     c                   z-add     1             dal_char          3 0
080500090213     c                   z-add     1             tot_char          3 0
080600090213      *
080700090213     c     successivo    tag
080800090213      *
080900090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
081000090213      *   e aggiunge il dominio Bartolini + il (;) separatore
081100090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
081200090213     c                   eval      tot_char = al_char - dal_char
081300100630     c                   z-add     dal_char      dalCarattere
081400100630     c                   z-add     tot_char      xTOTcaratteri
081500090213      *
081600090213     c                   clear                   Indir_Bartol     40
081700090213     c                   clear                   Indir_Parz       20
081800100630     c                   eval      Indir_Parz =
081900100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
082000100630      *
082100090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
082200090213     c                             %Trim(bart_it)
082300090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
082400090213      *
082500100630     c                   if        al_char = Lunghezza_Indirizzo
082600090213     c                   goto      fine_indmail
082700090213     c                   else
082800090213     c                   eval      dal_char = ( al_char + 1 )
082900090213     c                   goto      successivo
083000090213     c                   end
083100090213      *
083200090213     C     fine_indmail  TAG
083300090213      *
083400090213     C                   ENDSR
083500090213     c*==================================================================*
083600130204      * Manda un Msg x E-mail  ANCHE a CED
083700090213     c*==================================================================*
083800130204     c     Email_con_CED begsr
083900090213      *
084000120828     C*   Alert_mail :
084100090213     C                   movel     *blanks       wrkEml          253
084200090213     C                   movel     *blanks       wrkMsg         5000
084300090213     C                   movel     *blanks       wrkOgg           44
084400090213      *
084500090213     C* Valorizzo i campi della e-m@ail - indirizzo
084600090213     C                   eval      wrkEml = Indirizzi
084700090213     C                   eval      wrkOgg = Oggetto
084800090213     C                   eval      wrkMsg = Messaggio
084900130204      *
085000130204      *   il 1/2/2013 Fabrizio spiega che nel TIS701C viene chiodato l'invio a CED
085100130204     c*      ed il TRTCT00R2 invia con il TIS701C
085200130204     C                   call(e)   'TRTCT00R2'
085300130204     C                   parm                    wrkEml
085400130204     C                   parm                    wrkOgg
085500130204     C                   parm                    wrkMsg
085600090213     C*
085700090213     C                   ENDSR
085800130204     c*==================================================================*
085900130204      * Manda un Msg x E-mail  NON a CED
086000130204     c*==================================================================*
086100130204     c     Invio_Mail    begsr
086200130204      *
086300130204     C*   Alert_mail :
086400130204     C                   movel     *blanks       wrkEml          253
086500130204     C                   movel     *blank        wrkEmlcc        253
086600130204     C                   movel     *blanks       wrkMsg         5000
086700130204     C                   movel     *blanks       wrkOgg           44
086800130204     C                   movel     *blanks       wrkesito          1
086900130204      *
087000130204     C* Valorizzo i campi della e-m@ail - indirizzo
087100130204     C                   eval      wrkEml = Indirizzi
087200130204     C                   eval      wrkOgg = Oggetto
087300130204     C                   eval      wrkMsg = Messaggio
087400130204      *
087500130204      *  Se richiesta di NICOLA di cambiare solo questi pgm di GEL per NON mandare a CED
087600130204      *   le mails: il TIS701C1 al contrario del TIS701C (il 1/2/2013 Fabrizio spiega che
087700130204      *   nel TIS701C viene chiodato l'invio a CED)
087800130204     C                   call(e)   'TRTCT00R5'
087900130204     C                   parm                    wrkEml          253
088000130204     C                   parm                    wrkEmlCc        253
088100130204     C                   parm                    wrkOgg           44
088200130204     C                   parm                    wrkMsg         5000
088300130204     C                   parm                    wrkesito          1
088400130204     C*
088500130204     C                   ENDSR
088600120829      * ?================================================================== */
088700120829     C*? Imposta informazioni sullo STATO di Consegna
088800120829      * ?================================================================== */
088900120829     C     DATI_STS      BEGSR
089000120829     C*
089100120829     C*  Se abilitato alla conversione dei codici STATUS occorre passare per
089200120829     C*   la tabella di conversione e riportare i nostri codici STANDARD
089300120829     C*    al posto di quelli ricevuti dal Partner.
089400120829     c                   if        Converte_Cod_Status ='S'
089500120829      **
089600120829     c                   if         Cod_IdentKEY = *BLANK
089700120829     c                   eval        STSUNBCLI = GEL_DE
089800120829     c                   else
089900120829     c                   eval        STSUNBCLI = Cod_IdentKEY
090000120829     c                   end
090100120829      **
090200120829     c                   eval        STSCODSTS = TtypeCODE
090300120829     c                   eval        STSSUBSTS = Tcode
090400120829      **
090500120829     c     key_STS       chain     edSTS01l
090600120829     c                   if        %Found(edSTS01l)
090700120829     C                   eval      STATUS_Trovato = 'S'
090800120829     c                   eval           DB9011    = STSCODSTSb
090900120829     c                   eval           DB9013    = STSSUBSTSb
091000121207     c                   else
091100121207      * deve sempre scrivere adesso per passare l'informazione anche se errata
091200121207      * e non trascodificata così il programma che importa gli status emette una
091300121207      * stampa che in Filiale evidenzia questo tipo di errori ricevuti.
091400121207     C                   eval      STATUS_Trovato = 'S'
091500121207     c                   eval      MSG_ERR ='STATO non Concordato' + Tcode +
091600121207     c                             '/' + TtypeCODE
091700121207     c                   eval           DB9011    = TtypeCODE
091800121207     c                   eval           DB9013    = Tcode
091900121207     c                   end
092000120829      **
092100120829     c                   end
092200120829      **
092300120829     c     end_STS       endSR
092400120829     C*================================================================
092500120829      *   DEFINIZIONE CHIAVI
092600120829     C*================================================================
092700120829     C     *INZSR        BEGSR
092800120829      *
092900120829     c     Key_STS       KLIST
093000120829     c                   KFLD                    STSUNBCLI
093100120829     c                   KFLD                    STSCODSTS
093200120829     c                   KFLD                    STSSUBSTS
093300120829      *
093400120829     c     *ENTRY        PLIST
093500120829     C                   parm                    esito             1
093600120829      *
093700120829     C* RECUPERO DATI DELL'UTENTE
093800120829     C                   Z-ADD     1             CODUT             1 0
093900120829     C                   CALL      'XPARUT'
094000120829     C                   PARM                    UTEDSE0F
094100120829     C                   MOVEL     RAGUT         RSUT             20
094200120829     C* RICERCA CAPOCONTI
094300120829     C                   DO        50            X
094400120829     C                   MOVE      TCU(X)        TCUDS
094500120829     C     F56           IFEQ      'CG'
094600120829     C     F34           ANDEQ     '01'
094700120829     C                   Z-ADD     KCU(X)        KCI               4 0
094800120829     C                   END
094900120829     C                   END
095000120829     C*
095100120829     c                   movel(p)  'EDPAB'       User_Msg         10
095200120829     C*
095300120829     C* IMPOSTO A ZERO VARIABILI DI PGM
095400120829     c                   move      *blank        Snd_Msg_alert     1
095500120829     C*
095600120829     C* RECUPERO DATA E ORA
095700120829     C                   TIME                    Ora_Data         14 0
095800120829     C                   MOVE      Ora_Data      G02DAT
095900120829     C                   MOVE      Ora_Data      WDATA8            8 0
096000120829     C                   MOVE      WDATA8        Anno2             2 0
096100120829     C                   MOVEL     WDATA8        DATA              6 0
096200120829     C                   MOVE      Anno2         DATA
096300120829     C                   MOVEL     *BLANK        G02ERR
096400120829     C                   CALL      'XSRDA8'
096500120829     C                   PARM                    WLBDA8
096600120829     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
096700120829     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
096800120829     C                   TIME                    WORA              6 0
096900120829      *  carica testate campi
097000120829      *
097100120829     c                   clear                   Testa_Sk
097200120829     c                   z-add     0             num               3 0
097300120829     c                   z-add     0             x
097400120829     c                   eval      num = %elem(recT)
097500120829     c                   do        num           x
097600120829     c                   movel     recT(x)       Testa_Sk(x)
097700120829     c                   enddo
097800120829      * Recupero data e ora
097900120829     C                   TIME                    W0140
098000120829      * UDATE IN GGMMAAAA
098100120829     C                   MOVE      W0140         DataGGMMAAA
098200120829      * UDATE IN AAAAMMGG
098300120829     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
098400120829     C     *iso          MOVEL     DATA_eur      dateu
098500120829      **
098600120829      *  azzera tutte le variabili generali del messaggio
098700120829     c                   clear                   Almeno_Uno        1
098800120829     c                   clear                   Almeno_Un_Due     1
098900120829     c                   eval      esito  = '0'
099000120829      **
099100120829      *   Contatore dei dettagli
099200120829     c                   eval      conta_dettagli = 0
099300121210      * Un errore nel dettaglio
099400121210     c                   clear                   errori_in_Wrt     1
099500120829      *
099600120829      * ?              /*--------------------------- */
099700120829     c                   exsr      CARICA_TAB_PT
099800120829      * ?              /*--------------------------- */
099900120829      *
100000120829     C                   ENDSR
100100120829      * ?------------------------------------------------------------------ */
100200120829      *?    CARICA TABELLE SU SCHIERE
100300120829      * ?------------------------------------------------------------------ */
100400120829     C     CARICA_TAB_PT BEGSR
100500120829      *
100600120829     c     TBkey         klist
100700120829     c                   kfld                    TABCOD
100800120829     c                   kfld                    TABKEY
100900120829      *
101000120829      * Caricamento Tabella Partner esteri
101100120829     C                   Z-ADD     0             X
101200120829     C                   MOVEL     'PT'          TABCOD
101300120829     C                   MOVEL(p)  GEL_DE        TABKEY
101400120829     C     TBkey         CHAIN     EDTAB01L
101500120829     C                   if        %Found(EDTAB01L)
101600120829     C                   MOVEL     TABUNI        edidsPT
101700120829     C                   END
101800120829      *
101900120829     C                   MOVEL     'PU'          TABCOD
102000120829     C     TBkey         CHAIN     EDTAB01L
102100120829     C                   if        %Found(EDTAB01L)
102200120829     C                   MOVEL     TABUNI        edidsPU
102300120829     C                   END
102400120829      *
102500120829     C                   MOVEL     'PV'          TABCOD
102600120829     C     TBkey         CHAIN     EDTAB01L
102700120829     C                   if        %Found(EDTAB01L)
102800120829     C                   MOVEL     TABUNI        edidsPV
102900120829     C                   END
103000120829      *
103100120829     C                   MOVEL     'PZ'          TABCOD
103200120829     C     TBkey         CHAIN     EDTAB01L
103300120829     C                   if        %Found(EDTAB01L)
103400120829     C                   MOVEL     TABUNI        edidsPZ
103500120829     C                   END
103600120829      *
103700120829     C                   MOVEL     'PW'          TABCOD
103800120829     C     TBkey         CHAIN     EDTAB01L
103900120829     C                   if        %Found(EDTAB01L)
104000120829     C                   MOVEL     TABUNI        edidsPW
104100120829     C                   END
104200120829      *
104300120829      * ?imposta se deve passare per una tabella di conversione STATUS
104400120829      *    poichè inviano differenti STATUS codes rispetto al nostro Standard
104500120829     c                   eval       Converte_Cod_Status = §pvCNVSTS
104600120829     c                   eval       Cod_IdentKEY = §pvIDENT
104700120829      *
104800120829      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
104900120829      *  ricevuti:
105000120829     c                   movel     §PVema        mail_ind         44
105100120829      *
105200120829      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
105300120829     c                   if        mail_ind <> *blank
105400120829     c                   exsr      imp_ADDR_mail
105500120829     c                   end
105600120829      **
105700120829     C                   ENDSR
105800120829      * ?------------------------------------------------------------------ */
105900120829      *?      X non bloccare in nessun caso il traduttore CLIENTI
106000120829      * ?------------------------------------------------------------------ */
106100120829     C     *pssr         BEGSR
106200120829     C
106300120829     C                   eval      esito = '2'
106400120829???? C                   EVAL      Oggetto ='Problemi UPLOAD stati GEL'
106500120829     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
106600120829     C                             STATI consegna messaggio -
106700120829     C                             ricevuto errato su UPLOAD.'
106800120829     c                   if        §PVinv <> 'N'
106900130204     c                   exsr      Email_con_CED
107000120829     c                   end
107100120829     C
107200120829     C                   ENDSR
107300100712     c*==================================================================*
107400120828** Nomi campi record STATI
107500120828TTYPEREC    (TIPO REC -->(T))
107600120828TSNDDEPOT   (DEPOT MITTENTE)
107700120828TBARCODE    (BARCODE)
107800120828TSHPNUMBER  (RIFERIMENTO SPEDIZIONE)
107900120828TCODE       (CODICE EVENTO GEL)
108000120828TTYPECODE   (TIPO ED1-ED2 ATR O DELIVERY)
108100120828TDATE       (DATA EVENTO)
108200120828TTIME       (ORA EVENTO)
108300120828TPODNAME    (NOME CONSEGNATARIO)
108400120828TFREE1      (FREE)
108500120828TFREE2      (FREE)
