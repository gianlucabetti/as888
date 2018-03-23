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
018700060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
018800060612     C*
018900060612     C                   if        not %open(tivin00r)
019000060612     C                   open      tivin00r
019100060612     C                   endif
019200090205      **
019300120828     c     *start        setll     Tivin00r
019400120828      * ?              /*---------------------- */
019500120828     c                   EXSR      LEGGE_TIVIN_R
019600120828      * ?              /*---------------------- */
019700120828     c                   dow             EoFTivin <> 'S'
019800120828
019900120828      * solo i record sflaggati da rielaborare
020000120828     c                   IF              vinFLG = *blank
020100120828      *  Sempre Record OK
020200120828     C                   eval      vinFLG = '1'
020300120828      *** >>>>>>
020400120828     c                   IF              vinDTA <> *blank
020500120828      *** >>>>>>
020600120828      **  Ad ogni inizio Dettaglio incrementa il contatore dei dettagli
020700120829     c                   If                    conta_dettagli = 0
020800120829     c                   eval        conta_dettagli = conta_dettagli + 1
020900120828      *
021000120828      * SOLO la prima volta Scrivere la Testata
021100120829     c                   If           conta_dettagli = 1
021200120828     C                   EXSR      TESTAMSG
021300120828     c                   end
021400120828     c                   endIF
021500120828      *
021600120828      ** Decodifica record a campi variabili
021700120828      * ?              /*---------------------- */
021800120828     c                   exsr      Decod_Record
021900120828      * ?              /*---------------------- */
022000120828
022100120828      * ?              /*---------------------- */
022200120828     c                   exsr      Scrive_Dett
022300120828      * ?              /*---------------------- */
022400120828
022500120828      *  x errore bloccante nella composizione del VAB/VAT
022600120828     c                   if             err_bloccante ='S'
022700120828     C                   eval      vinFLG = '2'
022800120828     c                   eval      esito  = '2'
022900120828???? C                   EVAL      Oggetto ='Problemi UPLOAD STATI GEL'
023000120828     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
023100120828     C                             STATI su Bolle export - messaggio -
023200120828     C                             errato ricevuto su UPLOAD.'
023300120828     c                   if            §PVinv <> 'N'
023400120828     c                   exsr      invio_mail
023500120828     c                   end
023600120828     c                   end
023700120828      *** >>>>>>
023800120828     c                   endIF
023900120828      *** >>>>>>
024000120828      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
024100120828      *     viene inviato un erroneo messaggio
024200120828     c                   update    Tivin000
024300120828      *** ++++++
024400120828     c                   endIF
024500120828
024600120828      * ?              /*---------------------- */
024700120828     c                   EXSR      LEGGE_TIVIN_R
024800120828      * ?              /*---------------------- */
024900120828     C                   ENDdo
025000120828      **
025100060612      * ?              /*---------------------- */
025200060612      *
025300060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
025400090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
025500090225     c                             esito ='0'
025600060710     C                   eval      esito ='0'
025700090225      *
025800090224      *
025900090213     c                   if        almeno_un_due ='S'
026000090213     C                   eval      esito ='1'
026100060614     C                   endif
026200120828      *
026300100723     C                   end
026400100708      * chiude TIVIN
026500100708     C                   if        %open(tivin00r)
026600100708     C                   close     tivin00r
026700100708     C                   endif
026800121210      *
026900121210      * se almeno un errore invia la mail di controllo
027000121210     c                   if        errori_in_Wrt = 'S'
027100121210     c                   if        §PVinv <> 'N'
027200121210???? C                   EVAL      Oggetto ='Problemi UPLOAD stati GEL'
027300121210     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
027400121210     C                             STATI consegna messaggio -
027500121210     C                             ricevuto errato su UPLOAD.'
027600121210     c                   exsr      invio_mail
027700121210     c                   end
027800121210     C                   endif
027900060614      *
028000121106     c                   COMMIT
028100060612     c                   SETON                                        LR
028200060612      * ?================================================================== */
028300100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
028400100709      * ?------------------------------------------------------------------ */
028500100709     C     LEGGE_TIVIN_R BEGSR
028600100709      *
028700100709     c                   clear                   EoFTivin
028800100709      *  Lettura sequenziale
028900100709     c                   read      Tivin00r
029000120828      ***
029100100709     c                   if        %Eof(tivin00r)
029200100709     c                   move      'S'           EoFTivin
029300100709     c                   else
029400120828      *
029500120828      ** Controlli formali sui campi
029600120828     c                   clear                   se_errore
029700120828     C                   clear                   Msg_Vin_80
029800120828     c                   clear                   err_bloccante     1
029900100709      *
030000100709      * ?  imposta il tipo di segmento:
030100100709     c                   clear                   dati
030200100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
030300120829     c                   eval      dati = %trim(dati) + ';'
030400120828      * ?              /*---------------------- */
030500120828     c                   exsr      Split_Rec
030600120828      * ?              /*---------------------- */
030700120828      **
030800120828     c                   end
030900100709      *
031000100709     C                   ENDSR
031100120828      * ?------------------------------------------------------------------ */
031200120828      *?      Suddivide i campi della riga con carattere divisorio (;)
031300120828      * ?------------------------------------------------------------------ */
031400120828     c     Split_Rec     Begsr
031500120828      **
031600120828     c                   clear                   Nr_campi
031700120828     c                   clear                   Campi_Record
031800120828     c                   clear                   Campi_Lunghi
031900120828     c                   clear                   Campi_Numerici
032000120828     c                   clear                   Campi_Decimali
032100120828     c                   movel     ';'           Separa_campi
032200120828     c                   movel     '.'           Decimal_Separ
032300120828      **
032400120828      *  Essendo un file CSV come DPD richiama il FIEU anzichè il TRTCT
032500120828     c                   call      'FIEU00R'
032600120828      * ?     Input
032700120828     c                   parm                    Separa_campi
032800120828     c                   parm                    Decimal_Separ
032900120828     c                   parm                    dati
033000120828      * ?     Output  su schiere di 100 elementi
033100120828     c                   parm                    Nr_campi
033200120828     c                   parm                    Campi_Record                   * i dati
033300120828     c                   parm                    Campi_Lunghi                   * lunghezza dati
033400120828     c                   parm                    Campi_Numerici                 * se numerici
033500120828     c                   parm                    Campi_Decimali                 * quanti decimali
033600120828      **
033700120828     c                   Endsr
033800120829      *----------------------------------------------------------------
033900120829      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO     TUTTO CHIODATO
034000120829      *----------------------------------------------------------------
034100120829     C     TESTAMSG      BEGSR
034200120829      *
034300120829      * Creo la TESTA  chiodando tutti i records poichè non ricevo nulla
034400120829      *  dal loro file.
034500120829      *
034600120829      * Eseguo scrittura dati record ST00   (UNB/BGM/DTM)
034700120829     C                   EXSR      WRST00
034800120829      * Eseguo scrittura dati record ST05   (NAD)
034900120829     C                   EXSR      WRST05
035000120829      *
035100120829     C                   ENDSR
035200120829      *----------------------------------------------------------------
035300120829      *? WRST00 - Scrittura record partner   TUTTO CHIODATO
035400120829      *----------------------------------------------------------------
035500120829     C     WRST00        BEGSR
035600120829      *
035700120829      *  Pulisco dati DS
035800120829     C                   CLEAR                   EDST00
035900120829      *
036000120829     C                   eval        TA0004 = GEL_DE
036100120829     C                   eval        TA0010 = 'ITBAR005'
036200120829     C                   eval        TAA007 = 'ZZ'
036300120829      *  Data/Ora invio messagio:
036400120829     C                   eval        TA0017 = woggi
036500120829     C                   movel     Wora          TA0019
036600120829      *  Codice documento - nome documento
036700120829     C                   eval        TA1001 = '784'
036800120829     C                   eval        TA1000 = 'STATI GEL'
036900120829     C                   eval        TA1004 = '1'
037000120829     C                   eval        TA1225 = '9'
037100120829      *  Data messaggio:
037200120829     C                   eval        TA2005 = '137'
037300120829     C                   movel     woggi         TA2380
037400120829     C                   eval        TA2379 = '102'
037500120829      *--
037600120829      *  Scivo record IFTSTA
037700120829     C                   CLEAR                   trtct13r
037800120829     C                   CLEAR                   WDAT
037900120829     C                   MOVEL     EDST00        WDAT
038000120829     C                   MOVEL     'ST00'        STATPR
038100120829     C                   MOVEL     WDAT          FLATREC
038200120829     C                   WRITE     TRTCT13R                             99
038300120829      *
038400120829     C                   ENDSR
038500120829      *----------------------------------------------------------------
038600120829      *? WRST05 - Scrittura record partner
038700120829      *----------------------------------------------------------------
038800120829     C     WRST05        BEGSR
038900120829      *
039000120829      *  Ship from:
039100120829     C                   CLEAR                   EDST05
039200120829     C                   MOVEL     'SF'          TE3035
039300120829     C                   MOVEL     'GEL GERMANY' TE3036
039400120829     C                   CLEAR                   TRTCT13R
039500120829     C                   CLEAR                   WDAT
039600120829     C                   MOVEL     EDST05        WDAT
039700120829     C                   MOVEL     'ST05'        STATPR
039800120829     C                   MOVEL     WDAT          FLATREC
039900120829     C                   WRITE     TRTCT13R
040000120829      *  Ship to:
040100120829     C                   CLEAR                   EDST05
040200120829     C                   MOVEL     'ST'          TE3035
040300120829     C                   MOVEL     'BRT ITALY'   TE3036
040400120829     C                   CLEAR                   TRTCT13R
040500120829     C                   CLEAR                   WDAT
040600120829     C                   MOVEL     EDST05        WDAT
040700120829     C                   MOVEL     'ST05'        STATPR
040800120829     C                   MOVEL     WDAT          FLATREC
040900120829     C                   WRITE     TRTCT13R                             99
041000120829      *
041100120829     C                   ENDSR
041200120829      *----------------------------------------------------------------
041300120829      *  Decodifica la stringa e la scompone negli appositi CAMPI
041400120829      *----------------------------------------------------------------
041500120828     c     Decod_Record  Begsr
041600120828      *
041700120828      *** ------------------------------------
041800120828      *   Decodifica e scrive il FLAT file   *
041900120828      *** ------------------------------------
042000120828      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
042100120828     c                   clear                   edGELSTS
042200120828     c                   clear                   Msg_Err
042300120828      **
042400120828      * ? Nel 1° campo : c'è sempre il TIPO RECORD
042500120828     C                   z-add     1             sk
042600120828     c                   If        LunDato_sk(sk) >  0
042700120828     c                   eval      TTYPEREC   = %subst(Dato_sk(sk):1:+
042800120828     c                                              LunDato_sk(sk))
042900120828     c                   End
043000120828      **
043100120828      * ? Nel 2° campo : c'è sempre il DEPOT Mittente
043200120828     C                   add       1             sk
043300120828     c                   If        LunDato_sk(sk) >  0
043400120828     c                   eval      TSNDDEPOT  = %subst(Dato_sk(sk):1:+
043500120828     c                                              LunDato_sk(sk))
043600120828     c                   End
043700120828      **
043800120828      * ? Nel 3° campo : BARCODE                  (Obbligatorio)
043900120828      **
044000120828     C                   add       1             sk
044100120828     c                   If        LunDato_sk(sk) =  0
044200120828      * ?-----> Errore
044300120828     c                   eval      tipo_error = 1
044400120828     c                   exsr      Field_Error
044500120828     c                   Else
044600120828     c                   eval      TBARCODE = %subst(Dato_sk(sk):1:+
044700120828     c                                              LunDato_sk(sk))
044800120828     c                   End
044900120828      **
045000120828      * ? Nel 4° campo :  RIFERIMENTO SPEDIZIONE   (Obbligatorio)
045100120828      **
045200120828     C                   clear                   allinea_shpNbr
045300120828     C                   clear                   shp15
045400120828     C                   clear                   T
045500120828      **
045600120828     C                   add       1             sk
045700120828     c                   If        DatoNum_sk(sk) = 'N' or
045800120828     c                             LunDato_sk(sk) =  0
045900120828      * ?-----> Errore
046000120828     c                   eval      tipo_error = 2
046100120828     c                   exsr      Field_Error
046200120828     c                   Else
046300120828      *
046400120828     c                   move      *all'0'       fld30            30
046500120828     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
046600120828     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
046700120829     c                   move      fld30         TSHPNUMBER
046800120828
046900120828      **  x eliminare gli zeri inutili davanti al
047000120828     C*    numero di riferimento della Spedizione
047100120829     C                   MOVE      TSHPNUMBER    allinea_shpNbr
047200120828     C                   MOVEA     allinea_shpNbrshp15
047300120828     C                   DO        15            T
047400120828     C     shp15(T)      IFEQ      '0'
047500120828     c                   eval      shp15(T) = ' '
047600120828     c                   ELSE
047700120828     c                   leave
047800120828     C                   ENDIF
047900120828     C                   ENDDO
048000120828     C                   movea(p)  shp15(T)      allinea_shpNbr
048100120828      **
048200120828     c                   End
048300120828      **
048400120828      * ? Nel 5° campo :  CODICE EVENTO obbligatorio
048500120828      **
048600120828     C                   add       1             sk
048700120828     c                   If        LunDato_sk(sk) =  0
048800120828      * ?-----> Errore
048900120828     c                   eval      tipo_error = 1
049000120828     c                   exsr      Field_Error
049100120828     c                   Else
049200120828     c                   eval      TCODE      = %subst(Dato_sk(sk):1:+
049300120828     c                                              LunDato_sk(sk))
049400120828     c                   End
049500120828      **
049600120828      * ? Nel 6° campo :  TIPO COD EVENTO obbligatorio
049700120828      **
049800120828     C                   add       1             sk
049900120828     c                   If        LunDato_sk(sk) =  0
050000120828      * ?-----> Errore
050100120828     c                   eval      tipo_error = 1
050200120828     c                   exsr      Field_Error
050300120828     c                   Else
050400120828     c                   eval      TTYPECODE  = %subst(Dato_sk(sk):1:+
050500120828     c                                              LunDato_sk(sk))
050600120828     c                   End
050700120828      **
050800120828      * ? Nel 7° campo : DATA EVENTO
050900120828      **
051000120828     C                   add       1             sk
051100120829     c                   If        DatoNum_sk(sk) = 'N' or
051200120829     c                             LunDato_sk(sk) =  0
051300120829      * ?-----> Errore
051400120829     c                   eval      tipo_error = 2
051500120829     c                   exsr      Field_Error
051600120829     c                   Else
051700120829      *
051800120828     c                   move      *all'0'       fld30            30
051900120828     c                   eval      %subst(fld30:31-LunDato_sk(sk):LunDato_sk(sk)
052000120828     c                             )= %subst(Dato_sk(sk):1:LunDato_sk(sk))
052100120828     c                   move      fld30         TDATE
052200120828     c                   End
052300120828      **
052400120828      * ? Nel 8° campo : ORA EVENTO
052500120828      **
052600120828     C                   add       1             sk
052700120829     c                   If        LunDato_sk(sk) >  0
052800120829     c                   eval      TTIME      = %subst(Dato_sk(sk):1:+
052900120829     c                                              LunDato_sk(sk))
053000120828     c                   End
053100120828      **
053200120828      * ? Nel 9° campo :  NOME DEL CONSEGNATARIO
053300120828      **
053400120828     C                   add       1             sk
053500120828     c                   If        LunDato_sk(sk) >  0
053600120828     c                   eval      TPODNAME   = %subst(Dato_sk(sk):1:+
053700120828     c                                              LunDato_sk(sk))
053800120828     c                   End
053900120828      **
054000120828      * ? Nel 10° campo :   vuoto
054100120828      **
054200120828     C                   add       1             sk
054300120828     c                   If        LunDato_sk(sk) >  0
054400120828     c                   eval      TFREE1     = %subst(Dato_sk(sk):1:+
054500120828     c                                              LunDato_sk(sk))
054600120828     c                   End
054700120828      **
054800120828      * ? Nel 11° campo :  vuoto
054900120828      **
055000120828     C                   add       1             sk
055100120828     c                   If        LunDato_sk(sk) >  0
055200120828     c                   eval      TFREE2     = %subst(Dato_sk(sk):1:+
055300120828     c                                              LunDato_sk(sk))
055400120828     c                   End
055500120828      **
055600120828     c                   endSR
055700120828      * ?------------------------------------------------------------------ */
055800120828      *?      Campi in errore
055900120828      * ?------------------------------------------------------------------ */
056000120828     c     Field_Error   Begsr
056100120828      **
056200120828     C                   eval      se_errore   = 'S'
056300120828      * msg video o in posta
056400120828     c                   If        Msg_Err = *blank
056500120828     c
056600120828     c                   Select
056700120828     c
056800120828     c                   When      Tipo_Error = 1
056900120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
057000120828     C                             ' >> Obblig.- Manca Dato !!'
057100120828     c
057200120828     c                   When      Tipo_Error = 2
057300120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
057400120828     C                             ' >> Obblig.Mancante o NON Numerico !!'
057500120828     c
057600120828     c                   When      Tipo_Error = 3
057700120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
057800120828     C                             ' >> NON Numerico  !!'
057900120828     c
058000120828     c                   When      Tipo_Error = 4
058100120829     C                   eval      Msg_Err =  %trim(Testa_sk(sk)) +
058200120828     C                             ' >> Conversione non riuscita !!'
058300120828     c                   EndSL
058400120828     c
058500120828     c                   End
058600120828      * Errore su VINMSG
058700120828     c                   if        Msg_Vin_80 <> ' '
058800120828     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
058900120828     c                                          + %trim(Testa_sk(sk))
059000120828     c                   else
059100120828     C                   eval      Msg_Vin_80 = %trim(Testa_sk(sk))
059200120828     c                   End
059300120828      **
059400120828     c                   Endsr
059500120828      * ?================================================================== */
059600120828     C*? Azzera_MSG   - Azzera dati messaggio
059700120828      * ?================================================================== */
059800120828     C     Scrive_Dett   BEGSR
059900120828      * ?                         --------------
060000120828      **
060100120828      **  Se presente un errore nel record emette una segnalazione msg
060200120828      *     altrimenti scrive il FLAT
060300120829???? c                   if        msg_err = *blank
060400121207     c************       if        TTYPECODE = 'ED1' or
060500121207     c************                 TTYPECODE = 'ED2'
060600120828      * ?  QUANDO TUTTO OK.
060700120828      * ?              /*---------------------- */
060800120828     c                   exsr      Scrive_Flat
060900120828      * ?              /*---------------------- */
061000121207     c************       else
061100121207      *   Tipo Record
061200121207     C************       eval      Msg_ERR = 'Tipo Record :' + TTYPECODE + 'NON-
061300121207     C************                  concordato'
061400121207      ************
061500121207     c************       end
061600121207      *
061700121207     c                   eND
061800120828      *
061900121207???? c                   if        msg_err <>*blank
062000121207     c********           if        TTYPECODE = 'ED1' or
062100121207     c**********                   TTYPECODE = 'ED2'
062200120828      *  Problemi nella decodifica dei campi
062300120829     C                   eval      vinMSG = Msg_ERR
062400120828     c                   eval      errori_in_Wrt = 'S'
062500120829     C                   eval      vinFLG = '2'
062600120829     c                   eval      Almeno_Un_Due ='S'
062700121211     c*********          eval      esito = '1'
062800121211     c*********          end
062900121207      *
063000120828     c                   end
063100120828      *
063200120828     c     end_WRidett   endSR
063300120828      * ?================================================================== */
063400120828     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
063500120828      * ?================================================================== */
063600120828     C     Scrive_FLAT   BEGSR
063700120828      *
063800120828      *  azzera tutte le variabili per poi scrivere le righe del FLAT file
063900120828     c                   clear                   EDSD00
064000120828     c                   clear                   EDSD05
064100120828     c                   clear                   EDSD10
064200120828     c                   clear                   EDSD15
064300120828     c                   clear                   EDSD20
064400120829     c                   clear                   EDSD00_ok         1
064500120829     c                   clear                   EDSD05_ok         1
064600120829     c                   clear                   EDSD10_ok         1
064700120829     c                   clear                   EDSD15_ok         1
064800120829     c                   clear                   EDSD20_ok         1
064900120828      *  CNI
065000120828     C                   CLEAR                   EDSD00
065100120829     C                   MOVE      conta_dettagliDA1490
065200120829     C                   MOVEL     allinea_shpNbrDA1004
065300120828     C                   MOVE      *zeros        DA1312
065400120829     c                   move      'S'           EDSD00_ok
065500120828      * STS/RFF/DTM
065600120829      **     deve trascodificare lo stato da GEL al nostro EEX
065700120828     C                   CLEAR                   EDSD05
065800120829     C                   eval      STATUS_Trovato = *blank
065900120829     c                   exsr      DATI_STS
066000120829      **
066100120829     C                   MOVEL     'AGE'         DB1153
066200120829     C                   MOVEL     allinea_shpNbrDB1154
066300120829     c                   movel     TTIME         Ttime_ora         2
066400120829     c                   move      TTIME         Ttime_minuti      2
066500120829     c                   movel     Ttime_ora     Ttime_4           4
066600120829     c                   move      Ttime_minuti  Ttime_4
066700120829     c                   move      *zeros        position          2 0
066800120829      *
066900120829     c*** ok tutti numeri nel campo time
067000120829     c                   eval      position = %Check(digits:Ttime_4)
067100120829     c                   if        position = 0
067200120829     C                   MOVEL     '203'         DB2379
067300120829     C                   MOVE      Ttime_4       DB2380
067400120829     c                   else
067500120829     C                   MOVEL     '102'         DB2379
067600120829     c                   end
067700120829     C                   MOVEL     '7 '          DB2005
067800120829     C                   MOVEL     TDATE         DB2380
067900120829     c                   move      'S'           EDSD05_ok
068000120828      *  FTX
068100120828     C                   CLEAR                   EDSD10
068200120829     c                   if        DB9011 = '21 ' and TPODNAME <> *blank
068300120828     C                   MOVEL     'AP '         DC3035
068400120829     C                   MOVEL     TPODNAME      DC3036
068500120829     c                   move      'S'           EDSD10_ok
068600120829     c                   end
068700120828      *  FTX
068800120828     C                   CLEAR                   EDSD15
068900120829     c                   if        DB9011 <>'21 ' and TPODNAME <> *blank
069000120829     C                   MOVEL     'DIN'         DD4451
069100120829     C                   MOVEL     TPODNAME      DD4440
069200120829     c                   move      'S'           EDSD15_ok
069300120829     c                   end
069400120829      *  GID e PCI
069500120828     C                   CLEAR                   EDSD20
069600120829     C                   z-add     1             DE1496
069700120829     C                   z-add     1             DE7224
069800120828     C                   MOVEL     'PC'          DE7065
069900120828     C                   MOVEL     '24'          DE4233
070000120829     C                   MOVEL     TBARCODE      DE7102
070100120829     c                   move      'S'           EDSD20_ok
070200120828      *
070300120829     c                   if        TBARCODE       = *blank  or
070400120829     c                             allinea_shpnbr = *blank  or
070500120829     c                             TCODE          = *blank  or
070600120829     c                             TTYPECODE      = *blank  or
070700120829     c                             TDATE          = *zero
070800120829     c                   eval      errori_in_Wrt = 'S'
070900120829     C                   eval      se_errore  = 'S'
071000120829     c                   else
071100120828     c                   move      'S'           Almeno_Uno
071200120829     c                   end
071300120828     C* >>>>>>>>>>>>
071400120829      *    Per errore fa saltare la scrittura
071500120828     C                   if        se_errore  = 'S'
071600120828     c                   move      'S'           err_bloccante
071700120828     c                   goto      Error_Flat
071800120828     c                   end
071900120829      *
072000120829     C*  scrittura dettaglio flat
072100120829     c                   if           EDSD00_ok = 'S'  and
072200120829     c                                EDSD05_ok = 'S'  and
072300120829     c                                EDSD20_ok = 'S'  and
072400120829     C                              STATUS_Trovato <> *blank
072500120829     c                   exsr      WRSD00
072600120829     c                   exsr      WRSD05
072700120829     c                   if           EDSD10_ok = 'S'
072800120829     c                   exsr      WRSD10
072900120829     c                   end
073000120829     c                   if           EDSD15_ok = 'S'
073100120829     c                   exsr      WRSD15
073200120829     c                   end
073300120829     c                   exsr      WRSD20
073400120829     c                   end
073500120829     C*
073600120828     C     Error_Flat    Endsr
073700120828      *----------------------------------------------------------------
073800120828      *? WRSD00 - Scrittura record dettaglio
073900120828      *----------------------------------------------------------------
074000120828     C     WRSD00        BEGSR
074100120828      *
074200120828     C                   CLEAR                   TRTCT13R
074300120828     C                   clear                   WDAT
074400120828     C                   MOVEL     EDSD00        WDAT
074500120828     C                   MOVEL     'SD00'        STATPR
074600120828     C                   MOVEL     WDAT          FLATREC
074700120828     C                   WRITE     TRTCT13R                             99
074800120828      *
074900120828     C                   ENDSR
075000120828      *----------------------------------------------------------------
075100120828      *? WRSD05 - Scrittura record dettaglio
075200120828      *----------------------------------------------------------------
075300120828     C     WRSD05        BEGSR
075400120828      *
075500120828     C                   CLEAR                   TRTCT13R
075600120828     C                   clear                   WDAT
075700120828     C                   MOVEL     EDSD05        WDAT
075800120828     C                   MOVEL     'SD05'        STATPR
075900120828     C                   MOVEL     WDAT          FLATREC
076000120828     C                   WRITE     TRTCT13R                             99
076100120828      *
076200120828     C                   ENDSR
076300120828      *----------------------------------------------------------------
076400120828      *? WRSD10 - Scrittura record dettaglio
076500120828      *----------------------------------------------------------------
076600120828     C     WRSD10        BEGSR
076700120828      *
076800120828     C                   CLEAR                   TRTCT13R
076900120828     C                   clear                   wdat
077000120828     C                   MOVEL     EDSD10        WDAT
077100120828     C                   MOVEL     'SD10'        STATPR
077200120828     C                   MOVEL     WDAT          FLATREC
077300120828     C                   WRITE     TRTCT13R                             99
077400120828      *
077500120828     C                   ENDSR
077600120828      *----------------------------------------------------------------
077700120828      *? WRSD15 - Scrittura record dettaglio
077800120828      *----------------------------------------------------------------
077900120828     C     WRSD15        BEGSR
078000120828      *
078100120828     C                   CLEAR                   TRTCT13R
078200120828     C                   clear                   WDAT
078300120828     C                   MOVEL     EDSD15        WDAT
078400120828     C                   MOVEL     'SD15'        STATPR
078500120828     C                   MOVEL     WDAT          FLATREC
078600120828     C                   WRITE     TRTCT13R                             99
078700120828      *
078800120828     C                   ENDSR
078900120828      *----------------------------------------------------------------
079000120828      *? WRSD20 - Scrittura record dettaglio
079100120828      *----------------------------------------------------------------
079200120828     C     WRSD20        BEGSR
079300120828      *
079400120828     C                   CLEAR                   TRTCT13R
079500120828     C                   clear                   WDAT
079600120828     C                   MOVEL     EDSD20        WDAT
079700120828     C                   MOVEL     'SD20'        STATPR
079800120828     C                   MOVEL     WDAT          FLATREC
079900120828     C                   WRITE     TRTCT13R                             99
080000120828      *
080100120828     C                   ENDSR
080200090213     c*==================================================================*
080300090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
080400090213     c*==================================================================*
080500090213     c     Imp_ADDR_mail begsr
080600090213      *
080700090213     c                   clear                   Indirizzi
080800090213      *
080900090213      *  Lunghezza indirizzi presenti
081000100630     c                   eval      Lunghezza_Indirizzo =
081100100630     c                                       %Len(%TrimR(Mail_Ind))
081200090213     c                   z-add     1             al_char           3 0
081300090213     c                   z-add     1             dal_char          3 0
081400090213     c                   z-add     1             tot_char          3 0
081500090213      *
081600090213     c     successivo    tag
081700090213      *
081800090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
081900090213      *   e aggiunge il dominio Bartolini + il (;) separatore
082000090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
082100090213     c                   eval      tot_char = al_char - dal_char
082200100630     c                   z-add     dal_char      dalCarattere
082300100630     c                   z-add     tot_char      xTOTcaratteri
082400090213      *
082500090213     c                   clear                   Indir_Bartol     40
082600090213     c                   clear                   Indir_Parz       20
082700100630     c                   eval      Indir_Parz =
082800100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
082900100630      *
083000090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
083100090213     c                             %Trim(bart_it)
083200090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
083300090213      *
083400100630     c                   if        al_char = Lunghezza_Indirizzo
083500090213     c                   goto      fine_indmail
083600090213     c                   else
083700090213     c                   eval      dal_char = ( al_char + 1 )
083800090213     c                   goto      successivo
083900090213     c                   end
084000090213      *
084100090213     C     fine_indmail  TAG
084200090213      *
084300090213     C                   ENDSR
084400090213     c*==================================================================*
084500130204      * Manda un Msg x E-mail  ANCHE a CED
084600090213     c*==================================================================*
084700130204     c     Email_con_CED begsr
084800090213      *
084900120828     C*   Alert_mail :
085000090213     C                   movel     *blanks       wrkEml          253
085100090213     C                   movel     *blanks       wrkMsg         5000
085200090213     C                   movel     *blanks       wrkOgg           44
085300090213      *
085400090213     C* Valorizzo i campi della e-m@ail - indirizzo
085500090213     C                   eval      wrkEml = Indirizzi
085600090213     C                   eval      wrkOgg = Oggetto
085700090213     C                   eval      wrkMsg = Messaggio
085800130204      *
085900130204      *   il 1/2/2013 Fabrizio spiega che nel TIS701C viene chiodato l'invio a CED
086000130204     c*      ed il TRTCT00R2 invia con il TIS701C
086100130204     C                   call(e)   'TRTCT00R2'
086200130204     C                   parm                    wrkEml
086300130204     C                   parm                    wrkOgg
086400130204     C                   parm                    wrkMsg
086500090213     C*
086600090213     C                   ENDSR
086700130204     c*==================================================================*
086800130204      * Manda un Msg x E-mail  NON a CED
086900130204     c*==================================================================*
087000130204     c     Invio_Mail    begsr
087100130204      *
087200130204     C*   Alert_mail :
087300130204     C                   movel     *blanks       wrkEml          253
087400130204     C                   movel     *blank        wrkEmlcc        253
087500130204     C                   movel     *blanks       wrkMsg         5000
087600130204     C                   movel     *blanks       wrkOgg           44
087700130204     C                   movel     *blanks       wrkesito          1
087800130204      *
087900130204     C* Valorizzo i campi della e-m@ail - indirizzo
088000130204     C                   eval      wrkEml = Indirizzi
088100130204     C                   eval      wrkOgg = Oggetto
088200130204     C                   eval      wrkMsg = Messaggio
088300130204      *
088400130204      *  Se richiesta di NICOLA di cambiare solo questi pgm di GEL per NON mandare a CED
088500130204      *   le mails: il TIS701C1 al contrario del TIS701C (il 1/2/2013 Fabrizio spiega che
088600130204      *   nel TIS701C viene chiodato l'invio a CED)
088700130204     C                   call(e)   'TRTCT00R5'
088800130204     C                   parm                    wrkEml          253
088900130204     C                   parm                    wrkEmlCc        253
089000130204     C                   parm                    wrkOgg           44
089100130204     C                   parm                    wrkMsg         5000
089200130204     C                   parm                    wrkesito          1
089300130204     C*
089400130204     C                   ENDSR
089500120829      * ?================================================================== */
089600120829     C*? Imposta informazioni sullo STATO di Consegna
089700120829      * ?================================================================== */
089800120829     C     DATI_STS      BEGSR
089900120829     C*
090000120829     C*  Se abilitato alla conversione dei codici STATUS occorre passare per
090100120829     C*   la tabella di conversione e riportare i nostri codici STANDARD
090200120829     C*    al posto di quelli ricevuti dal Partner.
090300120829     c                   if        Converte_Cod_Status ='S'
090400120829      **
090500120829     c                   if         Cod_IdentKEY = *BLANK
090600120829     c                   eval        STSUNBCLI = GEL_DE
090700120829     c                   else
090800120829     c                   eval        STSUNBCLI = Cod_IdentKEY
090900120829     c                   end
091000120829      **
091100120829     c                   eval        STSCODSTS = TtypeCODE
091200120829     c                   eval        STSSUBSTS = Tcode
091300120829      **
091400120829     c     key_STS       chain     edSTS01l
091500120829     c                   if        %Found(edSTS01l)
091600120829     C                   eval      STATUS_Trovato = 'S'
091700120829     c                   eval           DB9011    = STSCODSTSb
091800120829     c                   eval           DB9013    = STSSUBSTSb
091900121207     c                   else
092000121207      * deve sempre scrivere adesso per passare l'informazione anche se errata
092100121207      * e non trascodificata così il programma che importa gli status emette una
092200121207      * stampa che in Filiale evidenzia questo tipo di errori ricevuti.
092300121207     C                   eval      STATUS_Trovato = 'S'
092400121207     c                   eval      MSG_ERR ='STATO non Concordato' + Tcode +
092500121207     c                             '/' + TtypeCODE
092600121207     c                   eval           DB9011    = TtypeCODE
092700121207     c                   eval           DB9013    = Tcode
092800121207     c                   end
092900120829      **
093000120829     c                   end
093100120829      **
093200120829     c     end_STS       endSR
093300120829     C*================================================================
093400120829      *   DEFINIZIONE CHIAVI
093500120829     C*================================================================
093600120829     C     *INZSR        BEGSR
093700120829      *
093800120829     c     Key_STS       KLIST
093900120829     c                   KFLD                    STSUNBCLI
094000120829     c                   KFLD                    STSCODSTS
094100120829     c                   KFLD                    STSSUBSTS
094200120829      *
094300120829     c     *ENTRY        PLIST
094400120829     C                   parm                    esito             1
094500120829      *
094600120829     C* RECUPERO DATI DELL'UTENTE
094700120829     C                   Z-ADD     1             CODUT             1 0
094800120829     C                   CALL      'XPARUT'
094900120829     C                   PARM                    UTEDSE0F
095000120829     C                   MOVEL     RAGUT         RSUT             20
095100120829     C* RICERCA CAPOCONTI
095200120829     C                   DO        50            X
095300120829     C                   MOVE      TCU(X)        TCUDS
095400120829     C     F56           IFEQ      'CG'
095500120829     C     F34           ANDEQ     '01'
095600120829     C                   Z-ADD     KCU(X)        KCI               4 0
095700120829     C                   END
095800120829     C                   END
095900120829     C*
096000120829     c                   movel(p)  'EDPAB'       User_Msg         10
096100120829     C*
096200120829     C* IMPOSTO A ZERO VARIABILI DI PGM
096300120829     c                   move      *blank        Snd_Msg_alert     1
096400120829     C*
096500120829     C* RECUPERO DATA E ORA
096600120829     C                   TIME                    Ora_Data         14 0
096700120829     C                   MOVE      Ora_Data      G02DAT
096800120829     C                   MOVE      Ora_Data      WDATA8            8 0
096900120829     C                   MOVE      WDATA8        Anno2             2 0
097000120829     C                   MOVEL     WDATA8        DATA              6 0
097100120829     C                   MOVE      Anno2         DATA
097200120829     C                   MOVEL     *BLANK        G02ERR
097300120829     C                   CALL      'XSRDA8'
097400120829     C                   PARM                    WLBDA8
097500120829     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
097600120829     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
097700120829     C                   TIME                    WORA              6 0
097800120829      *  carica testate campi
097900120829      *
098000120829     c                   clear                   Testa_Sk
098100120829     c                   z-add     0             num               3 0
098200120829     c                   z-add     0             x
098300120829     c                   eval      num = %elem(recT)
098400120829     c                   do        num           x
098500120829     c                   movel     recT(x)       Testa_Sk(x)
098600120829     c                   enddo
098700120829      * Recupero data e ora
098800120829     C                   TIME                    W0140
098900120829      * UDATE IN GGMMAAAA
099000120829     C                   MOVE      W0140         DataGGMMAAA
099100120829      * UDATE IN AAAAMMGG
099200120829     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
099300120829     C     *iso          MOVEL     DATA_eur      dateu
099400120829      **
099500120829      *  azzera tutte le variabili generali del messaggio
099600120829     c                   clear                   Almeno_Uno        1
099700120829     c                   clear                   Almeno_Un_Due     1
099800120829     c                   eval      esito  = '0'
099900120829      **
100000120829      *   Contatore dei dettagli
100100120829     c                   eval      conta_dettagli = 0
100200121210      * Un errore nel dettaglio
100300121210     c                   clear                   errori_in_Wrt     1
100400120829      *
100500120829      * ?              /*--------------------------- */
100600120829     c                   exsr      CARICA_TAB_PT
100700120829      * ?              /*--------------------------- */
100800120829      *
100900120829     C                   ENDSR
101000120829      * ?------------------------------------------------------------------ */
101100120829      *?    CARICA TABELLE SU SCHIERE
101200120829      * ?------------------------------------------------------------------ */
101300120829     C     CARICA_TAB_PT BEGSR
101400120829      *
101500120829     c     TBkey         klist
101600120829     c                   kfld                    TABCOD
101700120829     c                   kfld                    TABKEY
101800120829      *
101900120829      * Caricamento Tabella Partner esteri
102000120829     C                   Z-ADD     0             X
102100120829     C                   MOVEL     'PT'          TABCOD
102200120829     C                   MOVEL(p)  GEL_DE        TABKEY
102300120829     C     TBkey         CHAIN     EDTAB01L
102400120829     C                   if        %Found(EDTAB01L)
102500120829     C                   MOVEL     TABUNI        edidsPT
102600120829     C                   END
102700120829      *
102800120829     C                   MOVEL     'PU'          TABCOD
102900120829     C     TBkey         CHAIN     EDTAB01L
103000120829     C                   if        %Found(EDTAB01L)
103100120829     C                   MOVEL     TABUNI        edidsPU
103200120829     C                   END
103300120829      *
103400120829     C                   MOVEL     'PV'          TABCOD
103500120829     C     TBkey         CHAIN     EDTAB01L
103600120829     C                   if        %Found(EDTAB01L)
103700120829     C                   MOVEL     TABUNI        edidsPV
103800120829     C                   END
103900120829      *
104000120829     C                   MOVEL     'PZ'          TABCOD
104100120829     C     TBkey         CHAIN     EDTAB01L
104200120829     C                   if        %Found(EDTAB01L)
104300120829     C                   MOVEL     TABUNI        edidsPZ
104400120829     C                   END
104500120829      *
104600120829     C                   MOVEL     'PW'          TABCOD
104700120829     C     TBkey         CHAIN     EDTAB01L
104800120829     C                   if        %Found(EDTAB01L)
104900120829     C                   MOVEL     TABUNI        edidsPW
105000120829     C                   END
105100120829      *
105200120829      * ?imposta se deve passare per una tabella di conversione STATUS
105300120829      *    poichè inviano differenti STATUS codes rispetto al nostro Standard
105400120829     c                   eval       Converte_Cod_Status = §pvCNVSTS
105500120829     c                   eval       Cod_IdentKEY = §pvIDENT
105600120829      *
105700120829      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
105800120829      *  ricevuti:
105900120829     c                   movel     §PVema        mail_ind         44
106000120829      *
106100120829      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
106200120829     c                   if        mail_ind <> *blank
106300120829     c                   exsr      imp_ADDR_mail
106400120829     c                   end
106500120829      **
106600120829     C                   ENDSR
106700120829      * ?------------------------------------------------------------------ */
106800120829      *?      X non bloccare in nessun caso il traduttore CLIENTI
106900120829      * ?------------------------------------------------------------------ */
107000120829     C     *pssr         BEGSR
107100120829     C
107200120829     C                   eval      esito = '2'
107300120829???? C                   EVAL      Oggetto ='Problemi UPLOAD stati GEL'
107400120829     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
107500120829     C                             STATI consegna messaggio -
107600120829     C                             ricevuto errato su UPLOAD.'
107700120829     c                   if        §PVinv <> 'N'
107800130204     c                   exsr      Email_con_CED
107900120829     c                   end
108000120829     C
108100120829     C                   ENDSR
108200100712     c*==================================================================*
108300120828** Nomi campi record STATI
108400120828TTYPEREC    (TIPO REC -->(T))
108500120828TSNDDEPOT   (DEPOT MITTENTE)
108600120828TBARCODE    (BARCODE)
108700120828TSHPNUMBER  (RIFERIMENTO SPEDIZIONE)
108800120828TCODE       (CODICE EVENTO GEL)
108900120828TTYPECODE   (TIPO ED1-ED2 ATR O DELIVERY)
109000120828TDATE       (DATA EVENTO)
109100120828TTIME       (ORA EVENTO)
109200120828TPODNAME    (NOME CONSEGNATARIO)
109300120828TFREE1      (FREE)
109400120828TFREE2      (FREE)
