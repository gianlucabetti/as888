000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500111214      *  Da UPLOAD   DUCROS                                                     *
000600111214????  *  TRASCODIFICA : MESSAGGI EDI  -  STATI BOLLE EXPORT   IFTSTA vers D96A  *
000700120119????  *   il programma riporta il messaggio EDI sul FLAT RCVIFTSTA              *
000800120119????  *   riconvertendo la logica ricevuta con la nostra standard del flat file *
000900120119????  *   inoltre tramite il file di conversione degli stati EDSTS00F riporta i *
001000120119????  *   codici ricevuti ai nostri codici standard EEX.                        *
001100120119????  *   In pratica converte il file ricevuto come se avesse ricevuto un file  *
001200120119????  *   seguendo le regole impostre dal nostro traduttore di EEX, generando   *
001300120119????  *   un flat file leggibile dalle nostre abituali procedure di EEX.        *
001400060612      **?************************************************************************
001500100712      * Il pgm crea:  RCVIFTSTA                                                 *
001600060609      **?************************************************************************
001700100723     Ftivin00r  uF   E             DISK    INFDS(VINRECDS) usropn
001800100722     FEDTAB01L  IF   E           K DISK
001900111215     FEDSTS01L  IF   E           K DISK
002000111214     FtrtcT14W  O  A E           K DISK    INFDS(SRECDS)
002100100701      *----------------------------------------------------*
002200100701      * Stringhe per conversione alfanumerico/numerico N.Documento
002300100701     D CA              S             78    DIM(1) CTDATA PERRCD(1)
002400100701     D CN              S             78    DIM(1) CTDATA PERRCD(1)
002500940321      *----------------------------------------------------*
002600090209      * numero relativo di record
002700090209     D SRECDS          DS
002800090209     D  NRREC                397    400B 0
002900090209      *
003000090213      * numero relativo di record
003100090213     D VINRECDS        DS
003200090213     D  VNREC                397    400B 0
003300100701      * ---------------------------------------------------
003400100701     D KPJBA         E DS
003500100701     D     svkpjbu     s                   like(kpjbu)
003600100701     D UTEDSE0F      E DS
003700100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003800100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003900100701      * Ds scomposzione tipo capoconti
004000100701     D TCUDS           DS
004100100701     D  F34                    3      4
004200100701     D  F56                    5      6
004300100722      *----------------------------------------------------*
004400100701     D EDIDSPT       E DS
004500100701     D EDIDSPU       E DS
004600100701     D EDIDSPV       E DS
004700100722     D EDIDSMS       E DS
004800100701      **
004900100701     D WLBDA8          DS
005000100701     D  G02DAT                 1      8  0
005100100701     D  G02INV                 9     16  0
005200100701     D  G02ERR                17     17
005300100701     D  G02TGI                18     22  0
005400100712     D*---------------------------------------------------------------------*
005500100712     D* DS
005600100712     D*---------------------------------------------------------------------*
005700100712     D* .. per scompozione dati da spedire a seconda del tipo record
005800100712     D EDST00        E DS                  EXTNAME(EDST00DS)
005900100712     D EDST05        E DS                  EXTNAME(EDST05DS)
006000100712     D EDST10        E DS                  EXTNAME(EDST10DS)
006100100712     D EDST15        E DS                  EXTNAME(EDST15DS)
006200100712     D EDSD00        E DS                  EXTNAME(EDSD00DS)
006300100712     D EDSD05        E DS                  EXTNAME(EDSD05DS)
006400100712     D EDSD10        E DS                  EXTNAME(EDSD10DS)
006500100712     D EDSD15        E DS                  EXTNAME(EDSD15DS)
006600100712     D EDSD20        E DS                  EXTNAME(EDSD20DS)
006700100712     D*---------------------------------------------------------------------*
006800100712     D* .. per reperimento tipo record di EDIFTSTA da scrivere
006900100712     D*                    .. 5 - 8  : tipo record
007000100712     D WDAT            DS          1950
007100100712     D  STAPRG                 1      4  0
007200100712     D  STATPR                 5      8
007300100712     D*---------------------------------------------------------------------*
007400090213      *
007500100701      *----------------------------------------------------*
007600090205      **  Elenco DS di tutti i Segmenti da tradurre
007700100701      *----------------------------------------------------*
007800100712???? D edS00UNA      E DS
007900100712???? D edS00UNB      E DS
008000100712???? D edS00UNH      E DS
008100100712???? D edS00UNT      E DS
008200100712???? D edS00UNZ      E DS
008300091019???? D edS00BGM      E DS
008400100316???? D edS00CNI      E DS
008500091019???? D edS00DTM      E DS
008600100628???? D edS00FTX      E DS
008700091019???? D edS00GID      E DS
008800100316???? D edS00LOC      E DS
008900091019???? D edS00NAD      E DS
009000091019???? D edS00PCI      E DS
009100091019???? D edS00RFF      E DS
009200100712???? D edS00STS      E DS
009300100722      **---------------------------------------------------*
009400090209     D Valori_inErr    ds
009500090209     D  SKout_Errori                  1    DIM(50)
009600100630      **
009700090209     D Descr_Errore    ds
009800090209     D  SKout_DesErr                 50    DIM(50)
009900090205      *----------------------------------------------------*
010000091016     D EoFTivin        s              1
010100091016      *----------------------------------------------------*
010200090205     D Tipo_seg        S              3
010300100721     D Trovato_seg     S              1
010400100716      *
010500100716     d keyUNBCLI       s             35
010600100716     d keyTIPOMSG      s              6
010700100716     d keyVERSION      s              3
010800100716     d keyRELEASE      s              3
010900100716     d keyAGENCY       s              3
011000100716     d keyASSOCIA      s              6
011100100716      *
011200100721     D DsGenerica      s           2048
011300090209     D segmento        s           2048
011400090209     D Errore          s              1
011500100722      *------------------------------------------
011600000223     D W0140           S             14  0
011700991129     D WORA            S              6  0
011800100701     D DataGGMMAAA     S              8  0
011900100701     D DATEU           S              8  0
012000100701     D DATA_eur        S               D   DATFMT(*eur)
012100100723     D Work_Data__35   S             35
012200100723     D Work_Format_3   S              3
012300100722      *------------------------------------------
012400100708     D UNT_record      s                   LIKE(vnREC)
012500100708     D IniDET_RECord   s                   LIKE(vnREC)
012600100708     D FinDET_RECord   s                   LIKE(vnREC)
012700090213     D Last_RECord     s                   LIKE(vnREC)
012800110110     D UNZ_SEGMENTO    s              1
012900100722      *------------------------------------------
013000100722     D nad3035SF       s                   LIKE(NAD3035)
013100100723     D nad3036SF       s                   LIKE(NAD30361)
013200100722     D nad3039SF       s                   LIKE(NAD3039)
013300100722     D nad3035ST       s                   LIKE(NAD3035)
013400100723     D nad3036ST       s                   LIKE(NAD30361)
013500100722     D nad3039ST       s                   LIKE(NAD3039)
013600100722      *
013700100722     D LOC3227t1       s                   LIKE(LOC3227)
013800100722     D LOC3225t1       s                   LIKE(LOC3225)
013900100722     D LOC3227t2       s                   LIKE(LOC3227)
014000100722     D LOC3225t2       s                   LIKE(LOC3225)
014100100722     D dtm2005te       s                   LIKE(dtm2005)
014200100722     D dtm2380te       s                   LIKE(dtm2380)
014300100722     D dtm2379te       s                   LIKE(dtm2379)
014400100722     D rff1153te       s                   LIKE(rff1153)
014500100722     D rff1154te       s                   LIKE(rff1154)
014600100318      *------------------------------------------
014700100318      **  Definizione Qualificatori
014800100318      *------------------------------------------
014900100705???? D Data_senza_ora...
015000100628???? D                 s              3    INZ('102')
015100100705???? D Data_con_ora...
015200100628???? D                 s              3    INZ('203')
015300100705???? D Data_con_i_secondi...
015400100705???? D                 s              3    INZ('204')
015500100316      *---------------------------------------------------------------------*
015600100316      **  segmenti Identificativi parti specifiche del messaggio
015700100316      *---------------------------------------------------------------------*
015800100701???? D Segm_Inizio_Messaggio...
015900100701???? D                 s              3    INZ('UNB')
016000100701???? D Segm_Testata_Dettagli...
016100100701???? D                 s              3    INZ('UNH')
016200100701???? D Segm_Fine_messaggio...
016300100316???? D                 s              3    INZ('UNZ')
016400100722???? D Segm_Inizio_Dettaglio...
016500100722???? D                 s              3    INZ('CNI')
016600100722???? D Segm_Fine_Dettagli...
016700100722???? D                 s              3    INZ('UNT')
016800100722???? D Segm_Data_Dettaglio...
016900100722???? D                 s              3    INZ('DTM')
017000100722???? D Segm_Address_Dettaglio...
017100100722???? D                 s              3    INZ('NAD')
017200100722???? D Segm_Text_Dettaglio...
017300100722???? D                 s              3    INZ('FTX')
017400100722???? D Segm_Parcels_Dettaglio...
017500100722???? D                 s              3    INZ('PCI')
017600111215???? D Segm_Numero_Colli...
017700111215???? D                 s              3    INZ('GID')
017800111215???? D Segm_Codici_STATUS...
017900111215???? D                 s              3    INZ('STS')
018000100701???? D seg_imprevisto...
018100100701???? D                 s              1
018200120104???? D STATUS_Trovato...
018300120104???? D                 s              1
018400100723???? D eseguire_ROLBACK...
018500100723???? D                 s              1
018600100726???? D Forza_Uscita...
018700100726???? D                 s              1
018800111214???? D Segm_FTX_Firma...
018900111214???? D                 s              1
019000120105???? D Segm_GID_inviato...
019100120105???? D                 s              1
019200090209      *---------------------------------------------------------------------*
019300100701???? D Contatore_Dettagli...
019400100701???? D                 s              5s 0 INZ(0)
019500100701      *---------------------------------------------------------------------*
019600100628      *
019700100701      *---------------------------------------------------------------------*
019800100630     d UNB_diWork      s                   like(UNB0004)
019900100628     d  UNB_Mittente   s                   like(UNB0004)
020000100712     d UNB_parziale    s              1    inz('N')
020100100712     d UNB_Parz        s             35    inz(' ')
020200100712     d UNB_Generico    s             35    inz(' ')
020300100712     d UNB_NrTrasmiss  s             14    inz(' ')
020400100712      *
020500100701      * Tabella partner
020600121105     D PT_key          S             35    DIM(200)
020700121105     D PT_Parz         S             35    DIM(%elem(PT_key))
020800121105     D PT_KSC          S              7    DIM(%elem(PT_key))
020900121105     D PT_uni          S             90    DIM(%elem(PT_key))
021000121105     D PU_uni          S             90    DIM(%elem(PT_key))
021100121105     D PV_uni          S             90    DIM(%elem(PT_key))
021200121105     D TRUL0SDS      E DS
021300100701      *
021400100701     d PT_KeyXsav      s              3  0 inz(0)
021500100701     D PT_trovata      s              1    inz('N')
021600100701     D PU_key          S             35
021700100701     D PV_key          S             35
021800100701     D Nr_PT           S              3  0 INZ
021900100701      *
022000111214     d  Converte_Cod_Status...
022100111214     d                 s              1A
022200111221     d  Cod_IdentKEY   s             10A
022300111214      *
022400100702     d  CNI_Identificativo_Bolla...
022500100702     d                 s             35A
022600090209      *---------------------------------------------------------------------*
022700090209      * Schiere
022800090209      *---------------------------------------------------------------------*
022900090209      * Nr. documento alfanumerico da decodificare
023000090209     D NDA             S              1    DIM(35)
023100100630      *
023200090209      * Nr. documento alfanumerico da già decodificato
023300090209     D NDN             S              1    DIM(15)
023400060608      **?------------------------------------------------------------------ */
023500090209      *  Invio E-mail
023600090209     D Indirizzi       s            253
023700090209     D Oggetto         s             44
023800090209     D Messaggio       s           5000
023900090209      *
024000090209     d   DSmail        ds
024100090209     D Mail_Ind                      44
024200090209     d   IND_char                     1    dim(44)
024300090209      *
024400100630     D Lunghezza_Indirizzo...
024500100630     D                 s              5u 0
024600090209      *
024700100630     D dalCarattere    s              5u 0
024800100630     D xTOTcaratteri   s              5u 0
024900090209      *
025000090209     C*---------------------------------------------------------------*
025100090209     D MsgDSP          S            256                                         Testo generico
025200090209     C*---------------------------------------------------------------*
025300090209     D SndMg01         S             10                                         Message type
025400090209     D                                     INZ('*INFO')
025500090209     D SndMg02         S             10                                         Deluvery mode
025600090209     D                                     INZ('*BREAK')
025700090209     D SndMg03         S            256                                         Message text
025800090209     D SndMg04         S             10I 0                                      Length of text
025900090209     D                                     INZ(%SIZE(SndMg03))
026000090209     D SndMg05         S             10                                         User profile
026100090209     D                                     DIM(299)
026200090209     D SndMg06         S             10I 0                                      Number of user
026300090209     D SndMg07         S             10I 0                                      Message sent indic.
026400090209     D SndMg08         S             10I 0                                      Function requested
026500090209     D SndMg10         S              1                                         Show display
026600090209     D                                     INZ('N')
026700090209     D SndMg11         S             20                                         Qualified MSGQ name
026800090209     D SndMg12         S              4                                         Name type indicator
026900090209     D                                     INZ('*USR')
027000111214      * indice di schiera
027100090209     D Jx              S              5I 0
027200090209      *
027300090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
027400090209      *
027500090209     d bart_it         C                   CONST('@Bartolini.it;')
027600090209     d CED_Bart        C                   CONST('CED@Bartolini.it;')
027700060612      * ?================================================================== */
027800060612      * ?   * Campi x decodifica * (INPUT  del Record)
027900100319      * ?================================================================== */
028000090130     D  Dati           s           2048
028100100722      *
028200060612     D  se_errore      s              1    inz(' ')
028300060612      * ?* ------------------------------------------------------ *
028400060710     D Digits          C                   '0123456789'
028500091019     D*------------------
028600091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
028700091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
028800060612      * ?================================================================== */
028900060612      *   Ciclo principale
029000090205      * ?================================================================== */
029100060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
029200060612     C*
029300060612     C                   if        not %open(tivin00r)
029400060612     C                   open      tivin00r
029500060612     C                   endif
029600060612      **
029700060612      * ?------------------------------------------------------------------ */
029800100712     C*  Controllo dati arrivati
029900060612      * ?------------------------------------------------------------------ */
030000060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
030100060612      * ?- controllando che la trasmissione sia completa.
030200090205      **
030300060612      * ?              /*---------------------- */
030400090205     c                   goto      nonFare_adesso
030500060612     c                   exsr      check_Trasm
030600090205     c     nonFare_adessotag
030700060612      * ?              /*---------------------- */
030800090205      **
030900060613      **  Errore di trasmissione x tutti i records
031000060613      **   --> file in errore
031100090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
031200100712     c                   if        se_errore = 'S'
031300090205     c                   exsr      Msg_errato
031400060612     c                   else
031500060614      **
031600060612      * ?------------------------------------------------------------------ */
031700100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
031800060612      * ?------------------------------------------------------------------ */
031900100722      *
032000060612      * ?              /*---------------------- */
032100060612     c                   exsr      Importa_Msg
032200060612      * ?              /*---------------------- */
032300060614     c                   end
032400060612      *
032500100723     C                   if          eseguire_ROLBACK = 'S'
032600100723     C*
032700100723     c                   ROLBK
032800100723     C*
032900100723     C                   eval      esito = '2'
033000100723???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
033100100723     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
033200100723     C                             STATI IFTSTA - messaggio -
033300100723     C                             EDI ricevuto errato su UPLOAD.'
033400100723     c                   if        §PVinv <> 'N'
033500100723     c                   exsr      invio_mail
033600100723     c                   end
033700100723     C
033800100723      *
033900100723     c                   else
034000060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
034100090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
034200090225     c                             esito ='0'
034300060710     C                   eval      esito ='0'
034400090225      *
034500090225     c                   COMMIT
034600090224      *
034700090213     c                   if        almeno_un_due ='S'
034800090213     C                   eval      esito ='1'
034900060614     C                   endif
035000090213     C                   endif
035100100723     C                   end
035200100708      * chiude TIVIN
035300100708     C                   if        %open(tivin00r)
035400100708     C                   close     tivin00r
035500100708     C                   endif
035600060614      *
035700060612     c                   SETON                                        LR
035800060612      * ?================================================================== */
035900100708      *? Importa i records della trasmissione
036000060612      * ?------------------------------------------------------------------ */
036100060612     c     Importa_Msg   Begsr
036200060614
036300100726     c                   clear                   Forza_Uscita
036400060612     c     *start        setll     Tivin00r
036500100708
036600100708     c                   EXSR      LEGGE_TIVIN_R
036700100708     c                   dow       EoFTivin <> 'S'
036800060614
036900060614      * solo i record sflaggati da rielaborare
037000110107     c                   IF        vinFLG = *blank
037100110107      *** ++++++
037200090211      *  Sempre Record OK
037300090211      ** Controlli formali sui campi
037400090211     C                   eval      vinFLG = '1'
037500060612     c                   clear                   se_errore
037600110107      *** >>>>>>
037700110107     c                   IF        vinDTA <> *blank
037800110107      *** >>>>>>
037900100723      **
038000100723      **  Ad ogni inizio Dettaglio incrementa il contatore dei dettagli
038100100723     c                   If           tipo_seg = Segm_Inizio_Dettaglio
038200100723     c                   eval        Contatore_Dettagli = Contatore_Dettagli + 1
038300100723      **
038400100723      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
038500100723     c                   eval      FinDET_RECord = vnrec
038600100723      **
038700100723     c*  Scrivere la Testata
038800100723      * SOLO ALLA SCRITTURA del 1° dettaglio
038900100723     c                   If           Contatore_Dettagli = 1
039000100723     C                   EXSR      TESTAMSG
039100100723     c                   end
039200100723     c                   endIF
039300060612
039400060612      ** Decodifica record a campi variabili
039500060612      * ?              /*---------------------- */
039600090205     c                   exsr      Decod_Segmento
039700090209      * ?              /*---------------------- */
039800060612
039900060621      *  x errore bloccante nella composizione del VAB/VAT
040000060621     c                   if        err_bloccante ='S'
040100060621     C                   eval      vinFLG = '2'
040200090211     c                   eval      esito  = '2'
040300100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
040400090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
040500100712     C                             STATI su Bolle export - messaggio -
040600090603     C                             con UNB non presente in tabella PT controlla-
040700090603     C                             re EDI ricevuto su UPLOAD.'
040800100319     c                   if        §PVinv <> 'N'
040900100319     c                   exsr      invio_mail
041000060621     c                   end
041100100319     c                   end
041200060612
041300110107      *** >>>>>>
041400110107     c                   endIF
041500110107      *** >>>>>>
041600110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
041700110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
041800110107      *         un erroneo messaggio
041900110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
042000060612     c                   update    Tivin000
042100090211
042200090211      *** se non è riconosciuto l'UNB deve uscire direttamente
042300090211     c                   if        se_errore ='S'  and
042400100708     c                             tipo_seg = Segm_Inizio_Messaggio
042500090213     c                   eval      err_bloccante = 'S'
042600090211     c                   exsr      Msg_errato
042700090211     c                   LEAVE
042800090211     c                   endIF
042900110107      ***
043000110107      *** ++++++
043100110107     c                   endIF
043200060612
043300100708     c                   EXSR      LEGGE_TIVIN_R
043400060612     C                   ENDdo
043500060612      **
043600060612     c                   endSR
043700100709      * ?------------------------------------------------------------------ */
043800100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
043900100709      * ?------------------------------------------------------------------ */
044000100709     C     LEGGE_TIVIN_R BEGSR
044100100709      *
044200100709     c                   clear                   EoFTivin
044300100709      *  Lettura sequenziale
044400100709     c                   read      Tivin00r
044500100709
044600100709     c                   if        %Eof(tivin00r)
044700100709     c                   move      'S'           EoFTivin
044800100709     c                   else
044900100709      *
045000100709      * ?  imposta il tipo di segmento:
045100100709     c                   clear                   dati
045200100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
045300100728     c                   eval      segmento = %trimL(dati)
045400100728     c                   eval      tipo_seg = %subst(segmento:1:3)
045500100709      **
045600100709     c                   end
045700100709      *
045800100709     C                   ENDSR
045900100120      * ?------------------------------------------------------------------ */
046000100120      *?    Decodifica il segmento ed importa i campi in formato DS
046100100120      * ?------------------------------------------------------------------ */
046200100120     c     Decod_SegmentoBegsr
046300100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
046400100708      **?              /*---------------------- */
046500100722     c                   exsr      Pulizia_campi
046600100708      **?              /*---------------------- */
046700100722      *
046800111214      *   segmenti modificati per ricondurre la traduzione al NOSTRO Standard D94A
046900111214      **?              /*---------------------- */
047000111214     c                   exsr      SOSTITUZIONI
047100111214      **?              /*---------------------- */
047200111214      *
047300100706      *** ------------------------------------
047400100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
047500100120      * ?  Anche se non utilizzati.
047600100712      *** ------------------------------------
047700100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
047800100317     c                   clear                   seg_imprevisto
047900100721     c                   clear                   trovato_seg
048000100712      *** ------------------------------------
048100100319      *
048200100319      *  Contatore delle righe ossia dei segmenti del messaggio
048300100319     c                   add       1             Conta_segmenti
048400100708      *
048500100716     C                   clear                   keyUNBCLI
048600100716     C                   clear                   keyTIPOMSG
048700100716     C                   clear                   keyVERSION
048800100716     C                   clear                   keyRELEASE
048900100716     C                   clear                   keyAGENCY
049000100716     C                   clear                   keyASSOCIA
049100100716      *
049200100721      **--------
049300100721      *  Ritorna la DS GENERICA oppure l'errore
049400100721      **--------
049500100721      *  Chiamata generica x decodificare il singolo segmento letto
049600100721     c                   call      'TRTCT00R1'
049700100721      * input
049800100721     c                   parm                    tipo_seg
049900100721     c                   parm                    segmento
050000100721     C                   parm                    keyUNBCLI
050100100721     C                   parm                    keyTIPOMSG
050200100721     C                   parm                    keyVERSION
050300100721     C                   parm                    keyRELEASE
050400100721     C                   parm                    keyAGENCY
050500100721     C                   parm                    keyASSOCIA
050600100721      *output
050700100721     c                   parm                    Errore
050800100721     c                   parm                    DsGenerica
050900100721     c                   parm                    Valori_inErr
051000100721     c                   parm                    Descr_Errore
051100100721     c                   parm                    trovato_seg
051200100721      *
051300100708      *** ------------------------------------
051400100708      *   Decodifica il singolo segmento LETTO
051500100708      *** ------------------------------------
051600100120     c                   select
051700100120      *
051800110110     c                   when      trovato_seg ='N' and unz_segmento <>'S'
051900100723     C                   eval        eseguire_ROLBACK = 'S'
052000100721     c                   move      'S'           seg_imprevisto
052100100721     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
052200100721     c                   exsr      Error_DET
052300100721     c                   goto      end_Decod
052400100721      **--------
052500100723     c                   when      errore <> ' '
052600100723     C                   eval        eseguire_ROLBACK = 'S'
052700100723     C                   eval      vinMSG = tipo_seg +Valori_inErr +Descr_Errore
052800100723     c                   exsr      Error_DET
052900100723     c                   goto      end_Decod
053000100723      **--------
053100100120     c                   when      tipo_seg = 'UNA'
053200100721     c                   movel(p)  DsGenerica    EDS00UNA
053300100712      * --------
053400100120     c                   when      tipo_seg = 'UNB'
053500100721     c                   movel(p)  DsGenerica    EDS00UNB
053600100120     C                   EXSR      DATI_UNB
053700100120      * --------
053800100120     c                   when      tipo_seg = 'UNH'
053900100721     c                   movel(p)  DsGenerica    EDS00UNH
054000100120     C                   EXSR      DATI_UNH
054100100120      * --------
054200100120     c                   when      tipo_seg = 'BGM'
054300100721     c                   movel(p)  DsGenerica    EDS00BGM
054400100316     C                   EXSR      DATI_BGM
054500100120      **--------
054600100120     c                   when      tipo_seg = 'DTM'
054700100721     c                   movel(p)  DsGenerica    EDS00DTM
054800100120     C                   EXSR      DATI_DTM
054900100120      * --------
055000100712     c                   when      tipo_seg = 'STS'
055100100721     c                   movel(p)  DsGenerica    EDS00STS
055200100712     C                   EXSR      DATI_STS
055300100120      **--------
055400100120     c                   when      tipo_seg = 'RFF'
055500100721     c                   movel(p)  DsGenerica    EDS00RFF
055600100120     C                   EXSR      DATI_RFF
055700100316      **--------
055800100316     c                   when      tipo_seg = 'LOC'
055900100721     c                   movel(p)  DsGenerica    EDS00LOC
056000100316     C                   EXSR      DATI_LOC
056100100120      **--------
056200100120     c                   when      tipo_seg = 'FTX'
056300100721     c                   movel(p)  DsGenerica    EDS00FTX
056400100120     C                   EXSR      DATI_FTX
056500100120      **--------
056600100120     c                   when      tipo_seg = 'NAD'
056700100721     c                   movel(p)  DsGenerica    EDS00NAD
056800100120     C                   EXSR      DATI_NAD
056900100316      **--------
057000100316     c                   when      tipo_seg = 'CNI'
057100100721     c                   movel(p)  DsGenerica    EDS00CNI
057200100316     C                   EXSR      DATI_CNI
057300100120      **--------
057400100120     c                   when      tipo_seg = 'GID'
057500100721     c                   movel(p)  DsGenerica    EDS00GID
057600100120     C                   EXSR      DATI_GID
057700100120      **--------
057800100120     c                   when      tipo_seg = 'PCI'
057900100721     c                   movel(p)  DsGenerica    EDS00PCI
058000100120     C                   EXSR      DATI_PCI
058100100120      **--------
058200100120     c                   when      tipo_seg = 'UNT'
058300100721     c                   movel(p)  DsGenerica    EDS00UNT
058400100120     C                   EXSR      DATI_UNT
058500100120      **--------
058600100120     c                   when      tipo_seg = 'UNZ'
058700100721     c                   movel(p)  DsGenerica    EDS00UNZ
058800100120     C                   EXSR      DATI_UNZ
058900110110     c                   move      'S'           unz_segmento
059000110110      **--------
059100110110     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
059200100120      **--------
059300100120     c                   OTHER
059400100120      **--------
059500110110     c                   if           unz_segmento <>'S'
059600100723     C                   eval        eseguire_ROLBACK = 'S'
059700100120     c                   move      'S'           seg_imprevisto
059800100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
059900100120     c                   exsr      Error_DET
060000100120     c                   goto      end_Decod
060100110110     c                   end
060200100120      **--------
060300100120     c                   endSL
060400100120      **
060500100120     c     end_Decod     endSR
060600100701      * ?================================================================== */
060700111214     C*? Sostituisce parti di segmenti (Per ricondurre allo Standard D94A)
060800100701      * ?================================================================== */
060900111214     C     SOSTITUZIONI  BEGSR
061000100701      **
061100111214      **  sostituisce il 96 con il 94
061200111215     c                   IF        tipo_seg = Segm_Testata_Dettagli
061300111214     c                   z-add     1             pos               3 0
061400111214     c     'D:96A:UN'    SCAN      segmento      pos
061500111215     c                   eval      %subst(segmento:pos:100)= *blank
061600111215     c                   eval      %subst(segmento:pos:100)='D:94A:UN'''
061700111214     c                   end
061800111215      **
061900111215      **  sostituisce il GID aggiungendo il 99999 mancante
062000111215     c                   if        tipo_seg = Segm_Numero_Colli
062100111215     c                   if        %subst(segmento:1:5) ='GID++'
062200111215     c                   clear                   numero_colli     35
062300111215     c                   eval        numero_colli = %subst(segmento:6:35)
062400111215     c                   eval      segmento = 'GID+99999+' + %trim(numero_colli)
062500111215     c                   end
062600111215     c                   end
062700111214      **
062800111214     c                   endSR
062900111214      * ?================================================================== */
063000111214     C*? Azzera_MSG   - Azzera dati messaggio
063100111214      * ?================================================================== */
063200111214     C     Scrive_Dettag BEGSR
063300111214      **
063400100708      *  Deve flaggare il dettaglio con dei problemi
063500100712      * Imposta le righe in errore solo sullo specifico dettaglio
063600100708     C                   if        Err_in_detta = 'S'
063700100708      *  rilascia il record
063800100708     c                   update    tivin000
063900100708      * ?  Scrive su tutti i records il tipo di errore
064000100708     c     IniDET_RECord setll     tivin00r
064100100708      **?              /*---------------------- */
064200100708     c                   exsr      DETta_Errato
064300100708      **?              /*---------------------- */
064400100708     c                   end
064500100708      *
064600100708      **  Se presente un errore nel record emette una segnalazione msg
064700100712      *     altrimenti scrive il FLAT
064800100708???? c                   if        se_errore <> 'S'
064900120105      *
065000120105      *
065100120105      * ?  QUANDO TUTTO OK.
065200100708      * ?              /*---------------------- */
065300100712     c                   exsr      Scrive_Flat
065400100708      * ?              /*---------------------- */
065500120105      *
065600120105      *
065700100708     c                   else
065800100708      *
065900100712      *  Problemi nella decodifica dei campi
066000100708     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
066100100712     c                   eval      errori_in_Wrt = 'S'
066200100708     c                   exsr      Error_DET
066300100708     c                   end
066400100708      *
066500100708     c     end_WRidett   endSR
066600100722      * ?================================================================== */
066700100722     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
066800100722      * ?================================================================== */
066900100722     C     Scrive_FLAT   BEGSR
067000100722      *
067100120105      * il CNI lo deve scrivere solo se c'è uno STATUS convertito correttamente
067200120105      * prima il CNI e poi il record di Status
067300120105      *   quindi sempre a livello di DTM
067400120105     c                   IF           tipo_seg = Segm_Data_Dettaglio  and
067500120105     c                                Contatore_Dettagli > 0
067600100722     C                   CLEAR                   EDSD00
067700100722     C                   MOVEL     CNI1490       DA1490
067800100722     C                   MOVEL     CNI1004       DA1004
067900100722     C                   MOVEL     CNI1312       DA1312
068000111214     C                   CLEAR                   TRTCT14R
068100100722     C                   clear                   WDAT
068200100722     C                   MOVEL     EDSD00        WDAT
068300100722     C                   MOVEL     'SD00'        STATPR
068400100723     C                   MOVEL     WDAT          FLATREC
068500111214     C                   WRITE     TRTCT14R                             99
068600100722     c                   end
068700100722      * STS/RFF/DTM
068800100722     c                   IF           tipo_seg = Segm_Data_Dettaglio  and
068900100722     c                                Contatore_Dettagli > 0
069000100722     C                   CLEAR                   EDSD05
069100100722     C                   MOVEL     STS9011       DB9011
069200100722     C                   MOVEL     STS113120     DB1131
069300100722     C                   MOVEL     STS901330     DB9013
069400100722     C                   MOVEL     STS113130     DBA131
069500100722     C                   MOVEL     RFF1153       DB1153
069600100722     C                   MOVEL     RFF1154       DB1154
069700120105     c                   if        dtm2005 = '96'
069800120105     c                   movel     '7 '          dtm2005
069900120105     c                   end
070000100722     C                   MOVEL     DTM2005       DB2005
070100100722     C                   MOVEL     DTM2380       DB2380
070200120105     c                   if        dtm2379 = '204'
070300120105     c                   movel     '203'         dtm2379
070400120105     c                   end
070500100722     C                   MOVEL     DTM2379       DB2379
070600111214     C                   CLEAR                   TRTCT14R
070700100722     C                   clear                   WDAT
070800100722     C                   MOVEL     EDSD05        WDAT
070900100722     C                   MOVEL     'SD05'        STATPR
071000100723     C                   MOVEL     WDAT          FLATREC
071100111214     C                   WRITE     TRTCT14R                             99
071200100722     c                   end
071300111214      ***NAD****
071400111214     c**********         IF           tipo_seg = Segm_Address_Dettaglio  and
071500111214      *  FTX
071600120119      *   solo se trattasi di stato di consegna
071700111214     c                   IF           tipo_seg = Segm_Text_Dettaglio  and
071800111214     c                                Segm_FTX_Firm = 'S'             and
071900120119     c                                Contatore_Dettagli > 0
072000120119     C
072100100722     C                   CLEAR                   EDSD10
072200111214     C*******            MOVEL     NAD3035       DC3035
072300111214     C*******            MOVEL     NAD30361      DC3036
072400111214     C                   MOVEL     'AP '         DC3035
072500111214     C                   MOVEL     FTX44401      DC3036
072600111214     C                   CLEAR                   TRTCT14R
072700100723     C                   clear                   wdat
072800100722     C                   MOVEL     EDSD10        WDAT
072900100722     C                   MOVEL     'SD10'        STATPR
073000100723     C                   MOVEL     WDAT          FLATREC
073100111214     C                   WRITE     TRTCT14R                             99
073200100722     c                   end
073300100722      *  FTX
073400100722     c                   IF           tipo_seg = Segm_Text_Dettaglio  and
073500111214     c                                Segm_FTX_Firm <>'S'             and
073600100722     c                                Contatore_Dettagli > 0
073700100722     C                   CLEAR                   EDSD15
073800100722     C                   MOVEL     FTX4451       DD4451
073900100722     C                   MOVEL     FTX44401      DD4440
074000100722     C                   MOVEL     FTX44402      DDA440
074100100722     C                   MOVEL     FTX44403      DDB440
074200100722     C                   MOVEL     FTX44404      DDC440
074300100722     C                   MOVEL     FTX44405      DDD440
074400111214     C                   CLEAR                   TRTCT14R
074500100722     C                   clear                   WDAT
074600100722     C                   MOVEL     EDSD15        WDAT
074700100722     C                   MOVEL     'SD15'        STATPR
074800100723     C                   MOVEL     WDAT          FLATREC
074900111214     C                   WRITE     TRTCT14R                             99
075000100722     c                   end
075100100722      *  PCI
075200100722     c                   IF           tipo_seg = Segm_Parcels_Dettaglio  and
075300100722     c                                Contatore_Dettagli > 0
075400120112     c                                  or
075500120112      *  GID
075600120112     c                                tipo_seg = Segm_Numero_Colli       and
075700120112     c                                Contatore_Dettagli > 0
075800100722     C                   CLEAR                   EDSD20
075900120105      *
076000120105      *  Se non ha inviato il GID  x non far spaccare il programma
076100120105     c                   if        Segm_GID_inviato = 'N'
076200120105     c                   clear                   EDS00GID
076300120105     c                   end
076400100722      *
076500111214     C**********         MOVEL     GID1496       DE1496
076600120105     C                   MOVEL     '99999'       DE1496
076700120105     c                   if        GID722420 > 0
076800100722     C                   MOVEL     GID722420     DE7224
076900120105     c                   else
077000120105     C                   z-add     1             DE7224
077100120105     c                   end
077200111214     C**********         MOVEL     GID706520     DE7065
077300111214     C                   MOVEL     'PC'          DE7065
077400100722      *
077500120105     C                   MOVEL     '24'          DE4233
077600100722     C                   MOVEL     PCI71021      DE7102
077700100722     C                   MOVEL     PCI71022      DEA102
077800100722     C                   MOVEL     PCI71023      DEB102
077900100722     C                   MOVEL     PCI71024      DEC102
078000100722     C                   MOVEL     PCI71025      DED102
078100100722     C                   MOVEL     PCI71026      DEE102
078200100722     C                   MOVEL     PCI71027      DEF102
078300100722     C                   MOVEL     PCI71028      DEG102
078400100722     C                   MOVEL     PCI71029      DEH102
078500100722     C                   MOVEL     PCI710210     DEI102
078600100722      *
078700111214     C                   CLEAR                   TRTCT14R
078800100722     C                   clear                   WDAT
078900100722     C                   MOVEL     EDSD20        WDAT
079000100722     C                   MOVEL     'SD20'        STATPR
079100100723     C                   MOVEL     WDAT          FLATREC
079200111214     C                   WRITE     TRTCT14R                             99
079300100722      *
079400100722     C                   clear                   GID1496
079500100722     C                   clear                   GID722420
079600100722     C                   clear                   GID706520
079700100722     c                   end
079800100722      *
079900100722     c                   clear                   err_bloccante     1
080000100722     c                   move      'S'           Almeno_Uno
080100100722     c   99              eval      errori_in_Wrt = 'S'
080200100722     C* >>>>>>>>>>>>
080300100722     C                   if        se_errore  = 'S'
080400100722     c                   move      'S'           err_bloccante
080500100722     c                   goto      Error_Flat
080600100722     c                   end
080700100722     C*
080800100722     C     Error_Flat    Endsr
080900100708      * ?================================================================== */
081000100722     C*? Pulizia campi -
081100100708      * ?================================================================== */
081200100722     C     Pulizia_campi BEGSR
081300100708      **
081400100712      **
081500100712      * inzio messaggio azzera tutte le variabili generali del messaggio
081600100712      * ?                         --------------
081700100712     c                   If        tipo_seg = Segm_Inizio_Messaggio
081800100712      * ?                         --------------
081900100723     C                   eval        eseguire_ROLBACK = ' '
082000100712      *  INIZIALIZZA  dati fondamentali messaggio
082100100712     C                   clear                   EDIDSPT
082200100712     C                   clear                   EDIDSPU
082300100712     C                   clear                   EDIDSPV
082400100722     C                   clear                   EDIDSMS
082500100712     C                   move      'N'           PT_trovata
082600100712     c                   clear                   UNB_Generico
082700100712     c                   clear                   UNB_Parz
082800100712     c                   clear                   UNB_NrTrasmiss
082900100712      * per controllare se almeno un record è stato importato sul VAB
083000100712     c                   clear                   Almeno_Uno        1
083100100712     c                   clear                   Almeno_Un_Due     1
083200100712     c                   clear                   IniDET_RECord
083300100712     c                   clear                   FinDET_RECord
083400100712     c                   eval      esito  = '0'
083500100722     c                   clear                   EDST00
083600100722     c                   clear                   EDST05
083700100722     c                   clear                   EDST10
083800100722     c                   clear                   EDST15
083900100722     C                   clear                   nad3035SF
084000100722     C                   clear                   nad3039SF
084100100722     C                   clear                   nad3035ST
084200100722     C                   clear                   nad3039ST
084300100722     C                   clear                   LOC3227t1
084400100722     C                   clear                   LOC3225t1
084500100722     C                   clear                   LOC3227t2
084600100722     C                   clear                   LOC3225t2
084700100722     C                   clear                   dtm2005te
084800100722     C                   clear                   dtm2380te
084900100722     C                   clear                   dtm2379te
085000100722     C                   clear                   rff1153te
085100100722     C                   clear                   rff1154te
085200100712      *
085300100712      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
085400100712      * ?                         --------------
085500100712     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
085600100712      * ?                         --------------
085700100712      *   Contatore dei dettagli
085800100712     c                   eval      Contatore_Dettagli = 0
085900100712      **
086000100712      *   Inizia il conteggio delle righe
086100100712     c                   z-add     0             Conta_segmenti    5 0
086200100712      *
086300100712      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
086400100712      *  Prepara una nuova spedizione
086500100712      * ?                         --------------
086600100712     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
086700100712      * ?                         --------------
086800100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
086900100708      *  Prepara una nuova spedizione
087000100712     c                   clear                   EDSD00
087100100712     c                   clear                   EDSD05
087200100712     c                   clear                   EDSD10
087300100712     c                   clear                   EDSD15
087400100712     c                   clear                   EDSD20
087500100712      **
087600100712     c                   z-add     vnrec         IniDET_RECord
087700100722      **
087800100712      * Un errore nel dettaglio
087900100712     C                   clear                   Err_in_detta      1
088000100712     c                   clear                   errori_in_Wrt     1
088100111214     c                   clear                   Segm_FTX_Firm     1
088200100712      *
088300100723     c                   endIF
088400100708      **
088500100708     c     end_clear     endSR
088600100701      * ?================================================================== */
088700090210      *? DECODIFICA DATI UNB
088800100712      * ?================================================================== */
088900090210     C     DATI_UNB      BEGSR
089000090210      *
089100111214      *   Gli Status sono fuori STANDARD il Partner invia con altri codici
089200111214      *     gli avanzamenti della consegna
089300111214     c                   eval       Converte_Cod_Status = ' '
089400111221     c                   eval       Cod_IdentKEY = *BLANK
089500111214      *
089600100701      * identificativo Messaggio
089700100701     C                   eval      UNB_NrTrasmiss = unb0020
089800100701      *
089900100712      * quindi Controlla esistenza UNB ricevuto del mittente se presente
090000100712      *  anche se parziale.
090100100630     C                   MOVEL     unb0004       UNB_diWork
090200100630     C                   MOVE      unb000720     UNB_diWork
090300100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
090400100712     c                   clear                   UNB_parziale
090500100630     C                   Z-ADD     1             Nr_PT
090600100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
090700100712      * Poi occorre verificare se il codice UNB è fra quelli parzializzati
090800100317     C                   If        *IN32 = *off
090900100630     C                   eval      Nr_PT = 1
091000100630     c                   clear                   UNB_diWork
091100100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
091200100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
091300100712     c   32              eval      UNB_parziale = 'S'
091400100712     c   32              eval      UNB_Generico = UNB_diWork
091500100712     c                   end
091600100317      *
091700100712     C                   If        *IN32 = *off
091800100319      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
091900100319      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Bartolini.it
092000090209      * Msg di allerta Traduzione
092100090209     C                   EVAL      Indirizzi = CED_Bart
092200100317???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFCSUM'
092300090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
092400090209     C                             Bolle import in filiale - messaggio -
092500090209     C                             con UNB non presente in tabella PT controlla-
092600090209     C                             re EDI ricevuto su UPLOAD.'
092700090209      *
092800090209     c                   exsr      invio_mail
092900090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
093000090210     C                   eval      vinFLG = '2'
093100090210     C                   eval      se_errore   = 'S'
093200090210     c                   goto      end_UNB
093300090209      *
093400100630     C                   move      'N'           PT_trovata
093500090209      *
093600090209      *             *------------------------------*
093700090209     C                   eLSe
093800090209      *             *------------------------------*
093900100712      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
094000100712     c                   movel     UNB_diWork    UNB_Parz
094100100712      **
094200090209      * Se tutto OK su tab.PT:
094300100630     C                   move      'S'           PT_trovata
094400100630     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
094500100712     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
094600100712     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
094700111215     C                   MOVEL     PT_KEY(Nr_PT) SAVUNB_Mitt      35
094800091016     c                   clear                   Mittente         35
094900091016     c                   clear                   Mitt_Qualifier    4
095000091016     c                   clear                   Id_Messaggio     14
095100090226     C                   eval      Mittente       = unb0004
095200090226     C                   eval      Mitt_qualifier = unb000720
095300090226     C                   eval      Id_Messaggio   = UNB0022
095400090209      *
095500111214      * ?imposta se deve passare per una tabella di conversione STATUS
095600111214      *    poichè inviano differenti STATUS codes rispetto al nostro Standard
095700111221     c                   eval       Converte_Cod_Status = §pvCNVSTS
095800111221     c                   eval       Cod_IdentKEY = §pvIDENT
095900111214      *
096000090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
096100090209      *  ricevuti:
096200090209     c                   movel     §PVema        mail_ind         44
096300090209      *
096400090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
096500090209     c                   if        mail_ind <> *blank
096600090209     c                   exsr      imp_ADDR_mail
096700090209     c                   end
096800100712      *
096900090209     c                   Endif
097000111214      **
097100090210     c     end_UNB       endSR
097200090227      * ?================================================================== */
097300090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
097400090227      * ?================================================================== */
097500090227     C     DATI_UNH      BEGSR
097600100316      *
097700100701      *
097800090210     c     end_UNH       endSR
097900100701      * ?================================================================== */
098000100701      *? dati iniziali   BGM
098100100701      * ?================================================================== */
098200100701     C     DATI_BGM      BEGSR
098300100701      *
098400100712      *
098500100701     c                   endSR
098600100705      * ?================================================================== */
098700100705     C*? Imposta la Località
098800100705      * ?================================================================== */
098900100705     C     DATI_LOC      BEGSR
099000100705     C*
099100100722     c                   If        LOC3227t1 = *blank
099200100722      *
099300100722     c                   eval      LOC3227t1 = LOC3227
099400100722     c                   eval      LOC3225t1 = LOC3225
099500100722      *
099600100722     c                   else
099700100722      *
099800100722     c                   eval      LOC3227t2 = LOC3227
099900100722     c                   eval      LOC3225t2 = LOC3225
100000100722      *
100100100722     c                   end
100200100705      **
100300100705     c     end_LOC       endSR
100400100316      * ?================================================================== */
100500100316     C*? Imposta l'inizio del Dettaglio CNI
100600100316      * ?================================================================== */
100700100316     C     DATI_CNI      BEGSR
100800100316     C*
100900100702      ** Inizio Dettaglio riferimento iniziale
101000100702     c                   If        CNI1004 <> *blank
101100100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
101200100702     c                   end
101300120105      **
101400120105      **  per controllare se il GID è presente in seguito
101500120105     c                   eval      Segm_GID_inviato = 'N'
101600100316      **
101700120105      ** Non deve scrivere qui poichè potrebbe aver ricevuto uno STATUS da non tradurre
101800120105      **   quindi deve valutare in seguito la scrittura intanto si è memorizzato i campi
101900120105      **   x il record SD00 che scriverà nello stesso momento in cui si prepara a scrivere
102000120105      **     il record SD05.
102100100722      **
102200100316     c     end_CNI       endSR
102300100722      * ?================================================================== */
102400100722     C*? Imposta informazioni sullo STATO di Consegna
102500100722      * ?================================================================== */
102600100722     C     DATI_STS      BEGSR
102700100722     C*
102800111214     C*  Se abilitato alla conversione dei codici STATUS occorre passare per
102900111214     C*   la tabella di conversione e riportare i nostri codici STANDARD
103000111214     C*    al posto di quelli ricevuti dal Partner.
103100111214     c                   if        Converte_Cod_Status ='S'
103200111214      **
103300111221     c                   if         Cod_IdentKEY = *BLANK
103400111215     c                   eval        STSUNBCLI = SAVUNB_Mitt
103500111221     c                   else
103600111221     c                   eval        STSUNBCLI = Cod_IdentKEY
103700111221     c                   end
103800111221      **
103900111215     c                   eval        STSCODSTS = STS9011
104000111215     c                   eval        STSSUBSTS = STS901330
104100111214      **
104200111215     c     key_STS       chain     edSTS01l
104300111215     c                   if        %Found(edSTS01l)
104400120104     C                   eval      STATUS_Trovato = 'S'
104500111215     c                   eval          STS9011    = STSCODSTSb
104600111215     c                   eval          STS901330  = STSSUBSTSb
104700120104     c                   else
104800120104     C                   eval      STATUS_Trovato = 'N'
104900120104     c                   end
105000111215      **
105100111214     c                   end
105200100722      **
105300100722     c     end_STS       endSR
105400100722      * ?================================================================== */
105500100722     C*?  il Riferimento della spedizione
105600100722      * ?================================================================== */
105700100722     C     DATI_RFF      BEGSR
105800100722      **
105900111214      **  Converte il codice riferimento in AGE poichè solo con l'AGE
106000111214      **   viene riconosciuta la spedizione nel nostro sistema
106100111214     c                   if        RFF1153    = 'CU'
106200111214     c                   eval         RFF1153 = 'AGE'
106300130513      **---------
106400130513      **  per risolvere un problema di invio dati con riferimenti errati
106500130513      **   queste specifiche dovranno essere cancellate appena finito il problema
106600130513     c                   if        %subst(RFF1154:1:2)='00'
106700130514     c                   clear                   campo9A           9
106800130514     c                   eval      campo9A = CNI_Identificativo_Bolla
106900130514     c                   movel     '0'           rifer_0
107000130514     c                   move      campo9A       rifer_0
107100130513     c                   movel(p)  RFF1154       RIFER_1
107200130513     c                   clear                   RIFER_2
107300130513     c                   clear                   RIFER_3
107400130513     c                   call      'TRTCT14R9'
107500130514     c                   parm                    RIFER_0          10
107600130513     c                   parm                    RIFER_1          15
107700130513     c                   parm                    RIFER_2          15
107800130513     c                   parm                    RIFER_3           1
107900130513      **
108000130513      ***  ha trovato il codice UNIVOCO da sostituire
108100130514     c                   if        RIFER_2 <>*blank
108200130513     c                   movel(p)  RIFER_2       RFF1154
108300130513     c                   end
108400130513     c                   end
108500130513      **
108600130513      **
108700130513      **---------
108800111214     c                   end
108900100722      *
109000120104      *  Dettaglio Colli x ATR
109100120104     c                   if        RFF1153    = 'LS'
109200120104     c                   end
109300120104      *
109400100722     c     end_RFF       endSR
109500100722      * ?================================================================== */
109600100722     C*? Imposta la data dell'evento
109700100722      * ?================================================================== */
109800100722     C     DATI_DTM      BEGSR
109900100722      *
110000120104     c                   If           Contatore_Dettagli > 0
110100120104     C                              and  STATUS_Trovato = 'S'
110200120104      *
110300111214     c                   if        dtm2005 = '137' or  dtm2005 = '7'
110400120104      ** x il file ATR Ducros
110500120104     c                               or  dtm2005 = '96'
110600100722      **?              /*========================= */
110700100722     c                   exsr      Scrive_Dettag
110800100722      **?              /*========================= */
110900100723     c                   end
111000111214     c                   end
111100100722      *
111200100722     c     end_DTM       endSR
111300100705      * ?================================================================== */
111400100705     C*?  FREE TEXT
111500100705      * ?================================================================== */
111600100705     C     DATI_FTX      BEGSR
111700100705      *
111800120104     c                   If           Contatore_Dettagli > 0
111900120104     C                              and  STATUS_Trovato = 'S'
112000111214      *
112100111214     c                   eval      Segm_FTX_Firm = ' '
112200111214      * ABO = Firma del destinatario che riceve la merce
112300120119     C                   If        (STS9011 = '21 ' or STS901330 = '21 ')
112400111214     c                   eval      Segm_FTX_Firm = 'S'
112500111214     c                   end
112600111214      *
112700100722      **?              /*========================= */
112800100722     c                   exsr      Scrive_Dettag
112900100722      **?              /*========================= */
113000100723     c                   end
113100100721      *
113200100705     c     end_FTX       endSR
113300090213      * ?================================================================== */
113400090213     C*?  Anagrafica del mittente e del destinatario
113500090213      * ?================================================================== */
113600091016     C     DATI_NAD      BEGSR
113700100705      *
113800111214     c                   if        NAD3035 = 'MR '
113900100722      *
114000111215     c                   eval      nad3035SF = 'SF '
114100100722     c                   eval      nad3039SF = NAD3039
114200100722      *
114300111214     c                   elseIf    NAD3035 = 'MS '
114400100722      *
114500111215     c                   eval      nad3035ST = 'ST '
114600100722     c                   eval      nad3039ST = NAD3039
114700100722      *
114800100722     c                   end
114900100722      **
115000100723     c                   If           Contatore_Dettagli > 0
115100120104     C                              and  STATUS_Trovato = 'S'
115200100722      **?              /*========================= */
115300100722     c                   exsr      Scrive_Dettag
115400100722      **?              /*========================= */
115500100723     c                   end
115600090210      *
115700090210     c     end_NAD       endSR
115800090210      * ?================================================================== */
115900090210     C*?  Dati di dettaglio della spedizione e dei colli
116000090210      * ?================================================================== */
116100090210     C     DATI_GID      BEGSR
116200100706      *
116300120105     c                   eval      Segm_GID_inviato = 'S'
116400120112      **
116500120112     c                   If           Contatore_Dettagli > 0
116600120112     C                              and  STATUS_Trovato = 'S'
116700120112      **?              /*========================= */
116800120112     c                   exsr      Scrive_Dettag
116900120112      **?              /*========================= */
117000120112     c                   end
117100120112      *
117200090210      *
117300090210     c     end_GID       endSR
117400090210      * ?================================================================== */
117500090210     C*?  Dati di dettaglio Codice a barre Segnacolli
117600090210      * ?================================================================== */
117700090210     C     DATI_PCI      BEGSR
117800090210      *
117900100723     c                   If           Contatore_Dettagli > 0
118000120104     C                              and  STATUS_Trovato = 'S'
118100100722      **?              /*========================= */
118200100722     c                   exsr      Scrive_Dettag
118300100722      **?              /*========================= */
118400100723     c                   end
118500090210      *
118600090210     c     end_PCI       endSR
118700090211     C*----------------------------------------------------------------
118800090211      *? dati finali del dettaglio    UNT
118900090211     C*----------------------------------------------------------------
119000090211     C     DATI_UNT      BEGSR
119100110324      *
119200110324      * Non deve dare messaggio
119300110324     c                   goto      No_UNTctrl
119400090211      *
119500100708     c                   eval      UNT_record = vnREC
119600090213     c                   z-add     vnrec         last_RECord
119700090211      **
119800100319      **   verifica la quadratura di tutti i segmenti ricevuti
119900100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
120000100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
120100100319     C     UNT0074       IFne      Conta_segmenti
120200100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
120300100319     c                             trollare il Messaggio ricevuto'
120400100319      *
120500100319???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFCSUM :'
120600100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
120700100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
120800100319     C                             Bolle import in filiale - messaggio -
120900100319     C                             con quadratura UNT errata controllare EDI -
121000100319     C                             ricevuto su UPLOAD x il cliente :'
121100100319     C                   EVAL      Messaggio = %trim(Messaggio) +
121200100319     C                                         %editc(§PTksc:'X')
121300100319     C                   EVAL      Messaggio = %trim(Messaggio) +
121400100319     C                             ' i segmenti contati sono :' +
121500100319     C                             %editc(Conta_segmenti:'Z')
121600100319     c                   if        §PVinv <> 'N'
121700100319     c                   exsr      invio_mail
121800100319     c                   end
121900100319     c                   end
122000110324     c     No_UNTctrl    tag
122100100319      **
122200100319     c     End_UNT       endSR
122300100316     C*----------------------------------------------------------------
122400100316      *? dati finali     UNT
122500100316     C*----------------------------------------------------------------
122600100316     C     DATI_UNZ      BEGSR
122700100316      *
122800100726     c                   move      'S'           Forza_Uscita
122900100316      **
123000100316     c                   endSR
123100090210     C*----------------------------------------------------------------
123200051205      *   DEFINIZIONE CHIAVI                               *
123300100712     C*----------------------------------------------------------------
123400051205     C     *INZSR        BEGSR
123500051205      *
123600111215     c     Key_STS       KLIST
123700111215     c                   KFLD                    STSUNBCLI
123800111215     c                   KFLD                    STSCODSTS
123900111215     c                   KFLD                    STSSUBSTS
124000111215      *
124100991129     c     *ENTRY        PLIST
124200060613     C                   parm                    esito             1
124300090209      *
124400090209     C* RECUPERO DATI DELL'UTENTE
124500090209     C                   Z-ADD     1             CODUT             1 0
124600090209     C                   CALL      'XPARUT'
124700090209     C                   PARM                    UTEDSE0F
124800090209     C                   MOVEL     RAGUT         RSUT             20
124900090209     C*
125000090209     C* RICERCA CAPOCONTI
125100090209     C                   DO        50            X
125200090209     C                   MOVE      TCU(X)        TCUDS
125300090209     C     F56           IFEQ      'CG'
125400090209     C     F34           ANDEQ     '01'
125500090209     C                   Z-ADD     KCU(X)        KCI               4 0
125600090209     C                   END
125700090209     C                   END
125800100722     C*
125900090211     c                   movel(p)  'EDPAB'       User_Msg         10
126000090211     C*
126100090209     C* IMPOSTO A ZERO VARIABILI DI PGM
126200090209     c                   move      *blank        Snd_Msg_alert     1
126300090209     C                   MOVEL     CA(1)         WCA              78
126400090209     C                   MOVEL     CN(1)         WCN              78
126500090209     C*
126600090209     C* RECUPERO DATA E ORA
126700100630     C                   TIME                    Ora_Data         14 0
126800100630     C                   MOVE      Ora_Data      G02DAT
126900100630     C                   MOVE      Ora_Data      WDATA8            8 0
127000100630     C                   MOVE      WDATA8        Anno2             2 0
127100090209     C                   MOVEL     WDATA8        DATA              6 0
127200100630     C                   MOVE      Anno2         DATA
127300090209     C                   MOVEL     *BLANK        G02ERR
127400090209     C                   CALL      'XSRDA8'
127500090209     C                   PARM                    WLBDA8
127600090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
127700100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
127800090209     C                   TIME                    WORA              6 0
127900090209      * Recupero data e ora
128000090209     C                   TIME                    W0140
128100090209      * UDATE IN GGMMAAAA
128200100630     C                   MOVE      W0140         DataGGMMAAA
128300090209      * UDATE IN AAAAMMGG
128400100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
128500090209     C     *iso          MOVEL     DATA_eur      dateu
128600090209      *
128700090209      * ?              /*--------------------------- */
128800090209     c                   exsr      CARICA_TABELLE
128900090209      * ?              /*--------------------------- */
129000090209      *
129100090209     C                   MOVEL     *ALL'-'       CMP132          132
129200991124      *
129300050414      *------------------
129400991124     C                   ENDSR
129500060621      * ?------------------------------------------------------------------ */
129600090209      *?    CARICA TABELLE SU SCHIERE
129700060621      * ?------------------------------------------------------------------ */
129800090209     C     CARICA_TABELLEBEGSR
129900090209      *
130000090209      * Caricamento Tabella Partner esteri
130100090209     C                   Z-ADD     0             X
130200090209     C                   MOVEL     'PT'          TABCOD
130300090209     C     TABCOD        CHAIN     EDTAB01L                           30
130400090209     C     *IN30         DOWEQ     '0'
130500090209      *
130600090209     C                   SELECT
130700090209     C     TABFLG        WHENNE    *BLANKS
130800090209      *
130900090209     C                   OTHER
131000090209     C                   ADD       1             X
131100100630     C                   MOVEL     TABKEY        PT_key(X)
131200100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
131300100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
131400100630     C                   MOVEL     TABUNI        PT_uni(X)
131500090209     C                   ENDSL
131600090209      *
131700090209     C     TABCOD        READE     EDTAB01L                               30
131800090209     C                   END
131900121105      *
132000121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
132100121105     c                   exsr      ChecK_PT
132200121105      *
132300090209      * salva quanti Codici UNB ha caricato
132400100630     c                   z-add     x             PT_KeyXsav
132500090209      *
132600090209     C                   MOVEL     'PU'          TABCOD
132700090209     C     TABCOD        CHAIN     EDTAB01L                           30
132800090209     C     *IN30         DOWEQ     '0'
132900090209      *
133000090209     C                   SELECT
133100090209     C     TABFLG        WHENNE    *BLANKS
133200090209      *
133300090209     C                   OTHER
133400100630     C                   MOVEL     TABKEY        PU_key
133500100723     C                   Z-ADD     1             X                 3 0
133600100630     C     PU_key        LOOKUP    PT_key(X)                              32
133700100630     C   32              MOVEL     TABUNI        PU_uni(X)
133800090209     C                   ENDSL
133900090209      *
134000090209     C     TABCOD        READE     EDTAB01L                               30
134100090209     C                   END
134200090209      *
134300090209      * Prosegue tab.PT e PU
134400090209     C                   MOVEL     'PV'          TABCOD
134500090209     C     TABCOD        CHAIN     EDTAB01L                           30
134600090209     C     *IN30         DOWEQ     '0'
134700090209      *
134800090209     C                   SELECT
134900090209     C     TABFLG        WHENNE    *BLANKS
135000090209      *
135100090209     C                   OTHER
135200100630     C                   MOVEL     TABKEY        PV_key
135300090209     C                   Z-ADD     1             X
135400100630     C     PV_key        LOOKUP    PT_key(X)                              32
135500100630     C   32              MOVEL     TABUNI        PV_uni(X)
135600090209     C                   ENDSL
135700090209      *
135800090209     C     TABCOD        READE     EDTAB01L                               30
135900090209     C                   END
136000100722      *
136100090209     C                   ENDSR
136200090205      * ?------------------------------------------------------------------ */
136300090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
136400090205      * ?------------------------------------------------------------------ */
136500090205     C     *pssr         BEGSR
136600090205     C
136700060710     C                   eval      esito = '2'
136800100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
136900090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
137000100712     C                             STATI IFTSTA - messaggio -
137100090603     C                             EDI ricevuto errato su UPLOAD.'
137200100319     c                   if        §PVinv <> 'N'
137300100319     c                   exsr      invio_mail
137400100319     c                   end
137500060621     C
137600090213     C                   ENDSR
137700090213      * ?------------------------------------------------------------------ */
137800090213      *?      Segnala Errore su TIVIN
137900090213      * ?------------------------------------------------------------------ */
138000090213     C     Error_DET     BEGSR
138100090213     C
138200090213     C                   eval      vinFLG = '2'
138300090213     c                   eval      Almeno_Un_Due ='S'
138400090213     c                   eval      esito  = '1'
138500090213     c                   eval      se_errore ='S'
138600090213     C                   eval      Err_in_detta = 'S'
138700090213     C
138800090213     C                   ENDSR
138900100712      *----------------------------------------------------------------
139000100712      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
139100100712      *----------------------------------------------------------------
139200100712     C     TESTAMSG      BEGSR
139300100712      *
139400100722      * Eseguo scrittura dati record ST00   (UNB/BGM/DTM)
139500100712     C                   EXSR      WRST00
139600100722      * Eseguo scrittura dati record ST05   (NAD)
139700100712     C                   EXSR      WRST05
139800100722      * Eseguo scrittura dati record ST10   (RFF/LOC)
139900100712     C***                EXSR      WRST10
140000100722      * Eseguo scrittura dati record ST15   (FTX)
140100100712     C                   EXSR      WRST15
140200100712      *
140300100712     C                   ENDSR
140400100712      *----------------------------------------------------------------
140500100712      *? WRST00 - Scrittura record partner
140600100712      *----------------------------------------------------------------
140700100712     C     WRST00        BEGSR
140800100712      *
140900100712      *  Pulisco dati DS
141000100712     C                   CLEAR                   EDST00
141100100712      *
141200100712     C                   eval        TA0004 = UNB0004
141300100712     C                   eval        TA0007 = UNB000720
141400100712      *
141500100712     C                   eval        TA0010 = UNB0010
141600100712     C                   eval        TAA007 = UNB000730
141700100712      *
141800100712      *  Data/Ora invio messagio:
141900100712     C                   eval        TA0017 = UNB0017
142000100723     C                   move      UNB0019       TA0019
142100100712      *
142200100712      *  Codice documento - nome documento
142300100712     C                   eval        TA1001 = BGM1001
142400100712     C                   eval        TA1000 = BGM1000
142500100712     C                   eval        TA1004 = BGM1004
142600100712     C                   eval        TA1225 = BGM1225
142700100712      *  Data messaggio:
142800100712     C                   eval        TA2005 = DTM2005
142900100712     C                   eval        TA2380 = DTM2380
143000100712     C                   eval        TA2379 = DTM2379
143100100712      *
143200100712      *  Scivo record IFTSTA
143300111214     C                   CLEAR                   TRTCT14R
143400100712     C                   CLEAR                   WDAT
143500100712     C                   MOVEL     EDST00        WDAT
143600100712     C                   MOVEL     'ST00'        STATPR
143700100723     C                   MOVEL     WDAT          FLATREC
143800111214     C                   WRITE     TRTCT14R                             99
143900100712      *
144000100712     C                   ENDSR
144100100712      *----------------------------------------------------------------
144200100712      *? WRST05 - Scrittura record partner
144300100712      *----------------------------------------------------------------
144400100712     C     WRST05        BEGSR
144500100712      *
144600100712      *  Ship from:
144700100722     C                   CLEAR                   EDST05
144800100722     C                   MOVEL     nad3035SF     TE3035
144900100723     C                   MOVEL     nad3039SF     TE3036
145000111214     C                   CLEAR                   TRTCT14R
145100100722     C                   CLEAR                   WDAT
145200100712     C                   MOVEL     EDST05        WDAT
145300100712     C                   MOVEL     'ST05'        STATPR
145400100723     C                   MOVEL     WDAT          FLATREC
145500111214     C                   WRITE     TRTCT14R
145600100712      *  Ship to:
145700100712     C                   CLEAR                   EDST05
145800100722     C                   MOVEL     nad3035ST     TE3035
145900100723     C                   MOVEL     nad3039ST     TE3036
146000111214     C                   CLEAR                   TRTCT14R
146100100722     C                   CLEAR                   WDAT
146200100712     C                   MOVEL     EDST05        WDAT
146300100712     C                   MOVEL     'ST05'        STATPR
146400100723     C                   MOVEL     WDAT          FLATREC
146500111214     C                   WRITE     TRTCT14R                             99
146600100712      *
146700100712     C                   ENDSR
146800100712      *----------------------------------------------------------------
146900100712      *? WRST10 - Scrittura record partner
147000100712      *----------------------------------------------------------------
147100100712     C     WRST10        BEGSR
147200100712      *
147300100722      *  scrittura condizionata
147400100722     C                   if        rff1153 <> *blank or loc3227t1 <> *blank
147500100712      *  Pulisco dati DS
147600100712     C                   CLEAR                   EDST10
147700100722     C                   MOVEL     rff1153te     TB1153
147800100722     C                   MOVEL     rff1154te     TB1154
147900100722     C                   MOVEL     LOC3227t1     TB3227
148000100722     C                   MOVEL     LOC3225t1     TB3225
148100100722     C                   MOVEL     LOC3227t2     TBA227
148200100722     C                   MOVEL     LOC3225t2     TBA225
148300111214     C                   CLEAR                   TRTCT14R
148400100722     C                   clear                   WDAT
148500100712     C                   MOVEL     EDST10        WDAT
148600100712     C                   MOVEL     'ST10'        STATPR
148700100723     C                   MOVEL     WDAT          FLATREC
148800111214     C                   WRITE     TRTCT14R                             99
148900100722     c                   end
149000100712      *
149100100712     C                   ENDSR
149200100712      *----------------------------------------------------------------
149300100712      *? WRST15 - Scrittura record partner
149400100712      *----------------------------------------------------------------
149500100712     C     WRST15        BEGSR
149600100712      *
149700100722      *  scrittura condizionata
149800100722     C                   if         FTX44401 <> *blank
149900100712      *  Pulisco dati DS
150000100712     C                   CLEAR                   EDST15
150100100722     C                   MOVEL     ftx4451       TC4451
150200100722     C                   MOVEL     FTX44401      TC4440
150300100722     C                   MOVEL     FTX44402      TCA440
150400100722     C                   MOVEL     FTX44403      TCB440
150500100722     C                   MOVEL     FTX44404      TCC440
150600100722     C                   MOVEL     FTX44405      TCD440
150700111214     C                   CLEAR                   TRTCT14R
150800100722     C                   clear                   WDAT
150900100722     C                   MOVEL     EDST15        WDAT
151000100722     C                   MOVEL     'ST15'        STATPR
151100100723     C                   MOVEL     WDAT          FLATREC
151200111214     C                   WRITE     TRTCT14R                             99
151300100722     c                   end
151400100712      *
151500100712     C                   ENDSR
151600090211     c*==================================================================*
151700090211      * Manda un Msg in Posta AS Sede a EDP*
151800090211     c*==================================================================*
151900090211     c     SEND_MSG_EDP  begsr
152000090211      *
152100090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
152200090211      *   Lo manda una sola volta per lavoro
152300090211     c                   IF        Snd_Msg_alert  <> 'N' and
152400090211     c                             User_msg <> *blank
152500090211     c                   z-add     1             Jx
152600090211     C                   exsr      CalQEZSNDMG
152700090211     c                   end
152800090211      *
152900090211     c                   endsr
153000090213     c*==================================================================*
153100090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
153200090213     c*==================================================================*
153300090213     c     Imp_ADDR_mail begsr
153400090213      *
153500090213     c                   clear                   Indirizzi
153600090213      *
153700090213      *  Lunghezza indirizzi presenti
153800100630     c                   eval      Lunghezza_Indirizzo =
153900100630     c                                       %Len(%TrimR(Mail_Ind))
154000090213     c                   z-add     1             al_char           3 0
154100090213     c                   z-add     1             dal_char          3 0
154200090213     c                   z-add     1             tot_char          3 0
154300090213      *
154400090213     c     successivo    tag
154500090213      *
154600090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
154700090213      *   e aggiunge il dominio Bartolini + il (;) separatore
154800090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
154900090213     c                   eval      tot_char = al_char - dal_char
155000100630     c                   z-add     dal_char      dalCarattere
155100100630     c                   z-add     tot_char      xTOTcaratteri
155200090213      *
155300090213     c                   clear                   Indir_Bartol     40
155400090213     c                   clear                   Indir_Parz       20
155500100630     c                   eval      Indir_Parz =
155600100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
155700100630      *
155800090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
155900090213     c                             %Trim(bart_it)
156000090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
156100090213      *
156200100630     c                   if        al_char = Lunghezza_Indirizzo
156300090213     c                   goto      fine_indmail
156400090213     c                   else
156500090213     c                   eval      dal_char = ( al_char + 1 )
156600090213     c                   goto      successivo
156700090213     c                   end
156800090213      *
156900090213     C     fine_indmail  TAG
157000090213      *
157100090213     C                   ENDSR
157200090213     c*==================================================================*
157300090213      * Manda un Msg x E-mail
157400090213     c*==================================================================*
157500090213     c     Invio_Mail    begsr
157600090213      *
157700090213     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
157800090213     C* Inizializzo variabili
157900090213     C                   movel     *blanks       wrkEml          253
158000090213     C                   movel     *blanks       wrkMsg         5000
158100090213     C                   movel     *blanks       wrkOgg           44
158200090213      *
158300090213     C* Valorizzo i campi della e-m@ail - indirizzo
158400100319     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
158500090213     C                   eval      wrkEml = Indirizzi
158600090213     C                   eval      wrkOgg = Oggetto
158700090213     C                   eval      wrkMsg = Messaggio
158800110203      ********
158900110203     C                   call(e)   'TRTCT00R2'
159000110203     C                   parm                    wrkEml
159100110203     C                   parm                    wrkOgg
159200110203     C                   parm                    wrkMsg
159300110203     C*
159400090213     C*
159500090213     C                   ENDSR
159600090603      *
159700090211     ***********************************************************************
159800090211     **
159900090211     ** Send Message (QEZSNDMG) API
160000090211     **
160100090211     ***********************************************************************
160200090211     C     CalQEZSNDMG   BEGSR
160300090211      *
160400090211     ** Invio messaggio all'utente.
160500090211     C                   EVAL      SndMg03 = msgDSP
160600090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
160700090211     C                   EVAL      SndMg05(Jx) = User_msg
160800090211     C                   EVAL      SndMg06 = Jx
160900090211     C                   CLEAR                   QUSEC
161000090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
161100090211
161200090211     C                   CALL      'QEZSNDMG'
161300090211     C                   PARM                    SndMg01
161400090211     C                   PARM                    SndMg02
161500090211     C                   PARM                    SndMg03
161600090211     C                   PARM                    SndMg04
161700090211     C                   PARM                    SndMg05
161800090211     C                   PARM                    SndMg06
161900090211     C                   PARM                    SndMg07
162000090211     C                   PARM                    SndMg08
162100090211     C                   PARM                    Qusec
162200090211     C                   PARM                    SndMg10
162300090211     C                   PARM                    SndMg11
162400090211     C                   PARM                    SndMg12
162500090211      *
162600090211     C                   ENDSR
162700100708      * ?------------------------------------------------------------------ */
162800100708      *  Controlla la trasmissione   se completa
162900100708      * ?------------------------------------------------------------------ */
163000100708     c     Check_Trasm   Begsr
163100100708
163200100708     C                   clear                   se_errore
163300100708     C                   clear                   Riga_iNIZIo       1
163400100708     C                   clear                   Riga_fINe         1
163500100708      ** primo  record
163600100708     c     *start        setll     tivin00r
163700100708      *
163800100708     c     rileggi       tag
163900100708     c                   EXSR      LEGGE_TIVIN_R
164000100708      *
164100100708     c                   if        EoFTivin <> 'S'
164200100708      *
164300100708     c                   if        dati = *blank
164400100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
164500100708      * le leggo e le escludo.
164600100708     C                   eval      vinFLG = '1'
164700100708     c                   update    Tivin000
164800100708     c                   goto      rileggi
164900100708     c                   end
165000100708      * ?              /*---------------------- */
165100100708     c                   exsr      Decod_Segmento
165200100708      * ?              /*---------------------- */
165300100708     c                   endif
165400100708      **
165500100708      ** ultimo record
165600100708     c     *hival        setll     tivin00r
165700100708     c     leggiAncora   tag
165800100708     c                   readp     tivin00r
165900100708     c                   if        %Eof(tivin00r)
166000100708     c                   move      'S'           EoFTivin
166100100708     c                   else
166200100708     c                   clear                   dati
166300100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
166400100708      *
166500100708     c                   if        dati = *blank
166600100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
166700100708      * le leggo e le escludo.
166800100708     C                   eval      vinFLG = '1'
166900100708     c                   update    Tivin000
167000100708     c                   goto      leggiAncora
167100100708     c                   end
167200100708      * ?              /*---------------------- */
167300100708     c                   exsr      Decod_Segmento
167400100708      * ?              /*---------------------- */
167500100708     c                   endif
167600100708
167700100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
167800100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
167900100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
168000100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
168100100708      * ?-----> Errore
168200100708     C                   eval      se_errore = 'S'
168300100708     c                   end
168400100708
168500100708     c                   endSR
168600100712     c*==================================================================*
168700100712     C*? CNVDAT - DECODIFICA LA DATA
168800100712     C* INPUT:  - WFORM  (FORMATO DATA : 102)
168900100712     C*         - WDATA  (DATA ALFANUMERICA)
169000100712     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
169100100712     C*         - WHMS   (ORA NUMERICA)
169200100712     C*----------------------------------------------------------------
169300100712     C     Converte_Data BEGSR
169400100712     C*
169500100712     C                   MOVEL     ' '           WERRO             1
169600100712     C                   Z-ADD     *ZEROS        WCAMG             8 0
169700100712     C                   Z-ADD     *ZEROS        WCAMGora         14 0
169800100712     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
169900100712     C                   Z-ADD     *ZEROS        WHMS              6 0
170000100712     C*
170100100712     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
170200100712     C                   MOVEL     Work_Data__35 WCOMO8            8
170300100712     C                   TESTN                   WCOMO8               99
170400100712     C   99              MOVE      WCOMO8        WCAMG                          *DATA
170500100712     C  N99              MOVEL     'S'           WERRO             1
170600100712     C                   END
170700100712     C*
170800100712     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
170900100712     C                   MOVEL     Work_Data__35 WCOMO12          12
171000100712     C                   TESTN                   WCOMO12              99
171100100712     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
171200100712     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
171300100712     C   99              MOVE      WCAMGORA      WHMS                           *DATA
171400100712     C  N99              MOVEL     'S'           WERRO             1
171500100712     C                   END
171600100712     C*
171700100712     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
171800100712     C                   MOVEL     Work_Data__35 WCOMO14          14
171900100712     C                   TESTN                   WCOMO14              99
172000100712     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
172100100712     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
172200100712     C   99              MOVE      WCAMGORA      WHMS                           *DATA
172300100712     C  N99              MOVEL     'S'           WERRO             1
172400100712     C                   END
172500100712     C*
172600100712     C                   ENDSR
172700100712      * ?------------------------------------------------------------------ */
172800100712      *?    Flagga il TIVIN come messaggio completamente ERRATO
172900100712      * ?------------------------------------------------------------------ */
173000100712     C     MSG_Errato    BEGSR
173100100712      *
173200100712      * ?  Scrive su tutti i records il tipo di errore
173300100712     c     *start        setll     tivin00r
173400100712     c
173500100712     c                   EXSR      LEGGE_TIVIN_R
173600100712     c                   dow       EoFTivin <> 'S'
173700100712     c
173800100712     c                   if        vinMSG  = *blank
173900100712     C                   evalR     vinMSG  = 'MSG IFTSTA ricevuto Anomalo +
174000100712     C                               >> Controllare i DATI su UPLOAD !!'
174100100712     c                   end
174200100712     C                   eval      vinFLG = '2'
174300100712     c                   eval      Almeno_Un_Due ='S'
174400100712     c                   eval      esito  = '2'
174500100712     c                   eval      se_errore ='S'
174600100712     c                   eval      err_bloccante ='S'
174700100712      *
174800100712     c                   update    tivin000
174900100712     c                   EXSR      LEGGE_TIVIN_R
175000100712     c                   endDO
175100100712      *
175200100712     C                   ENDSR
175300100712      * ?------------------------------------------------------------------ */
175400100712      *?    Flagga il TIVIN come messaggio completamente ERRATO
175500100712      * ?------------------------------------------------------------------ */
175600100712     C     DETta_Errato  BEGSR
175700100712      *
175800100712     c                   EXSR      LEGGE_TIVIN_R
175900100712     c                   dow       EoFTivin <> 'S'
176000100712      *
176100100712      * Se ha letto il successivo record
176200100712      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
176300100712     c                   if        VNREC = UNT_record  or
176400100712     c                             VNREC = finDET_RECord
176500100712     c                   leave
176600100712     c                   end
176700100712      *
176800100712     c                   exsr      Error_DET
176900100712      *
177000100712     c                   if        vinMSG  = *blank
177100100712     C                   evalR     vinMSG  = 'Dettaglio IFTSTA ERRATO -
177200100712     C                             >> Controllare i DATI di ogni Segmento <<'
177300100712     c                   end
177400100712      *
177500100712     c                   update    tivin000
177600100712     c                   EXSR      LEGGE_TIVIN_R
177700100712     c                   endDO
177800100712      * ------
177900100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
178000100712     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
178100100712???? C                             STATI IFTSTA non totalmente tradotto. -
178200100712     C                             Controllare UPLOAD del cliente:'
178300100712     C                   EVAL      Messaggio = %trim(Messaggio) +
178400100712     C                                         %editc(§PTksc:'X')
178500100712     c                   if        §PVinv <> 'N'
178600100712     c                   exsr      invio_mail
178700100712     c                   end
178800100712      *
178900100712      * ripulisce per un nuovo giro
179000100712     C                   clear                   Err_in_detta
179100100712      *
179200100712     C                   ENDSR
179300121105      *---------------------------------------------------------------*
179400121105      * CTRL caricamento schiere    PT                               -*
179500121105      *---------------------------------------------------------------*
179600121105     C     Check_PT      begsr
179700121105     C*
179800121105     c* Controllo riempimento schiera
179900121105     c                   clear                   trul0sds
180000121105     c                   eval      i0sski='PT_key'
180100121105     c                   eval      i0sele=%elem(PT_key)
180200121105     c                   eval      i0spie=x
180300121105     c                   eval      i0sfile='EDTAB00F'
180400121105     c                   eval      i0ssif=knsif
180500121105     c                   eval      i0spgm='TRTCT14R'
180600121113     c                   move      kpjbu         SvKpjbu
180700121113     c                   clear                   kpjbu
180800121105     c                   movel     trul0sds      kpjbu
180900121105     c                   call      'TRUL0SR'
181000121105     c                   parm                    kpjba
181100121113     c                   eval      kpjbu  =  SvKpjbu
181200121105     C*
181300121105     C                   ENDSR
181400100712      * ?------------------------------------------------------------------ */
181500090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
181600090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
181700090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
181800090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
