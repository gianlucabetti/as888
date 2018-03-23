000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400090211      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600100712????  *  TRASCODIFICA : MESSAGGI EDI  -  STATI BOLLE EXPORT   IFTSTA vers D94A  *
000700060612      **?************************************************************************
000800100712      * Il pgm crea:  RCVIFTSTA                                                 *
000900060609      **?************************************************************************
001000100723     Ftivin00r  uF   E             DISK    INFDS(VINRECDS) usropn
001100100722     FEDTAB01L  IF   E           K DISK
001200101018     FtrtcT94W  O  A E           K DISK    INFDS(SRECDS)
001300100701      *----------------------------------------------------*
001400100701      * Stringhe per conversione alfanumerico/numerico N.Documento
001500100701     D CA              S             78    DIM(1) CTDATA PERRCD(1)
001600100701     D CN              S             78    DIM(1) CTDATA PERRCD(1)
001700940321      *----------------------------------------------------*
001800090209      * numero relativo di record
001900090209     D SRECDS          DS
002000090209     D  NRREC                397    400B 0
002100090209      *
002200090213      * numero relativo di record
002300090213     D VINRECDS        DS
002400090213     D  VNREC                397    400B 0
002500100701      * ---------------------------------------------------
002600100701     D KPJBA         E DS
002700100701     D     svkpjbu     s                   like(kpjbu)
002800100701     D UTEDSE0F      E DS
002900100701     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003000100701     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003100100701      * Ds scomposzione tipo capoconti
003200100701     D TCUDS           DS
003300100701     D  F34                    3      4
003400100701     D  F56                    5      6
003500100722      *----------------------------------------------------*
003600100701     D EDIDSPT       E DS
003700100701     D EDIDSPU       E DS
003800100701     D EDIDSPV       E DS
003900100722     D EDIDSMS       E DS
004000100701      **
004100100701     D WLBDA8          DS
004200100701     D  G02DAT                 1      8  0
004300100701     D  G02INV                 9     16  0
004400100701     D  G02ERR                17     17
004500100701     D  G02TGI                18     22  0
004600100712     D*---------------------------------------------------------------------*
004700100712     D* DS
004800100712     D*---------------------------------------------------------------------*
004900100712     D* .. per scompozione dati da spedire a seconda del tipo record
005000100712     D EDST00        E DS                  EXTNAME(EDST00DS)
005100100712     D EDST05        E DS                  EXTNAME(EDST05DS)
005200100712     D EDST10        E DS                  EXTNAME(EDST10DS)
005300100712     D EDST15        E DS                  EXTNAME(EDST15DS)
005400100712     D EDSD00        E DS                  EXTNAME(EDSD00DS)
005500100712     D EDSD05        E DS                  EXTNAME(EDSD05DS)
005600100712     D EDSD10        E DS                  EXTNAME(EDSD10DS)
005700100712     D EDSD15        E DS                  EXTNAME(EDSD15DS)
005800100712     D EDSD20        E DS                  EXTNAME(EDSD20DS)
005900100712     D*---------------------------------------------------------------------*
006000100712     D* .. per reperimento tipo record di EDIFTSTA da scrivere
006100100712     D*                    .. 5 - 8  : tipo record
006200100712     D WDAT            DS          1950
006300100712     D  STAPRG                 1      4  0
006400100712     D  STATPR                 5      8
006500100712     D*---------------------------------------------------------------------*
006600090213      *
006700100701      *----------------------------------------------------*
006800090205      **  Elenco DS di tutti i Segmenti da tradurre
006900100701      *----------------------------------------------------*
007000100712???? D edS00UNA      E DS
007100100712???? D edS00UNB      E DS
007200100712???? D edS00UNH      E DS
007300100712???? D edS00UNT      E DS
007400100712???? D edS00UNZ      E DS
007500091019???? D edS00BGM      E DS
007600100316???? D edS00CNI      E DS
007700091019???? D edS00DTM      E DS
007800100628???? D edS00FTX      E DS
007900091019???? D edS00GID      E DS
008000100316???? D edS00LOC      E DS
008100091019???? D edS00NAD      E DS
008200091019???? D edS00PCI      E DS
008300091019???? D edS00RFF      E DS
008400100712???? D edS00STS      E DS
008500100722      **---------------------------------------------------*
008600090209     D Valori_inErr    ds
008700090209     D  SKout_Errori                  1    DIM(50)
008800100630      **
008900090209     D Descr_Errore    ds
009000090209     D  SKout_DesErr                 50    DIM(50)
009100090205      *----------------------------------------------------*
009200091016     D EoFTivin        s              1
009300091016      *----------------------------------------------------*
009400090205     D Tipo_seg        S              3
009500100721     D Trovato_seg     S              1
009600100716      *
009700100716     d keyUNBCLI       s             35
009800100716     d keyTIPOMSG      s              6
009900100716     d keyVERSION      s              3
010000100716     d keyRELEASE      s              3
010100100716     d keyAGENCY       s              3
010200100716     d keyASSOCIA      s              6
010300100716      *
010400100721     D DsGenerica      s           2048
010500090209     D segmento        s           2048
010600090209     D Errore          s              1
010700100722      *------------------------------------------
010800000223     D W0140           S             14  0
010900991129     D WORA            S              6  0
011000100701     D DataGGMMAAA     S              8  0
011100100701     D DATEU           S              8  0
011200100701     D DATA_eur        S               D   DATFMT(*eur)
011300100723     D Work_Data__35   S             35
011400100723     D Work_Format_3   S              3
011500100722      *------------------------------------------
011600100708     D UNT_record      s                   LIKE(vnREC)
011700100708     D IniDET_RECord   s                   LIKE(vnREC)
011800100708     D FinDET_RECord   s                   LIKE(vnREC)
011900090213     D Last_RECord     s                   LIKE(vnREC)
012000110110     D UNZ_SEGMENTO    s              1
012100100722      *------------------------------------------
012200100722     D nad3035SF       s                   LIKE(NAD3035)
012300100723     D nad3036SF       s                   LIKE(NAD30361)
012400100722     D nad3039SF       s                   LIKE(NAD3039)
012500100722     D nad3035ST       s                   LIKE(NAD3035)
012600100723     D nad3036ST       s                   LIKE(NAD30361)
012700100722     D nad3039ST       s                   LIKE(NAD3039)
012800100722      *
012900100722     D LOC3227t1       s                   LIKE(LOC3227)
013000100722     D LOC3225t1       s                   LIKE(LOC3225)
013100100722     D LOC3227t2       s                   LIKE(LOC3227)
013200100722     D LOC3225t2       s                   LIKE(LOC3225)
013300100722     D dtm2005te       s                   LIKE(dtm2005)
013400100722     D dtm2380te       s                   LIKE(dtm2380)
013500100722     D dtm2379te       s                   LIKE(dtm2379)
013600100722     D rff1153te       s                   LIKE(rff1153)
013700100722     D rff1154te       s                   LIKE(rff1154)
013800100318      *------------------------------------------
013900100318      **  Definizione Qualificatori
014000100318      *------------------------------------------
014100100705???? D Data_senza_ora...
014200100628???? D                 s              3    INZ('102')
014300100705???? D Data_con_ora...
014400100628???? D                 s              3    INZ('203')
014500100705???? D Data_con_i_secondi...
014600100705???? D                 s              3    INZ('204')
014700100316      *---------------------------------------------------------------------*
014800100316      **  segmenti Identificativi parti specifiche del messaggio
014900100316      *---------------------------------------------------------------------*
015000100701???? D Segm_Inizio_Messaggio...
015100100701???? D                 s              3    INZ('UNB')
015200100701???? D Segm_Testata_Dettagli...
015300100701???? D                 s              3    INZ('UNH')
015400100701???? D Segm_Fine_messaggio...
015500100316???? D                 s              3    INZ('UNZ')
015600100722???? D Segm_Inizio_Dettaglio...
015700100722???? D                 s              3    INZ('CNI')
015800100722???? D Segm_Fine_Dettagli...
015900100722???? D                 s              3    INZ('UNT')
016000100722???? D Segm_Data_Dettaglio...
016100100722???? D                 s              3    INZ('DTM')
016200100722???? D Segm_Address_Dettaglio...
016300100722???? D                 s              3    INZ('NAD')
016400100722???? D Segm_Text_Dettaglio...
016500100722???? D                 s              3    INZ('FTX')
016600100722???? D Segm_Parcels_Dettaglio...
016700100722???? D                 s              3    INZ('PCI')
016800100701???? D seg_imprevisto...
016900100701???? D                 s              1
017000100723???? D eseguire_ROLBACK...
017100100723???? D                 s              1
017200100726???? D Forza_Uscita...
017300100726???? D                 s              1
017400090209      *---------------------------------------------------------------------*
017500100701???? D Contatore_Dettagli...
017600100701???? D                 s              5s 0 INZ(0)
017700100701      *---------------------------------------------------------------------*
017800100628      *
017900100701      *---------------------------------------------------------------------*
018000100630     d UNB_diWork      s                   like(UNB0004)
018100100628     d  UNB_Mittente   s                   like(UNB0004)
018200100712     d UNB_parziale    s              1    inz('N')
018300100712     d UNB_Parz        s             35    inz(' ')
018400100712     d UNB_Generico    s             35    inz(' ')
018500100712     d UNB_NrTrasmiss  s             14    inz(' ')
018600100712      *
018700100701      * Tabella partner
018800121105     D PT_key          S             35    DIM(200)
018900121105     D PT_Parz         S             35    DIM(%elem(PT_key))
019000121105     D PT_KSC          S              7    DIM(%elem(PT_key))
019100121105     D PT_uni          S             90    DIM(%elem(PT_key))
019200121105     D PU_uni          S             90    DIM(%elem(PT_key))
019300121105     D PV_uni          S             90    DIM(%elem(PT_key))
019400121105     D TRUL0SDS      E DS
019500100701      *
019600100701     d PT_KeyXsav      s              3  0 inz(0)
019700100701     D PT_trovata      s              1    inz('N')
019800100701     D PU_key          S             35
019900100701     D PV_key          S             35
020000100701     D Nr_PT           S              3  0 INZ
020100100701      *
020200100702     d  CNI_Identificativo_Bolla...
020300100702     d                 s             35A
020400090209      *---------------------------------------------------------------------*
020500090209      * Schiere
020600090209      *---------------------------------------------------------------------*
020700090209      * Nr. documento alfanumerico da decodificare
020800090209     D NDA             S              1    DIM(35)
020900100630      *
021000090209      * Nr. documento alfanumerico da già decodificato
021100090209     D NDN             S              1    DIM(15)
021200060608      **?------------------------------------------------------------------ */
021300090209      *  Invio E-mail
021400090209     D Indirizzi       s            253
021500090209     D Oggetto         s             44
021600090209     D Messaggio       s           5000
021700090209      *
021800090209     d   DSmail        ds
021900090209     D Mail_Ind                      44
022000090209     d   IND_char                     1    dim(44)
022100090209      *
022200100630     D Lunghezza_Indirizzo...
022300100630     D                 s              5u 0
022400090209      *
022500100630     D dalCarattere    s              5u 0
022600100630     D xTOTcaratteri   s              5u 0
022700090209      *
022800090209     C*---------------------------------------------------------------*
022900090209     D MsgDSP          S            256                                         Testo generico
023000090209     C*---------------------------------------------------------------*
023100090209     D SndMg01         S             10                                         Message type
023200090209     D                                     INZ('*INFO')
023300090209     D SndMg02         S             10                                         Deluvery mode
023400090209     D                                     INZ('*BREAK')
023500090209     D SndMg03         S            256                                         Message text
023600090209     D SndMg04         S             10I 0                                      Length of text
023700090209     D                                     INZ(%SIZE(SndMg03))
023800090209     D SndMg05         S             10                                         User profile
023900090209     D                                     DIM(299)
024000090209     D SndMg06         S             10I 0                                      Number of user
024100090209     D SndMg07         S             10I 0                                      Message sent indic.
024200090209     D SndMg08         S             10I 0                                      Function requested
024300090209     D SndMg10         S              1                                         Show display
024400090209     D                                     INZ('N')
024500090209     D SndMg11         S             20                                         Qualified MSGQ name
024600090209     D SndMg12         S              4                                         Name type indicator
024700090209     D                                     INZ('*USR')
024800090209      * indice di scihera
024900090209     D Jx              S              5I 0
025000090209      *
025100090209     D/COPY QSYSINC/QRPGLESRC,QUSEC
025200090209      *
025300130313     d bart_it         C                   CONST('@Brt.it;')
025400130313     d CED_Bart        C                   CONST('CEDALERT@Brt.it;')
025500060612      * ?================================================================== */
025600060612      * ?   * Campi x decodifica * (INPUT  del Record)
025700100319      * ?================================================================== */
025800090130     D  Dati           s           2048
025900100722      *
026000060612     D  se_errore      s              1    inz(' ')
026100060612      * ?* ------------------------------------------------------ *
026200060710     D Digits          C                   '0123456789'
026300091019     D*------------------
026400091019     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
026500091019     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
026600060612      * ?================================================================== */
026700060612      *   Ciclo principale
026800090205      * ?================================================================== */
026900060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
027000060612     C*
027100060612     C                   if        not %open(tivin00r)
027200060612     C                   open      tivin00r
027300060612     C                   endif
027400060612      **
027500060612      * ?------------------------------------------------------------------ */
027600100712     C*  Controllo dati arrivati
027700060612      * ?------------------------------------------------------------------ */
027800060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
027900060612      * ?- controllando che la trasmissione sia completa.
028000090205      **
028100060612      * ?              /*---------------------- */
028200090205     c                   goto      nonFare_adesso
028300060612     c                   exsr      check_Trasm
028400090205     c     nonFare_adessotag
028500060612      * ?              /*---------------------- */
028600090205      **
028700060613      **  Errore di trasmissione x tutti i records
028800060613      **   --> file in errore
028900090205      ** Aggiorna il TIVIN completamente come messaggio tutto errato
029000100712     c                   if        se_errore = 'S'
029100090205     c                   exsr      Msg_errato
029200060612     c                   else
029300060614      **
029400060612      * ?------------------------------------------------------------------ */
029500100708     C*  Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
029600060612      * ?------------------------------------------------------------------ */
029700100722      *
029800060612      * ?              /*---------------------- */
029900060612     c                   exsr      Importa_Msg
030000060612      * ?              /*---------------------- */
030100060614     c                   end
030200060612      *
030300100723     C                   if          eseguire_ROLBACK = 'S'
030400130313     c                   ROLBK
030500100723     C*
030600100723     C                   eval      esito = '2'
030700100723???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
030800100723     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
030900100723     C                             STATI IFTSTA - messaggio -
031000100723     C                             EDI ricevuto errato su UPLOAD.'
031100100723     c                   if        §PVinv <> 'N'
031200100723     c                   exsr      invio_mail
031300100723     c                   end
031400100723     C
031500100723      *
031600100723     c                   else
031700060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
031800090225     c                   if        (almeno_uno ='S' and esito ='1' ) or
031900090225     c                             esito ='0'
032000060710     C                   eval      esito ='0'
032100090225      *
032200130313     c                   COMMIT
032300090224      *
032400090213     c                   if        almeno_un_due ='S'
032500090213     C                   eval      esito ='1'
032600060614     C                   endif
032700090213     C                   endif
032800100723     C                   end
032900100708      * chiude TIVIN
033000100708     C                   if        %open(tivin00r)
033100100708     C                   close     tivin00r
033200100708     C                   endif
033300060614      *
033400060612     c                   SETON                                        LR
033500060612      * ?================================================================== */
033600100708      *? Importa i records della trasmissione
033700060612      * ?------------------------------------------------------------------ */
033800060612     c     Importa_Msg   Begsr
033900060614
034000130506      *   Inizia il conteggio totale delle righe
034100130506     c                   z-add     0             total_segmenti    5 0
034200100726     c                   clear                   Forza_Uscita
034300060612     c     *start        setll     Tivin00r
034400100708
034500100708     c                   EXSR      LEGGE_TIVIN_R
034600100708     c                   dow       EoFTivin <> 'S'
034700060614
034800060614      * solo i record sflaggati da rielaborare
034900110107     c                   IF        vinFLG = *blank
035000110107      *** ++++++
035100090211      *  Sempre Record OK
035200090211      ** Controlli formali sui campi
035300090211     C                   eval      vinFLG = '1'
035400060612     c                   clear                   se_errore
035500110107      *** >>>>>>
035600110107     c                   IF        vinDTA <> *blank
035700110107      *** >>>>>>
035800100723      **
035900100723      **  Ad ogni inizio Dettaglio incrementa il contatore dei dettagli
036000100723     c                   If           tipo_seg = Segm_Inizio_Dettaglio
036100100723     c                   eval        Contatore_Dettagli = Contatore_Dettagli + 1
036200100723      **
036300100723      ** <Memorizza> fin dove deve aggiornare il singolo dettaglio:
036400100723     c                   eval      FinDET_RECord = vnrec
036500100723      **
036600100723     c*  Scrivere la Testata
036700100723      * SOLO ALLA SCRITTURA del 1° dettaglio
036800100723     c                   If           Contatore_Dettagli = 1
036900100723     C                   EXSR      TESTAMSG
037000100723     c                   end
037100100723     c                   endIF
037200060612
037300060612      ** Decodifica record a campi variabili
037400060612      * ?              /*---------------------- */
037500090205     c                   exsr      Decod_Segmento
037600090209      * ?              /*---------------------- */
037700060612
037800060621      *  x errore bloccante nella composizione del VAB/VAT
037900060621     c                   if        err_bloccante ='S'
038000060621     C                   eval      vinFLG = '2'
038100090211     c                   eval      esito  = '2'
038200100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
038300090603     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
038400100712     C                             STATI su Bolle export - messaggio -
038500090603     C                             con UNB non presente in tabella PT controlla-
038600090603     C                             re EDI ricevuto su UPLOAD.'
038700100319     c                   if        §PVinv <> 'N'
038800100319     c                   exsr      invio_mail
038900060621     c                   end
039000100319     c                   end
039100060612
039200110107      *** >>>>>>
039300110107     c                   endIF
039400110107      *** >>>>>>
039500110107      *   Deve aggiornare la RIGA VUOTA  flaggandola '1' ,  altrimenti
039600110107      *     viene invato dall'UPLOAD GENERALE DEL VAS TIVIN00R
039700110107      *         un erroneo messaggio
039800110107      *     di errore come se tutto il messaggio fosse ricevuto errato.
039900060612     c                   update    Tivin000
040000090211
040100090211      *** se non è riconosciuto l'UNB deve uscire direttamente
040200090211     c                   if        se_errore ='S'  and
040300100708     c                             tipo_seg = Segm_Inizio_Messaggio
040400090213     c                   eval      err_bloccante = 'S'
040500090211     c                   exsr      Msg_errato
040600090211     c                   LEAVE
040700090211     c                   endIF
040800110107      ***
040900110107      *** ++++++
041000110107     c                   endIF
041100060612
041200100708     c                   EXSR      LEGGE_TIVIN_R
041300060612     C                   ENDdo
041400060612      **
041500060612     c                   endSR
041600100709      * ?------------------------------------------------------------------ */
041700100709      *?    Legge il TIVIN eseguendo subito delle operazioni Essenziali
041800100709      * ?------------------------------------------------------------------ */
041900100709     C     LEGGE_TIVIN_R BEGSR
042000100709      *
042100100709     c                   clear                   EoFTivin
042200100709      *  Lettura sequenziale
042300100709     c                   read      Tivin00r
042400100709
042500100709     c                   if        %Eof(tivin00r)
042600100709     c                   move      'S'           EoFTivin
042700100709     c                   else
042800100709      *
042900100709      * ?  imposta il tipo di segmento:
043000100709     c                   clear                   dati
043100100709     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
043200100728     c                   eval      segmento = %trimL(dati)
043300100728     c                   eval      tipo_seg = %subst(segmento:1:3)
043400100709      **
043500100709     c                   end
043600100709      *
043700100709     C                   ENDSR
043800100120      * ?------------------------------------------------------------------ */
043900100120      *?    Decodifica il segmento ed importa i campi in formato DS
044000100120      * ?------------------------------------------------------------------ */
044100100120     c     Decod_SegmentoBegsr
044200100708      * a livello msg/testata dett./singolo Dettaglio pulisce i campi
044300100708      **?              /*---------------------- */
044400100722     c                   exsr      Pulizia_campi
044500100708      **?              /*---------------------- */
044600100722      *
044700100706      *** ------------------------------------
044800100120      * ?  Occorre elencare comunque tutti i segmenti previsti nel messaggio
044900100120      * ?  Anche se non utilizzati.
045000100712      *** ------------------------------------
045100100120      *   ?  IMPORTANTE per riconoscere se arrivano SEGMENTI non previsti...  ?
045200100317     c                   clear                   seg_imprevisto
045300100721     c                   clear                   trovato_seg
045400100712      *** ------------------------------------
045500100319      *
045600100319      *  Contatore delle righe ossia dei segmenti del messaggio
045700100319     c                   add       1             Conta_segmenti
045800130506     c                   add       1             total_segmenti
045900100708      *
046000100716     C                   clear                   keyUNBCLI
046100100716     C                   clear                   keyTIPOMSG
046200100716     C                   clear                   keyVERSION
046300100716     C                   clear                   keyRELEASE
046400100716     C                   clear                   keyAGENCY
046500100716     C                   clear                   keyASSOCIA
046600100716      *
046700100721      **--------
046800100721      *  Ritorna la DS GENERICA oppure l'errore
046900100721      **--------
047000100721      *  Chiamata generica x decodificare il singolo segmento letto
047100100721     c                   call      'TRTCT00R1'
047200100721      * input
047300100721     c                   parm                    tipo_seg
047400100721     c                   parm                    segmento
047500100721     C                   parm                    keyUNBCLI
047600100721     C                   parm                    keyTIPOMSG
047700100721     C                   parm                    keyVERSION
047800100721     C                   parm                    keyRELEASE
047900100721     C                   parm                    keyAGENCY
048000100721     C                   parm                    keyASSOCIA
048100100721      *output
048200100721     c                   parm                    Errore
048300100721     c                   parm                    DsGenerica
048400100721     c                   parm                    Valori_inErr
048500100721     c                   parm                    Descr_Errore
048600100721     c                   parm                    trovato_seg
048700100721      *
048800100708      *** ------------------------------------
048900100708      *   Decodifica il singolo segmento LETTO
049000100708      *** ------------------------------------
049100100120     c                   select
049200100120      *
049300110110     c                   when      trovato_seg ='N' and unz_segmento <>'S'
049400100723     C                   eval        eseguire_ROLBACK = 'S'
049500100721     c                   move      'S'           seg_imprevisto
049600100721     C                   eval      vinMSG = tipo_seg + ': Segmento NON trovato!'
049700100721     c                   exsr      Error_DET
049800100721     c                   goto      end_Decod
049900100721      **--------
050000100723     c                   when      errore <> ' '
050100100723     C                   eval        eseguire_ROLBACK = 'S'
050200100723     C                   eval      vinMSG = tipo_seg +Valori_inErr +Descr_Errore
050300100723     c                   exsr      Error_DET
050400100723     c                   goto      end_Decod
050500100723      **--------
050600100120     c                   when      tipo_seg = 'UNA'
050700100721     c                   movel(p)  DsGenerica    EDS00UNA
050800100712      * --------
050900100120     c                   when      tipo_seg = 'UNB'
051000100721     c                   movel(p)  DsGenerica    EDS00UNB
051100100120     C                   EXSR      DATI_UNB
051200100120      * --------
051300100120     c                   when      tipo_seg = 'UNH'
051400100721     c                   movel(p)  DsGenerica    EDS00UNH
051500100120     C                   EXSR      DATI_UNH
051600100120      * --------
051700100120     c                   when      tipo_seg = 'BGM'
051800100721     c                   movel(p)  DsGenerica    EDS00BGM
051900100316     C                   EXSR      DATI_BGM
052000100120      **--------
052100100120     c                   when      tipo_seg = 'DTM'
052200100721     c                   movel(p)  DsGenerica    EDS00DTM
052300100120     C                   EXSR      DATI_DTM
052400100120      * --------
052500100712     c                   when      tipo_seg = 'STS'
052600100721     c                   movel(p)  DsGenerica    EDS00STS
052700100712     C                   EXSR      DATI_STS
052800100120      **--------
052900100120     c                   when      tipo_seg = 'RFF'
053000100721     c                   movel(p)  DsGenerica    EDS00RFF
053100100120     C                   EXSR      DATI_RFF
053200100316      **--------
053300100316     c                   when      tipo_seg = 'LOC'
053400100721     c                   movel(p)  DsGenerica    EDS00LOC
053500100316     C                   EXSR      DATI_LOC
053600100120      **--------
053700100120     c                   when      tipo_seg = 'FTX'
053800100721     c                   movel(p)  DsGenerica    EDS00FTX
053900100120     C                   EXSR      DATI_FTX
054000100120      **--------
054100100120     c                   when      tipo_seg = 'NAD'
054200100721     c                   movel(p)  DsGenerica    EDS00NAD
054300100120     C                   EXSR      DATI_NAD
054400100316      **--------
054500100316     c                   when      tipo_seg = 'CNI'
054600100721     c                   movel(p)  DsGenerica    EDS00CNI
054700100316     C                   EXSR      DATI_CNI
054800100120      **--------
054900100120     c                   when      tipo_seg = 'GID'
055000100721     c                   movel(p)  DsGenerica    EDS00GID
055100100120     C                   EXSR      DATI_GID
055200100120      **--------
055300100120     c                   when      tipo_seg = 'PCI'
055400100721     c                   movel(p)  DsGenerica    EDS00PCI
055500100120     C                   EXSR      DATI_PCI
055600100120      **--------
055700100120     c                   when      tipo_seg = 'UNT'
055800100721     c                   movel(p)  DsGenerica    EDS00UNT
055900100120     C                   EXSR      DATI_UNT
056000100120      **--------
056100100120     c                   when      tipo_seg = 'UNZ'
056200100721     c                   movel(p)  DsGenerica    EDS00UNZ
056300100120     C                   EXSR      DATI_UNZ
056400110110     c                   move      'S'           unz_segmento
056500110110      **--------
056600110110     c                   When      tipo_seg = *blank and UNZ_segmento = 'S'
056700100120      **--------
056800100120     c                   OTHER
056900100120      **--------
057000110110     c                   if           unz_segmento <>'S'
057100100723     C                   eval        eseguire_ROLBACK = 'S'
057200100120     c                   move      'S'           seg_imprevisto
057300100120     C                   eval      vinMSG = tipo_seg + ': Segmento NON previsto'
057400100120     c                   exsr      Error_DET
057500100120     c                   goto      end_Decod
057600110110     c                   end
057700100120      **--------
057800100120     c                   endSL
057900100120      **
058000100120     c     end_Decod     endSR
058100100701      * ?================================================================== */
058200100319     C*? Azzera_MSG   - Azzera dati messaggio
058300100701      * ?================================================================== */
058400100708     C     Scrive_Dettag BEGSR
058500100701      **
058600100708      *  Deve flaggare il dettaglio con dei problemi
058700100712      * Imposta le righe in errore solo sullo specifico dettaglio
058800100708     C                   if        Err_in_detta = 'S'
058900100708      *  rilascia il record
059000100708     c                   update    tivin000
059100100708      * ?  Scrive su tutti i records il tipo di errore
059200100708     c     IniDET_RECord setll     tivin00r
059300100708      **?              /*---------------------- */
059400100708     c                   exsr      DETta_Errato
059500100708      **?              /*---------------------- */
059600100708     c                   end
059700100708      *
059800100708      **  Se presente un errore nel record emette una segnalazione msg
059900100712      *     altrimenti scrive il FLAT
060000100708???? c                   if        se_errore <> 'S'
060100100708      * ?              /*---------------------- */
060200100712     c                   exsr      Scrive_Flat
060300100708      * ?              /*---------------------- */
060400100708     c                   else
060500100708      *
060600100712      *  Problemi nella decodifica dei campi
060700100708     C                   evalR     vinMSG = 'ATTENZIONE -Problemi scrittura VAB'
060800100712     c                   eval      errori_in_Wrt = 'S'
060900100708     c                   exsr      Error_DET
061000100708     c                   end
061100100708      *
061200100708     c     end_WRidett   endSR
061300100722      * ?================================================================== */
061400100722     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
061500100722      * ?================================================================== */
061600100722     C     Scrive_FLAT   BEGSR
061700100722      *
061800100722      *  dopo il CNI
061900100722     c                   IF           tipo_seg = Segm_Inizio_Dettaglio  and
062000100722     c                                Contatore_Dettagli > 0
062100100722     C                   CLEAR                   EDSD00
062200100722     C                   MOVEL     CNI1490       DA1490
062300100722     C                   MOVEL     CNI1004       DA1004
062400100722     C                   MOVEL     CNI1312       DA1312
062500101018     C                   CLEAR                   TRTCT94R
062600100722     C                   clear                   WDAT
062700100722     C                   MOVEL     EDSD00        WDAT
062800100722     C                   MOVEL     'SD00'        STATPR
062900100723     C                   MOVEL     WDAT          FLATREC
063000101018     C                   WRITE     TRTCT94R                             99
063100100722     c                   end
063200100722      * STS/RFF/DTM
063300100722     c                   IF           tipo_seg = Segm_Data_Dettaglio  and
063400100722     c                                Contatore_Dettagli > 0
063500100722     C                   CLEAR                   EDSD05
063600100722     C                   MOVEL     STS9011       DB9011
063700100722     C                   MOVEL     STS113120     DB1131
063800100722     C                   MOVEL     STS901330     DB9013
063900100722     C                   MOVEL     STS113130     DBA131
064000100722     C                   MOVEL     RFF1153       DB1153
064100100722     C                   MOVEL     RFF1154       DB1154
064200100722     C                   MOVEL     DTM2005       DB2005
064300100722     C                   MOVEL     DTM2380       DB2380
064400100722     C                   MOVEL     DTM2379       DB2379
064500101018     C                   CLEAR                   TRTCT94R
064600100722     C                   clear                   WDAT
064700100722     C                   MOVEL     EDSD05        WDAT
064800100722     C                   MOVEL     'SD05'        STATPR
064900100723     C                   MOVEL     WDAT          FLATREC
065000101018     C                   WRITE     TRTCT94R                             99
065100100722     c                   end
065200100722      *  NAD
065300100722     c                   IF           tipo_seg = Segm_Address_Dettaglio  and
065400100722     c                                Contatore_Dettagli > 0
065500100722     C                   CLEAR                   EDSD10
065600100723     C                   MOVEL     NAD3035       DC3035
065700100722     C                   MOVEL     NAD30361      DC3036
065800101018     C                   CLEAR                   TRTCT94R
065900100723     C                   clear                   wdat
066000100722     C                   MOVEL     EDSD10        WDAT
066100100722     C                   MOVEL     'SD10'        STATPR
066200100723     C                   MOVEL     WDAT          FLATREC
066300101018     C                   WRITE     TRTCT94R                             99
066400100722     c                   end
066500100722      *  FTX
066600100722     c                   IF           tipo_seg = Segm_Text_Dettaglio  and
066700100722     c                                Contatore_Dettagli > 0
066800100722     C                   CLEAR                   EDSD15
066900100722     C                   MOVEL     FTX4451       DD4451
067000100722     C                   MOVEL     FTX44401      DD4440
067100100722     C                   MOVEL     FTX44402      DDA440
067200100722     C                   MOVEL     FTX44403      DDB440
067300100722     C                   MOVEL     FTX44404      DDC440
067400100722     C                   MOVEL     FTX44405      DDD440
067500101018     C                   CLEAR                   TRTCT94R
067600100722     C                   clear                   WDAT
067700100722     C                   MOVEL     EDSD15        WDAT
067800100722     C                   MOVEL     'SD15'        STATPR
067900100723     C                   MOVEL     WDAT          FLATREC
068000101018     C                   WRITE     TRTCT94R                             99
068100100722     c                   end
068200100722      *  PCI
068300100722     c                   IF           tipo_seg = Segm_Parcels_Dettaglio  and
068400100722     c                                Contatore_Dettagli > 0
068500100722     C                   CLEAR                   EDSD20
068600100722      *
068700100722     C                   MOVEL     GID1496       DE1496
068800100722     C                   MOVEL     GID722420     DE7224
068900100722     C                   MOVEL     GID706520     DE7065
069000100722      *
069100100722     C                   MOVEL     PCI4233       DE4233
069200100722     C                   MOVEL     PCI71021      DE7102
069300100722     C                   MOVEL     PCI71022      DEA102
069400100722     C                   MOVEL     PCI71023      DEB102
069500100722     C                   MOVEL     PCI71024      DEC102
069600100722     C                   MOVEL     PCI71025      DED102
069700100722     C                   MOVEL     PCI71026      DEE102
069800100722     C                   MOVEL     PCI71027      DEF102
069900100722     C                   MOVEL     PCI71028      DEG102
070000100722     C                   MOVEL     PCI71029      DEH102
070100100722     C                   MOVEL     PCI710210     DEI102
070200100722      *
070300101018     C                   CLEAR                   TRTCT94R
070400100722     C                   clear                   WDAT
070500100722     C                   MOVEL     EDSD20        WDAT
070600100722     C                   MOVEL     'SD20'        STATPR
070700100723     C                   MOVEL     WDAT          FLATREC
070800101018     C                   WRITE     TRTCT94R                             99
070900100722      *
071000100722     C                   clear                   GID1496
071100100722     C                   clear                   GID722420
071200100722     C                   clear                   GID706520
071300100722     c                   end
071400100722      *
071500100722     c                   clear                   err_bloccante     1
071600100722     c                   move      'S'           Almeno_Uno
071700100722     c   99              eval      errori_in_Wrt = 'S'
071800100722     C* >>>>>>>>>>>>
071900100722     C                   if        se_errore  = 'S'
072000100722     c                   move      'S'           err_bloccante
072100100722     c                   goto      Error_Flat
072200100722     c                   end
072300100722     C*
072400100722     C     Error_Flat    Endsr
072500100708      * ?================================================================== */
072600100722     C*? Pulizia campi -
072700100708      * ?================================================================== */
072800100722     C     Pulizia_campi BEGSR
072900100708      **
073000100712      **
073100100712      * inzio messaggio azzera tutte le variabili generali del messaggio
073200100712      * ?                         --------------
073300100712     c                   If        tipo_seg = Segm_Inizio_Messaggio
073400100712      * ?                         --------------
073500100723     C                   eval        eseguire_ROLBACK = ' '
073600100712      *  INIZIALIZZA  dati fondamentali messaggio
073700100712     C                   clear                   EDIDSPT
073800100712     C                   clear                   EDIDSPU
073900100712     C                   clear                   EDIDSPV
074000100722     C                   clear                   EDIDSMS
074100100712     C                   move      'N'           PT_trovata
074200100712     c                   clear                   UNB_Generico
074300100712     c                   clear                   UNB_Parz
074400100712     c                   clear                   UNB_NrTrasmiss
074500100712      * per controllare se almeno un record è stato importato sul VAB
074600100712     c                   clear                   Almeno_Uno        1
074700100712     c                   clear                   Almeno_Un_Due     1
074800100712     c                   clear                   IniDET_RECord
074900100712     c                   clear                   FinDET_RECord
075000100712     c                   eval      esito  = '0'
075100100722     c                   clear                   EDST00
075200100722     c                   clear                   EDST05
075300100722     c                   clear                   EDST10
075400100722     c                   clear                   EDST15
075500100722     C                   clear                   nad3035SF
075600100722     C                   clear                   nad3039SF
075700100722     C                   clear                   nad3035ST
075800100722     C                   clear                   nad3039ST
075900100722     C                   clear                   LOC3227t1
076000100722     C                   clear                   LOC3225t1
076100100722     C                   clear                   LOC3227t2
076200100722     C                   clear                   LOC3225t2
076300100722     C                   clear                   dtm2005te
076400100722     C                   clear                   dtm2380te
076500100722     C                   clear                   dtm2379te
076600100722     C                   clear                   rff1153te
076700100722     C                   clear                   rff1154te
076800100712      *
076900100712      * Testata Dettagli azzera tutte le variabili per i singoli dettagli
077000100712      * ?                         --------------
077100100712     c                   elseIf    tipo_seg = Segm_Testata_Dettagli
077200100712      * ?                         --------------
077300100712      *   Contatore dei dettagli
077400100712     c                   eval      Contatore_Dettagli = 0
077500100712      **
077600100712      *   Inizia il conteggio delle righe
077700100712     c                   z-add     0             Conta_segmenti    5 0
077800100712      *
077900100712      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
078000100712      *  Prepara una nuova spedizione
078100100712      * ?                         --------------
078200100712     c                   elseIf    tipo_seg = Segm_Inizio_Dettaglio
078300100712      * ?                         --------------
078400100708      * inzio Dettaglio azzera tutte le variabili della Singola Spedizione
078500100708      *  Prepara una nuova spedizione
078600100712     c                   clear                   EDSD00
078700100712     c                   clear                   EDSD05
078800100712     c                   clear                   EDSD10
078900100712     c                   clear                   EDSD15
079000100712     c                   clear                   EDSD20
079100100712      **
079200100712     c                   z-add     vnrec         IniDET_RECord
079300100722      **
079400100712      * Un errore nel dettaglio
079500100712     C                   clear                   Err_in_detta      1
079600100712     c                   clear                   errori_in_Wrt     1
079700100712      *
079800100723     c                   endIF
079900100708      **
080000100708     c     end_clear     endSR
080100100701      * ?================================================================== */
080200090210      *? DECODIFICA DATI UNB
080300100712      * ?================================================================== */
080400090210     C     DATI_UNB      BEGSR
080500090210      *
080600100701      * identificativo Messaggio
080700100701     C                   eval      UNB_NrTrasmiss = unb0020
080800100701      *
080900100712      * quindi Controlla esistenza UNB ricevuto del mittente se presente
081000100712      *  anche se parziale.
081100100630     C                   MOVEL     unb0004       UNB_diWork
081200100630     C                   MOVE      unb000720     UNB_diWork
081300100630     C                   MOVEl(p)  UNB_diWork    UNB_Mittente
081400100712     c                   clear                   UNB_parziale
081500100630     C                   Z-ADD     1             Nr_PT
081600100630     C     UNB_diWork    LOOKUP    PT_key(Nr_PT)                          32
081700100712      * Poi occorre verificare se il codice UNB è fra quelli parzializzati
081800100317     C                   If        *IN32 = *off
081900100630     C                   eval      Nr_PT = 1
082000100630     c                   clear                   UNB_diWork
082100100630     C                   eval      UNB_diWork = %subst(unb0004:1:31)
082200100630     C     UNB_diWork    lookup    PT_Parz(Nr_PT)                         32
082300100712     c   32              eval      UNB_parziale = 'S'
082400100712     c   32              eval      UNB_Generico = UNB_diWork
082500100712     c                   end
082600100317      *
082700100712     C                   If        *IN32 = *off
082800100319      *  Se non ha trovato UNB...Errore  x messaggio NON riconosciuto
082900130313      *   Non potendo decodificare a chi mandare la mail la INVIA a CED@Brt.it
083000090209      * Msg di allerta Traduzione
083100090209     C                   EVAL      Indirizzi = CED_Bart
083200130313???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
083300090209     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
083400090209     C                             Bolle import in filiale - messaggio -
083500090209     C                             con UNB non presente in tabella PT controlla-
083600090209     C                             re EDI ricevuto su UPLOAD.'
083700090209      *
083800090209     c                   exsr      invio_mail
083900090210     C                   eval      vinMSG = 'UNB sconosciuto al sistema'
084000090210     C                   eval      vinFLG = '2'
084100090210     C                   eval      se_errore   = 'S'
084200090210     c                   goto      end_UNB
084300090209      *
084400100630     C                   move      'N'           PT_trovata
084500090209      *
084600090209      *             *------------------------------*
084700090209     C                   eLSe
084800090209      *             *------------------------------*
084900100712      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
085000100712     c                   movel     UNB_diWork    UNB_Parz
085100100712      **
085200090209      * Se tutto OK su tab.PT:
085300100630     C                   move      'S'           PT_trovata
085400100630     C                   MOVEL     PT_uni(Nr_PT) EDIDSPT
085500100712     C                   MOVEL     PU_uni(Nr_PT) EDIDSPU
085600100712     C                   MOVEL     PV_uni(Nr_PT) EDIDSPV
085700091016     c                   clear                   Mittente         35
085800091016     c                   clear                   Mitt_Qualifier    4
085900091016     c                   clear                   Id_Messaggio     14
086000090226     C                   eval      Mittente       = unb0004
086100090226     C                   eval      Mitt_qualifier = unb000720
086200090226     C                   eval      Id_Messaggio   = UNB0022
086300090209      *
086400090209      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
086500090209      *  ricevuti:
086600090209     c                   movel     §PVema        mail_ind         44
086700090209      *
086800090209      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
086900090209     c                   if        mail_ind <> *blank
087000090209     c                   exsr      imp_ADDR_mail
087100090209     c                   end
087200100712      *
087300090209     c                   Endif
087400090209      **
087500090210     c     end_UNB       endSR
087600090227      * ?================================================================== */
087700090227     C*? Testata della singola spedizione  Inizilizza i campi dettaglio
087800090227      * ?================================================================== */
087900090227     C     DATI_UNH      BEGSR
088000100316      *
088100100701      *
088200090210     c     end_UNH       endSR
088300100701      * ?================================================================== */
088400100701      *? dati iniziali   BGM
088500100701      * ?================================================================== */
088600100701     C     DATI_BGM      BEGSR
088700100701      *
088800100712      *
088900100701     c                   endSR
089000100705      * ?================================================================== */
089100100705     C*? Imposta la Località
089200100705      * ?================================================================== */
089300100705     C     DATI_LOC      BEGSR
089400100705     C*
089500100722     c                   If        LOC3227t1 = *blank
089600100722      *
089700100722     c                   eval      LOC3227t1 = LOC3227
089800100722     c                   eval      LOC3225t1 = LOC3225
089900100722      *
090000100722     c                   else
090100100722      *
090200100722     c                   eval      LOC3227t2 = LOC3227
090300100722     c                   eval      LOC3225t2 = LOC3225
090400100722      *
090500100722     c                   end
090600100705      **
090700100705     c     end_LOC       endSR
090800100316      * ?================================================================== */
090900100316     C*? Imposta l'inizio del Dettaglio CNI
091000100316      * ?================================================================== */
091100100316     C     DATI_CNI      BEGSR
091200100316     C*
091300100702      ** Inizio Dettaglio riferimento iniziale
091400100702     c                   If        CNI1004 <> *blank
091500100702     c                   eval      CNI_Identificativo_Bolla = CNI1004
091600100702     c                   end
091700100316      **
091800100722      **?              /*========================= */
091900100722     c                   exsr      Scrive_Dettag
092000100722      **?              /*========================= */
092100100722      **
092200100316     c     end_CNI       endSR
092300100722      * ?================================================================== */
092400100722     C*? Imposta informazioni sullo STATO di Consegna
092500100722      * ?================================================================== */
092600100722     C     DATI_STS      BEGSR
092700100722     C*
092800100722      **
092900100722     c     end_STS       endSR
093000100722      * ?================================================================== */
093100100722     C*?  il Riferimento della spedizione
093200100722      * ?================================================================== */
093300100722     C     DATI_RFF      BEGSR
093400100722      **
093500100722      *
093600100722     c     end_RFF       endSR
093700100722      * ?================================================================== */
093800100722     C*? Imposta la data dell'evento
093900100722      * ?================================================================== */
094000100722     C     DATI_DTM      BEGSR
094100100722      *
094200100723     c                   If           Contatore_Dettagli > 0
094300100722      **?              /*========================= */
094400100722     c                   exsr      Scrive_Dettag
094500100722      **?              /*========================= */
094600100723     c                   end
094700100722      *
094800100722     c     end_DTM       endSR
094900100705      * ?================================================================== */
095000100705     C*?  FREE TEXT
095100100705      * ?================================================================== */
095200100705     C     DATI_FTX      BEGSR
095300100705      *
095400100723     c                   If           Contatore_Dettagli > 0
095500100722      **?              /*========================= */
095600100722     c                   exsr      Scrive_Dettag
095700100722      **?              /*========================= */
095800100723     c                   end
095900100721      *
096000100705     c     end_FTX       endSR
096100090213      * ?================================================================== */
096200090213     C*?  Anagrafica del mittente e del destinatario
096300090213      * ?================================================================== */
096400091016     C     DATI_NAD      BEGSR
096500100705      *
096600100722     c                   if        NAD3035 = 'SF '
096700100722      *
096800100722     c                   eval      nad3035SF = NAD3035
096900100722     c                   eval      nad3039SF = NAD3039
097000100722      *
097100100722     c                   elseIf    NAD3035 = 'ST '
097200100722      *
097300100722     c                   eval      nad3035ST = NAD3035
097400100722     c                   eval      nad3039ST = NAD3039
097500100722      *
097600100722     c                   end
097700100722      **
097800100723     c                   If           Contatore_Dettagli > 0
097900100722      **?              /*========================= */
098000100722     c                   exsr      Scrive_Dettag
098100100722      **?              /*========================= */
098200100723     c                   end
098300090210      *
098400090210     c     end_NAD       endSR
098500090210      * ?================================================================== */
098600090210     C*?  Dati di dettaglio della spedizione e dei colli
098700090210      * ?================================================================== */
098800090210     C     DATI_GID      BEGSR
098900100706      *
099000090210      *
099100090210     c     end_GID       endSR
099200090210      * ?================================================================== */
099300090210     C*?  Dati di dettaglio Codice a barre Segnacolli
099400090210      * ?================================================================== */
099500090210     C     DATI_PCI      BEGSR
099600090210      *
099700100723     c                   If           Contatore_Dettagli > 0
099800100722      **?              /*========================= */
099900100722     c                   exsr      Scrive_Dettag
100000100722      **?              /*========================= */
100100100723     c                   end
100200090210      *
100300090210     c     end_PCI       endSR
100400090211     C*----------------------------------------------------------------
100500090211      *? dati finali del dettaglio    UNT
100600090211     C*----------------------------------------------------------------
100700090211     C     DATI_UNT      BEGSR
100800110324      *
100900110324      * Non deve dare messaggio
101000110324     c                   goto      No_UNTctrl
101100090211      *
101200100708     c                   eval      UNT_record = vnREC
101300090213     c                   z-add     vnrec         last_RECord
101400090211      **
101500100319      **   verifica la quadratura di tutti i segmenti ricevuti
101600100319      *  se il msg non quadra invia un'allerta e lo scrive anche sul record ma
101700100319      **  senza bloccare i record precedentemente ricevuti e ormai scritti.
101800100319     C     UNT0074       IFne      Conta_segmenti
101900100319     C                   eval      vinMSG = 'UNT quadratura segmenti errata. Co-
102000100319     c                             trollare il Messaggio ricevuto'
102100100319      *
102200130313???? C                   EVAL      Oggetto ='Errori UPLOAD EDI Import IFTSTA :'
102300100319     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
102400100319     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
102500100319     C                             Bolle import in filiale - messaggio -
102600100319     C                             con quadratura UNT errata controllare EDI -
102700100319     C                             ricevuto su UPLOAD x il cliente :'
102800100319     C                   EVAL      Messaggio = %trim(Messaggio) +
102900100319     C                                         %editc(§PTksc:'X')
103000100319     C                   EVAL      Messaggio = %trim(Messaggio) +
103100100319     C                             ' i segmenti contati sono :' +
103200100319     C                             %editc(Conta_segmenti:'Z')
103300100319     c                   if        §PVinv <> 'N'
103400100319     c                   exsr      invio_mail
103500100319     c                   end
103600100319     c                   end
103700110324     c     No_UNTctrl    tag
103800100319      **
103900100319     c     End_UNT       endSR
104000100316     C*----------------------------------------------------------------
104100100316      *? dati finali     UNT
104200100316     C*----------------------------------------------------------------
104300100316     C     DATI_UNZ      BEGSR
104400100316      *
104500100726     c                   move      'S'           Forza_Uscita
104600100316      **
104700100316     c                   endSR
104800090210     C*----------------------------------------------------------------
104900051205      *   DEFINIZIONE CHIAVI                               *
105000100712     C*----------------------------------------------------------------
105100051205     C     *INZSR        BEGSR
105200051205      *
105300991129     c     *ENTRY        PLIST
105400060613     C                   parm                    esito             1
105500090209      *
105600090209     C* RECUPERO DATI DELL'UTENTE
105700090209     C                   Z-ADD     1             CODUT             1 0
105800090209     C                   CALL      'XPARUT'
105900090209     C                   PARM                    UTEDSE0F
106000090209     C                   MOVEL     RAGUT         RSUT             20
106100090209     C*
106200090209     C* RICERCA CAPOCONTI
106300090209     C                   DO        50            X
106400090209     C                   MOVE      TCU(X)        TCUDS
106500090209     C     F56           IFEQ      'CG'
106600090209     C     F34           ANDEQ     '01'
106700090209     C                   Z-ADD     KCU(X)        KCI               4 0
106800090209     C                   END
106900090209     C                   END
107000100722     C*
107100090211     c                   movel(p)  'EDPAB'       User_Msg         10
107200090211     C*
107300090209     C* IMPOSTO A ZERO VARIABILI DI PGM
107400090209     c                   move      *blank        Snd_Msg_alert     1
107500090209     C                   MOVEL     CA(1)         WCA              78
107600090209     C                   MOVEL     CN(1)         WCN              78
107700090209     C*
107800090209     C* RECUPERO DATA E ORA
107900100630     C                   TIME                    Ora_Data         14 0
108000100630     C                   MOVE      Ora_Data      G02DAT
108100100630     C                   MOVE      Ora_Data      WDATA8            8 0
108200100630     C                   MOVE      WDATA8        Anno2             2 0
108300090209     C                   MOVEL     WDATA8        DATA              6 0
108400100630     C                   MOVE      Anno2         DATA
108500090209     C                   MOVEL     *BLANK        G02ERR
108600090209     C                   CALL      'XSRDA8'
108700090209     C                   PARM                    WLBDA8
108800090209     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
108900100119     C                   Z-ADD     G02INV        WOGGI_CMR         8 0           UDATE
109000090209     C                   TIME                    WORA              6 0
109100090209      * Recupero data e ora
109200090209     C                   TIME                    W0140
109300090209      * UDATE IN GGMMAAAA
109400100630     C                   MOVE      W0140         DataGGMMAAA
109500090209      * UDATE IN AAAAMMGG
109600100630     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
109700090209     C     *iso          MOVEL     DATA_eur      dateu
109800090209      *
109900090209      * ?              /*--------------------------- */
110000090209     c                   exsr      CARICA_TABELLE
110100090209      * ?              /*--------------------------- */
110200090209      *
110300090209     C                   MOVEL     *ALL'-'       CMP132          132
110400991124      *
110500050414      *------------------
110600991124     C                   ENDSR
110700060621      * ?------------------------------------------------------------------ */
110800090209      *?    CARICA TABELLE SU SCHIERE
110900060621      * ?------------------------------------------------------------------ */
111000090209     C     CARICA_TABELLEBEGSR
111100090209      *
111200090209      * Caricamento Tabella Partner esteri
111300090209     C                   Z-ADD     0             X
111400090209     C                   MOVEL     'PT'          TABCOD
111500090209     C     TABCOD        CHAIN     EDTAB01L                           30
111600090209     C     *IN30         DOWEQ     '0'
111700090209      *
111800090209     C                   SELECT
111900090209     C     TABFLG        WHENNE    *BLANKS
112000090209      *
112100090209     C                   OTHER
112200090209     C                   ADD       1             X
112300100630     C                   MOVEL     TABKEY        PT_key(X)
112400100630     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
112500100630     C                   eval      PT_KSC(X) = %subst(TABuni:1:7)
112600100630     C                   MOVEL     TABUNI        PT_uni(X)
112700090209     C                   ENDSL
112800090209      *
112900090209     C     TABCOD        READE     EDTAB01L                               30
113000090209     C                   END
113100121105      *
113200121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
113300121105     c                   exsr      ChecK_PT
113400121105      *
113500090209      * salva quanti Codici UNB ha caricato
113600100630     c                   z-add     x             PT_KeyXsav
113700090209      *
113800090209     C                   MOVEL     'PU'          TABCOD
113900090209     C     TABCOD        CHAIN     EDTAB01L                           30
114000090209     C     *IN30         DOWEQ     '0'
114100090209      *
114200090209     C                   SELECT
114300090209     C     TABFLG        WHENNE    *BLANKS
114400090209      *
114500090209     C                   OTHER
114600100630     C                   MOVEL     TABKEY        PU_key
114700100723     C                   Z-ADD     1             X                 3 0
114800100630     C     PU_key        LOOKUP    PT_key(X)                              32
114900100630     C   32              MOVEL     TABUNI        PU_uni(X)
115000090209     C                   ENDSL
115100090209      *
115200090209     C     TABCOD        READE     EDTAB01L                               30
115300090209     C                   END
115400090209      *
115500090209      * Prosegue tab.PT e PU
115600090209     C                   MOVEL     'PV'          TABCOD
115700090209     C     TABCOD        CHAIN     EDTAB01L                           30
115800090209     C     *IN30         DOWEQ     '0'
115900090209      *
116000090209     C                   SELECT
116100090209     C     TABFLG        WHENNE    *BLANKS
116200090209      *
116300090209     C                   OTHER
116400100630     C                   MOVEL     TABKEY        PV_key
116500090209     C                   Z-ADD     1             X
116600100630     C     PV_key        LOOKUP    PT_key(X)                              32
116700100630     C   32              MOVEL     TABUNI        PV_uni(X)
116800090209     C                   ENDSL
116900090209      *
117000090209     C     TABCOD        READE     EDTAB01L                               30
117100090209     C                   END
117200100722      *
117300090209     C                   ENDSR
117400090205      * ?------------------------------------------------------------------ */
117500090205      *?      X non bloccare in nessun caso il traduttore CLIENTI
117600090205      * ?------------------------------------------------------------------ */
117700090205     C     *pssr         BEGSR
117800090205     C
117900060710     C                   eval      esito = '2'
118000130506???? C                   EVAL      Oggetto ='EDI UPLOAD Import IFTSTA 94'
118100130506      *
118200130506     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
118300130506     C                             IFCSUM D94A EDI ricevuto errato in UPLOAD -
118400130506     C                             del Cliente/Partner: ' + %editc(§PTksc:'X')
118500130506     c                             + ' verificare il '+
118600130506     c                             %editc(total_segmenti:'Z')
118700130506     c                             + ' segmento -> ' + dati
118800130506      *
118900100319     c                   if        §PVinv <> 'N'
119000100319     c                   exsr      invio_mail
119100100319     c                   end
119200060621     C
119300130506     c                   seton                                        LR
119400130506     c                   return
119500130506     C
119600090213     C                   ENDSR
119700090213      * ?------------------------------------------------------------------ */
119800090213      *?      Segnala Errore su TIVIN
119900090213      * ?------------------------------------------------------------------ */
120000090213     C     Error_DET     BEGSR
120100090213     C
120200090213     C                   eval      vinFLG = '2'
120300090213     c                   eval      Almeno_Un_Due ='S'
120400090213     c                   eval      esito  = '1'
120500090213     c                   eval      se_errore ='S'
120600090213     C                   eval      Err_in_detta = 'S'
120700090213     C
120800090213     C                   ENDSR
120900100712      *----------------------------------------------------------------
121000100712      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
121100100712      *----------------------------------------------------------------
121200100712     C     TESTAMSG      BEGSR
121300100712      *
121400100722      * Eseguo scrittura dati record ST00   (UNB/BGM/DTM)
121500100712     C                   EXSR      WRST00
121600100722      * Eseguo scrittura dati record ST05   (NAD)
121700100712     C                   EXSR      WRST05
121800100722      * Eseguo scrittura dati record ST10   (RFF/LOC)
121900100712     C***                EXSR      WRST10
122000100722      * Eseguo scrittura dati record ST15   (FTX)
122100100712     C                   EXSR      WRST15
122200100712      *
122300100712     C                   ENDSR
122400100712      *----------------------------------------------------------------
122500100712      *? WRST00 - Scrittura record partner
122600100712      *----------------------------------------------------------------
122700100712     C     WRST00        BEGSR
122800100712      *
122900100712      *  Pulisco dati DS
123000100712     C                   CLEAR                   EDST00
123100100712      *
123200100712     C                   eval        TA0004 = UNB0004
123300100712     C                   eval        TA0007 = UNB000720
123400100712      *
123500100712     C                   eval        TA0010 = UNB0010
123600100712     C                   eval        TAA007 = UNB000730
123700100712      *
123800100712      *  Data/Ora invio messagio:
123900100712     C                   eval        TA0017 = UNB0017
124000100723     C                   move      UNB0019       TA0019
124100100712      *
124200100712      *  Codice documento - nome documento
124300100712     C                   eval        TA1001 = BGM1001
124400100712     C                   eval        TA1000 = BGM1000
124500100712     C                   eval        TA1004 = BGM1004
124600100712     C                   eval        TA1225 = BGM1225
124700100712      *  Data messaggio:
124800100712     C                   eval        TA2005 = DTM2005
124900100712     C                   eval        TA2380 = DTM2380
125000100712     C                   eval        TA2379 = DTM2379
125100100712      *
125200100712      *  Scivo record IFTSTA
125300101018     C                   CLEAR                   TRTCT94R
125400100712     C                   CLEAR                   WDAT
125500100712     C                   MOVEL     EDST00        WDAT
125600100712     C                   MOVEL     'ST00'        STATPR
125700100723     C                   MOVEL     WDAT          FLATREC
125800101018     C                   WRITE     TRTCT94R                             99
125900100712      *
126000100712     C                   ENDSR
126100100712      *----------------------------------------------------------------
126200100712      *? WRST05 - Scrittura record partner
126300100712      *----------------------------------------------------------------
126400100712     C     WRST05        BEGSR
126500100712      *
126600100712      *  Ship from:
126700100722     C                   CLEAR                   EDST05
126800100722     C                   MOVEL     nad3035SF     TE3035
126900100723     C                   MOVEL     nad3039SF     TE3036
127000101018     C                   CLEAR                   TRTCT94R
127100100722     C                   CLEAR                   WDAT
127200100712     C                   MOVEL     EDST05        WDAT
127300100712     C                   MOVEL     'ST05'        STATPR
127400100723     C                   MOVEL     WDAT          FLATREC
127500101018     C                   WRITE     TRTCT94R
127600100712      *  Ship to:
127700100712     C                   CLEAR                   EDST05
127800100722     C                   MOVEL     nad3035ST     TE3035
127900100723     C                   MOVEL     nad3039ST     TE3036
128000101018     C                   CLEAR                   TRTCT94R
128100100722     C                   CLEAR                   WDAT
128200100712     C                   MOVEL     EDST05        WDAT
128300100712     C                   MOVEL     'ST05'        STATPR
128400100723     C                   MOVEL     WDAT          FLATREC
128500101018     C                   WRITE     TRTCT94R                             99
128600100712      *
128700100712     C                   ENDSR
128800100712      *----------------------------------------------------------------
128900100712      *? WRST10 - Scrittura record partner
129000100712      *----------------------------------------------------------------
129100100712     C     WRST10        BEGSR
129200100712      *
129300100722      *  scrittura condizionata
129400100722     C                   if        rff1153 <> *blank or loc3227t1 <> *blank
129500100712      *  Pulisco dati DS
129600100712     C                   CLEAR                   EDST10
129700100722     C                   MOVEL     rff1153te     TB1153
129800100722     C                   MOVEL     rff1154te     TB1154
129900100722     C                   MOVEL     LOC3227t1     TB3227
130000100722     C                   MOVEL     LOC3225t1     TB3225
130100100722     C                   MOVEL     LOC3227t2     TBA227
130200100722     C                   MOVEL     LOC3225t2     TBA225
130300101018     C                   CLEAR                   TRTCT94R
130400100722     C                   clear                   WDAT
130500100712     C                   MOVEL     EDST10        WDAT
130600100712     C                   MOVEL     'ST10'        STATPR
130700100723     C                   MOVEL     WDAT          FLATREC
130800101018     C                   WRITE     TRTCT94R                             99
130900100722     c                   end
131000100712      *
131100100712     C                   ENDSR
131200100712      *----------------------------------------------------------------
131300100712      *? WRST15 - Scrittura record partner
131400100712      *----------------------------------------------------------------
131500100712     C     WRST15        BEGSR
131600100712      *
131700100722      *  scrittura condizionata
131800100722     C                   if         FTX44401 <> *blank
131900100712      *  Pulisco dati DS
132000100712     C                   CLEAR                   EDST15
132100100722     C                   MOVEL     ftx4451       TC4451
132200100722     C                   MOVEL     FTX44401      TC4440
132300100722     C                   MOVEL     FTX44402      TCA440
132400100722     C                   MOVEL     FTX44403      TCB440
132500100722     C                   MOVEL     FTX44404      TCC440
132600100722     C                   MOVEL     FTX44405      TCD440
132700101018     C                   CLEAR                   TRTCT94R
132800100722     C                   clear                   WDAT
132900100722     C                   MOVEL     EDST15        WDAT
133000100722     C                   MOVEL     'ST15'        STATPR
133100100723     C                   MOVEL     WDAT          FLATREC
133200101018     C                   WRITE     TRTCT94R                             99
133300100722     c                   end
133400100712      *
133500100712     C                   ENDSR
133600090211     c*==================================================================*
133700090211      * Manda un Msg in Posta AS Sede a EDP*
133800090211     c*==================================================================*
133900090211     c     SEND_MSG_EDP  begsr
134000090211      *
134100090211      * INVIA MESSAGGIO ad un Utente --> EDPAB
134200090211      *   Lo manda una sola volta per lavoro
134300090211     c                   IF        Snd_Msg_alert  <> 'N' and
134400090211     c                             User_msg <> *blank
134500090211     c                   z-add     1             Jx
134600090211     C                   exsr      CalQEZSNDMG
134700090211     c                   end
134800090211      *
134900090211     c                   endsr
135000090213     c*==================================================================*
135100090213      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
135200090213     c*==================================================================*
135300090213     c     Imp_ADDR_mail begsr
135400090213      *
135500090213     c                   clear                   Indirizzi
135600090213      *
135700090213      *  Lunghezza indirizzi presenti
135800100630     c                   eval      Lunghezza_Indirizzo =
135900100630     c                                       %Len(%TrimR(Mail_Ind))
136000090213     c                   z-add     1             al_char           3 0
136100090213     c                   z-add     1             dal_char          3 0
136200090213     c                   z-add     1             tot_char          3 0
136300090213      *
136400090213     c     successivo    tag
136500090213      *
136600090213      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
136700090213      *   e aggiunge il dominio Bartolini + il (;) separatore
136800090213     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
136900090213     c                   eval      tot_char = al_char - dal_char
137000100630     c                   z-add     dal_char      dalCarattere
137100100630     c                   z-add     tot_char      xTOTcaratteri
137200090213      *
137300090213     c                   clear                   Indir_Bartol     40
137400090213     c                   clear                   Indir_Parz       20
137500100630     c                   eval      Indir_Parz =
137600100630     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
137700100630      *
137800090213     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
137900090213     c                             %Trim(bart_it)
138000090213     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
138100090213      *
138200100630     c                   if        al_char = Lunghezza_Indirizzo
138300090213     c                   goto      fine_indmail
138400090213     c                   else
138500090213     c                   eval      dal_char = ( al_char + 1 )
138600090213     c                   goto      successivo
138700090213     c                   end
138800090213      *
138900090213     C     fine_indmail  TAG
139000090213      *
139100090213     C                   ENDSR
139200090213     c*==================================================================*
139300090213      * Manda un Msg x E-mail
139400090213     c*==================================================================*
139500090213     c     Invio_Mail    begsr
139600090213      *
139700130313     C*   Alert_mail : invio Mail a CEDALERT@Brt.it                  *
139800090213     C* Inizializzo variabili
139900090213     C                   movel     *blanks       wrkEml          253
140000090213     C                   movel     *blanks       wrkMsg         5000
140100090213     C                   movel     *blanks       wrkOgg           44
140200090213      *
140300090213     C* Valorizzo i campi della e-m@ail - indirizzo
140400130313     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
140500090213     C                   eval      wrkEml = Indirizzi
140600090213     C                   eval      wrkOgg = Oggetto
140700090213     C                   eval      wrkMsg = Messaggio
140800110203      ********
140900110203     C********           call(e)   'TIS701C'
141000110203     C********           parm                    wrkEml
141100110203     C********           parm                    wrkOgg
141200110203     C********           parm                    wrkMsg
141300110203      ** *****
141400130506     C                   call      'TRTCT00R2'                          99
141500110203     C                   parm                    wrkEml
141600110203     C                   parm                    wrkOgg
141700110203     C                   parm                    wrkMsg
141800110203     C*
141900090213     C*
142000090213     C                   ENDSR
142100090603      *
142200090211     ***********************************************************************
142300090211     **
142400090211     ** Send Message (QEZSNDMG) API
142500090211     **
142600090211     ***********************************************************************
142700090211     C     CalQEZSNDMG   BEGSR
142800090211      *
142900090211     ** Invio messaggio all'utente.
143000090211     C                   EVAL      SndMg03 = msgDSP
143100090211     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
143200090211     C                   EVAL      SndMg05(Jx) = User_msg
143300090211     C                   EVAL      SndMg06 = Jx
143400090211     C                   CLEAR                   QUSEC
143500090211     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
143600090211
143700090211     C                   CALL      'QEZSNDMG'
143800090211     C                   PARM                    SndMg01
143900090211     C                   PARM                    SndMg02
144000090211     C                   PARM                    SndMg03
144100090211     C                   PARM                    SndMg04
144200090211     C                   PARM                    SndMg05
144300090211     C                   PARM                    SndMg06
144400090211     C                   PARM                    SndMg07
144500090211     C                   PARM                    SndMg08
144600090211     C                   PARM                    Qusec
144700090211     C                   PARM                    SndMg10
144800090211     C                   PARM                    SndMg11
144900090211     C                   PARM                    SndMg12
145000090211      *
145100090211     C                   ENDSR
145200100708      * ?------------------------------------------------------------------ */
145300100708      *  Controlla la trasmissione   se completa
145400100708      * ?------------------------------------------------------------------ */
145500100708     c     Check_Trasm   Begsr
145600100708
145700100708     C                   clear                   se_errore
145800100708     C                   clear                   Riga_iNIZIo       1
145900100708     C                   clear                   Riga_fINe         1
146000100708      ** primo  record
146100100708     c     *start        setll     tivin00r
146200100708      *
146300100708     c     rileggi       tag
146400100708     c                   EXSR      LEGGE_TIVIN_R
146500100708      *
146600100708     c                   if        EoFTivin <> 'S'
146700100708      *
146800100708     c                   if        dati = *blank
146900100708      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
147000100708      * le leggo e le escludo.
147100100708     C                   eval      vinFLG = '1'
147200100708     c                   update    Tivin000
147300100708     c                   goto      rileggi
147400100708     c                   end
147500100708      * ?              /*---------------------- */
147600100708     c                   exsr      Decod_Segmento
147700100708      * ?              /*---------------------- */
147800100708     c                   endif
147900100708      **
148000100708      ** ultimo record
148100100708     c     *hival        setll     tivin00r
148200100708     c     leggiAncora   tag
148300100708     c                   readp     tivin00r
148400100708     c                   if        %Eof(tivin00r)
148500100708     c                   move      'S'           EoFTivin
148600100708     c                   else
148700100708     c                   clear                   dati
148800100708     C     minu:maiu     XLATE     VINDTA        dati                           *Minus -> Maiuscolo
148900100708      *
149000100708     c                   if        dati = *blank
149100100708      * le ultime righe potrebbero essere vuote prima di finire il messaggio
149200100708      * le leggo e le escludo.
149300100708     C                   eval      vinFLG = '1'
149400100708     c                   update    Tivin000
149500100708     c                   goto      leggiAncora
149600100708     c                   end
149700100708      * ?              /*---------------------- */
149800100708     c                   exsr      Decod_Segmento
149900100708      * ?              /*---------------------- */
150000100708     c                   endif
150100100708
150200100708      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
150300100708      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
150400100708      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
150500100708     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
150600100708      * ?-----> Errore
150700100708     C                   eval      se_errore = 'S'
150800100708     c                   end
150900100708
151000100708     c                   endSR
151100100712     c*==================================================================*
151200100712     C*? CNVDAT - DECODIFICA LA DATA
151300100712     C* INPUT:  - WFORM  (FORMATO DATA : 102)
151400100712     C*         - WDATA  (DATA ALFANUMERICA)
151500100712     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
151600100712     C*         - WHMS   (ORA NUMERICA)
151700100712     C*----------------------------------------------------------------
151800100712     C     Converte_Data BEGSR
151900100712     C*
152000100712     C                   MOVEL     ' '           WERRO             1
152100100712     C                   Z-ADD     *ZEROS        WCAMG             8 0
152200100712     C                   Z-ADD     *ZEROS        WCAMGora         14 0
152300100712     C                   Z-ADD     *ZEROS        WCAMGoramin      12 0
152400100712     C                   Z-ADD     *ZEROS        WHMS              6 0
152500100712     C*
152600100712     C     Work_Format_3 IFEQ      Data_senza_ora                               CCYMMDD
152700100712     C                   MOVEL     Work_Data__35 WCOMO8            8
152800100712     C                   TESTN                   WCOMO8               99
152900100712     C   99              MOVE      WCOMO8        WCAMG                          *DATA
153000100712     C  N99              MOVEL     'S'           WERRO             1
153100100712     C                   END
153200100712     C*
153300100712     C     Work_Format_3 IFEQ      Data_con_ora                                 CCYMMDDHHMM
153400100712     C                   MOVEL     Work_Data__35 WCOMO12          12
153500100712     C                   TESTN                   WCOMO12              99
153600100712     C   99              MOVEl     WCOMO12       WCAMGORA                       *DATA
153700100712     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
153800100712     C   99              MOVE      WCAMGORA      WHMS                           *DATA
153900100712     C  N99              MOVEL     'S'           WERRO             1
154000100712     C                   END
154100100712     C*
154200100712     C                   IF        Work_Format_3 = Data_con_i_secondi           CCYMMDDHHMMSS
154300100712     C                   MOVEL     Work_Data__35 WCOMO14          14
154400100712     C                   TESTN                   WCOMO14              99
154500100712     C   99              MOVEl     WCOMO14       WCAMGORA                       *DATA
154600100712     C   99              MOVEl     WCAMGORA      WCAMG                          *DATA
154700100712     C   99              MOVE      WCAMGORA      WHMS                           *DATA
154800100712     C  N99              MOVEL     'S'           WERRO             1
154900100712     C                   END
155000100712     C*
155100100712     C                   ENDSR
155200100712      * ?------------------------------------------------------------------ */
155300100712      *?    Flagga il TIVIN come messaggio completamente ERRATO
155400100712      * ?------------------------------------------------------------------ */
155500100712     C     MSG_Errato    BEGSR
155600100712      *
155700100712      * ?  Scrive su tutti i records il tipo di errore
155800100712     c     *start        setll     tivin00r
155900100712     c
156000100712     c                   EXSR      LEGGE_TIVIN_R
156100100712     c                   dow       EoFTivin <> 'S'
156200100712     c
156300100712     c                   if        vinMSG  = *blank
156400100712     C                   evalR     vinMSG  = 'MSG IFTSTA ricevuto Anomalo +
156500100712     C                               >> Controllare i DATI su UPLOAD !!'
156600100712     c                   end
156700100712     C                   eval      vinFLG = '2'
156800100712     c                   eval      Almeno_Un_Due ='S'
156900100712     c                   eval      esito  = '2'
157000100712     c                   eval      se_errore ='S'
157100100712     c                   eval      err_bloccante ='S'
157200100712      *
157300100712     c                   update    tivin000
157400100712     c                   EXSR      LEGGE_TIVIN_R
157500100712     c                   endDO
157600100712      *
157700100712     C                   ENDSR
157800100712      * ?------------------------------------------------------------------ */
157900100712      *?    Flagga il TIVIN come messaggio completamente ERRATO
158000100712      * ?------------------------------------------------------------------ */
158100100712     C     DETta_Errato  BEGSR
158200100712      *
158300100712     c                   EXSR      LEGGE_TIVIN_R
158400100712     c                   dow       EoFTivin <> 'S'
158500100712      *
158600100712      * Se ha letto il successivo record
158700100712      *  rispetto a quello che doveva aggiornare , esce dal ciclo di aggiornamento
158800100712     c                   if        VNREC = UNT_record  or
158900100712     c                             VNREC = finDET_RECord
159000100712     c                   leave
159100100712     c                   end
159200100712      *
159300100712     c                   exsr      Error_DET
159400100712      *
159500100712     c                   if        vinMSG  = *blank
159600100712     C                   evalR     vinMSG  = 'Dettaglio IFTSTA ERRATO -
159700100712     C                             >> Controllare i DATI di ogni Segmento <<'
159800100712     c                   end
159900100712      *
160000100712     c                   update    tivin000
160100100712     c                   EXSR      LEGGE_TIVIN_R
160200100712     c                   endDO
160300100712      * ------
160400100712???? C                   EVAL      Oggetto ='Problemi EDI UPLOAD Import IFTSTA'
160500100712     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
160600100712???? C                             STATI IFTSTA non totalmente tradotto. -
160700100712     C                             Controllare UPLOAD del cliente:'
160800100712     C                   EVAL      Messaggio = %trim(Messaggio) +
160900100712     C                                         %editc(§PTksc:'X')
161000100712     c                   if        §PVinv <> 'N'
161100100712     c                   exsr      invio_mail
161200100712     c                   end
161300100712      *
161400100712      * ripulisce per un nuovo giro
161500100712     C                   clear                   Err_in_detta
161600100712      *
161700100712     C                   ENDSR
161800121105      *---------------------------------------------------------------*
161900121105      * CTRL caricamento schiere    PT                               -*
162000121105      *---------------------------------------------------------------*
162100121105     C     Check_PT      begsr
162200121105     C*
162300121105     c* Controllo riempimento schiera
162400121105     c                   clear                   trul0sds
162500121105     c                   eval      i0sski='PT_key'
162600121105     c                   eval      i0sele=%elem(PT_key)
162700121105     c                   eval      i0spie=x
162800121105     c                   eval      i0sfile='EDTAB00F'
162900121105     c                   eval      i0ssif=knsif
163000121105     c                   eval      i0spgm='TRTCT94R'
163100121112     c                   move      kpjbu         SvKpjbu
163200121112     c                   clear                   kpjbu
163300121105     c                   movel     trul0sds      kpjbu
163400121105     c                   call      'TRUL0SR'
163500121105     c                   parm                    kpjba
163600121112     c                   eval      kpjbu = SvKpjbu
163700121105     C*
163800121105     C                   ENDSR
163900100712      * ?------------------------------------------------------------------ */
164000090210** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
164100090210ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
164200090210** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
164300090210987654321098765432109876540123456789987654321098765432109876540999999999999999 1
