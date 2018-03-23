000100060626      *PARMS TGTRLS(V5R1M0)
000200161019     /*PRM  tgtrls(*PRV)
000300161019     /*END
000400060517      *------------*
000500060518      *? FNLV2RZS ?*--------------------------------------------------*
000600060518      *------------*                                                  *
000700060518      *?STAMPA SEGNACOLLO CON BARCODE A 18 CON ZEBRA 8 DOT/MM        ?*
000800060518      *?       CON BARCODE 3X70                                      ?*
000900060518      *---------------------------------------------------------------*
001000060523      *
001100060511     h decedit('0,') datedit(*dmy.)
001200060511      *
001300060516      *?ARCHIVI?------------------------------------------------------*
001400060511      *
001500060511     fFNLV22P   o    f  132        printer
001600060511      *
001700060516      *?COSTANTI?-----------------------------------------------------*
001800060511      *
001900060523      * - Caratteri numerici
002000060523     d C_DigitN        c                   const('0123456789')
002100060512      * - Costanti DPD
002200060519     d C_dpd00a        c                   const('Notification on dama-
002300060519     d                                     ge which is not ')
002400060519     d C_dpd00b        c                   const('recognizable from th-
002500060519     d                                     e outside has to')
002600060519     d C_dpd00c        c                   const('be submitted to DPD -
002700060519     d                                     within 7 days in')
002800060519     d C_dpd00d        c                   const('written.            -
002900060519     d                                                     ')
003000060517     d C_dpd01         c                   const('Delivery Adress:')
003100060512     d C_dpd03         c                   const('Sender:')
003200060512     d C_dpd04         c                   const('Receiver:')
003300060512     d C_dpd05         c                   const('Italy')
003400060512     d C_dpd09         c                   const('01')
003500060512     d C_dpd10         c                   const('BARTOLINI S.p.A')
003600060512     d C_dpd11         c                   const('sending')
003700060512     d C_dpd12         c                   const('consists of:')
003800060512     d C_dpd13         c                   const('parcel no:')
003900060512     d C_dpd14         c                   const('1 Parcel')
004000060512     d C_dpd15         c                   const('order no:')
004100060512     d C_dpd16         c                   const('date:')
004200060512     d C_dpd17         c                   const('weight in kg:')
004300060512     d C_dpd18         c                   const('delivery no:')
004400060512      * - Nazione di Partenza
004500060512     d C_nazP          c                   const('IT')
004600060512      *
004700060512      * - Caratteri "finali" in stampa - per segnacollo
004800060516     d C_End           c                   const('^FS')
004900060511      *
005000060516      *?SCHIERE?------------------------------------------------------*
005100060511      *
005200060519      * - Composizioni: Rag.Soc.Mitt.+Prov. Mitt./Dest.
005300060519      *                 Ind.+Loc.           Dest.
005400060512     d §RI             s              1    dim(37)  inz
005500060519      * - Sequenza colli: editati e separati da "-"
005600060512     d §RX             s              1    dim(11)  inz
005700060511      *
005800060516      *?STRUTTURE DATI?-----------------------------------------------*
005900060511      *
006000060512      * - Parametri
006100060511     d dsBLD         e ds
006200060511     d dsBLDE        e ds
006300060512      *
006400060512      * DS x composizione segnacollo lungo 7
006500060512      *    e con codice a barre (senza check-digit)
006600060512     d dsBLD7        e ds                           inz
006700060512     d  $SegnaC                1     17  0 dim(17)  inz
006800060511      *
006900060517      * Schiere per calcolo del check-digit
007000060512     d ds_BarCode      ds            18             inz
007100060512     d  $CB                    1     18  0 dim(18)  inz
007200060517     d ds2a            ds             2             inz
007300060512     d  §2N                    1      2  0 dim(2)   inz
007400060511      *
007500060517      * Campi per editazione e trattamento dei numeri segnacollo
007600060512     d ds7a            ds             7    inz
007700060512     d  §N7                    1      7  0 inz
007800060518     d  §s4                    1      4
007900060518     d  §s3                    5      7
008000060511      *
008100060518      * x DPD (DP3): Numero spedizione in etichetta Estero
008200060518     d DP3NSPds        ds                  inz
008300060518     d  DP3nsp_3a                     3    inz
008400060518     d  DP3nsp_8a                     8    inz
008500060518     d  DP3nsp_1a                     1    inz
008600060511      *
008700060518     *** * x EuroExpress: num. AWB
008800060518      * x DPD (DP3) .: "Dove"
008900060518     d DP3AWBds        ds                  inz
009000060518     d  wDPDc1w                       2    inz
009100060518     d  wDPDc3w                       5    inz
009200060518     d  wDPDc5w                       2    inz
009300060518     d  wDPDc7w                       1    inz
009400060511      * x DPD .......: "Dove" in etichetta Estero
009500060518     d DP3DOVEds       ds                  inz
009600060518     d  wDPDc1                        2    inz
009700060518     d  wDPDc2                        1    inz('-')
009800060518     d  wDPDc3                        5    inz
009900060518     d  wDPDc4                        1    inz('-')
010000060518     d  wDPDc5                        2    inz
010100060518     d  wDPDc6                        1    inz('-')
010200060518     d  wDPDc7                        1    inz
010300060511      *
010400060518      * x DPD (DP3): dati in etichetta Estero
010500060518     d DP3XX1ds        ds                  inz
010600060511      * - versione cappario
010700060518     d  wDP3xx1vcap                   5    inz
010800060511      * - *blanks
010900060518     d  wDP3xx1_                      5    inz
011000060511      * - linea di arrivo
011100060518     d  wDP3xx1lna                    3    inz
011200060511      * - zona di arrivo
011300060518     d  wDP3xx1zna                    2    inz
011400060518      *
011500060518      * x DPD (DP4): dati in etichetta Estero
011600060518     d DP4XX1ds        ds                  inz
011700060518      * - versione cappario
011800060518     d  wDP4xx1vcap                   7    inz
011900060518      * - depot
012000060518     d  wDP4xx1depo                   4    inz
012100060518      * - destination sort
012200060518     d  wDP4xx1dsor                   4    inz
012300060511      *
012400060511      * x DPD: numero di telefono
012500060512     d dsDPD2          ds                  inz
012600060511     d  DPDtel                        3    inz('Tel')
012700060512     d  DPDteln                      15    inz
012800060511      *
012900060518      * x DPD (DP3): numero di fax
013000060512     d dsDPD2f         ds                  inz
013100060511     d  DPDfax                        3    inz('Fax')
013200060512     d  DPDfaxn                      15    inz
013300060511      *
013400060511      * x DPD: testo "fisso" in etichetta
013500060518     d dsDPDn          ds                  inz
013600060518     d  wDPDn1                       38    inz
013700060518     d  wDPDn2                       38    inz
013800060518     d  wDPDn3                       38    inz
013900060518     d  wDPDn4                       39    inz
014000060518      *
014100060518      * x DPD (DP4): barcode (28 char + check-digit)
014200060523     d dsDP4_BarCode   ds            29    inz
014300060529      * - Identification tag ("%")                - alfanumerico
014400060518     d  DP4bc_IdTag                   1    inz
014500060529      * - Destination post code (c.a.p. - con "00" iniziale)
014600060518     d  DP4bc_DPC                     7    inz
014700060529      * - Tracking number (parcel)                - alfanumerico
014800060518     d  DP4bc_TrackNr                14    inz
014900060529      * - Service code                            - numerico
015000060606     d  DP4bc_SrvCd                   3  0 inz
015100060529      * - Destination country code (cod.ISO num.) - numerico
015200060606     d  DP4bc_DCC                     3  0 inz
015300060518      * - Check digit (sui caratteri escluso "IdTag")
015400060518     d  DP4bc_ChkD                    1    inz
015500060627     *** * - "spezzamento" tracking number per isolamento campi numerici
015600060627     ***d   DP4bc_TrackA          9     12
015700060627     ***d   DP4bc_TrackN         13     22  0
015800060529      * - "isolamento" campi per la stampa di "decodifica"
015900060523     d   DP4bc_01              2      5
016000060523     d   DP4bc_02              6      8
016100060523     d   DP4bc_03              9     12
016200060523     d   DP4bc_04             13     16
016300060523     d   DP4bc_05             17     20
016400060523     d   DP4bc_06             21     22
016500060523     d   DP4bc_07             23     25
016600060523     d   DP4bc_08             26     28
016700060518      *
016800060518      * x DPD (DP4): parcel/tracking number
016900060518     d dsDP4_Parcel    ds                  inz
017000060518     d  DP4pn_a                       4    inz
017100060518     d  DP4pn_b                       4    inz
017200060518     d  DP4pn_c                       4    inz
017300060518     d  DP4pn_d                       2    inz
017400060518      * - Check digit
017500060518     d  DP4pn_ChkD                    1    inz
017600060511      *
017700060512     d dsPIOPPA        ds                  inz
017800010719     d  operat                        1    inz
017900010719     d  pista                         1    inz
018000010719     d  pallet                        2    inz
018100060512      *
018200060512      * Data 8 cifre (per routine di inversione)
018300060518     d WdataAMG        ds             8    inz
018400060518     d  Waa                           4  0 inz
018500060518     d  Wmm                           2  0 inz
018600060518     d  Wgg                           2  0 inz
018700040614      *
018800060516      *?VARIABILI?----------------------------------------------------*
018900060511      *
019000060512      * - Indici di schiera
019100060512     d XY              s              3  0 inz
019200060512     d XZ              s              3  0 inz
019300060512      * - Data Spedizione in formato gg/mm/aaaa
019400060512     d DataSp          s              8  0 inz
019500060512      * - flags in stampa ("ristampa", "valore", ...)
019600060512     d WLDrst          s              1    inz
019700060512     d WLDrst2         s              1    inz
019800060512      * - numero serie + "-"
019900060512     d WLDnrt          s              3    inz('  -')
020000060512      * - "Rif.:" + codice prodotto
020100060512     d WRif21          s             21    inz
020200060512      * - Terminal di Arrivo (alfanum.)
020300060518     d WLDtnaO         s              3    inz
020400060512      * - P.O. di Arrivo     (alfanum.)
020500060518     d WLDlnaO         s              3    inz
020600060512      * - Zona segnacolli    (alfanum.)
020700060518     d WLDzscO         s              2    inz
020800060512      *
020900060512      * - Località Destinatario x DPD
021000060512     d WLDlod          s             25    inz
021100060517      * - Ragione Sociale Mittente x DPD
021200060517     d WLDrsm          s             22    inz
021300060523     d WLDrsm1         s             25    inz
021400060523     d WLDrsm2         s             10    inz
021500060607      * - Nazione del Deposito DPD (DP4)
021600060607     d WDEsia          s              2    inz
021700060518      * - Decodifica Service code x DPD (DP4)
021800060518     d WDPDsrvc        s             16    inz
021900061010      * - "-" + IATA code + Priority group DPD (DP4)
022000061010     d WDP4shpp        s              5    inz
022100060512      *
022200060512      * - Codice a Barre
022300060512     d WCDbar          s             18    inz
022400060512      * - Parte finale in stampa Codice a Barre
022500060512     d WBCend          s             20    inz
022600060523      * - Campi per l'impostazione dei dati da stampare nel barcode
022700060523     d wLast           s              1    inz
022800060523     d wChar           s              1    inz
022900060512      * - Campi di comodo per calcolo Check-Digit
023000060512     d wCHKDGT         s              1  0 inz
023100060512     d §TT             s              3  0 inz
023200060512     d §TT2            s              3  0 inz
023300060512     d §CTR            s              3  0 inz
023400060512     d §RST            s              3  0 inz
023500060512      *
023600060512      * - Campi di comodo
023700060512     d W0007a          s              7    inz
023800060512     d W0003a          s              3    inz
023900060512     d W0002a          s              2    inz
024000060511
024100060511      *---------------------------------------------------------------*
024200060516      *?INDICATORI UTILIZZATI                                        ?*
024300060512      *---------------------------------------------------------------*
024400060512      *  19 - Stampa Serie Segnacollo - se c'è ("..-")                *
024500060512      *  55 - Segnacollo estero EuroExpress                           *
024600060626      *  61 - Sigla Servizio <> *blanks (box da stampare)             *
024700060512      *---------------------------------------------------------------*
024800060511
024900060511     c     *Entry        plist
025000060511     c                   parm                    dsBLD
025100060511     c                   parm                    dsBLDE
025200060512      *
025300060512      * Inversione Data Spedizione (in fmt gg/mm/aaaa)
025400060518     c                   movel     §LDaas        WdataAMG
025500060518     c                   move      §LDmgs        WdataAMG
025600060518     c                   eval      DataSp  =  (Wgg * 1000000)
025700060518     c                                     +  (Wmm * 10000)
025800060518     c                                     +  Waa
025900050415      *
026000060512      * Impostazione Flag Ristampa: " ", "R", "M", "V", "X", "W"
026100060512     c*** già così:      clear                   wLDrst
026200060512     c*** già così:      clear                   wLDrst2
026300060512sel 1c                   SELECT
026400050415      *
026500060518w   1c                   WHEN      §LDrst  =  *blanks
026600050415      *
026700060518w   1c                   WHEN      §LDrst  =  'V'
026800060518     c                   eval      wLDrst2 =  §LDrst
026900050415      *
027000060518w   1c                   WHEN      §LDrst  =  'X'
027100060518     c                   eval      wLDrst  =  'R'
027200060518     c                   eval      wLDrst2 =  'V'
027300050415      *
027400060518w   1c                   WHEN      §LDrst  =  'W'
027500060518     c                   eval      wLDrst  =  'M'
027600060518     c                   eval      wLDrst2 =  'V'
027700050415      *
027800060512x   1c                   OTHER
027900060518     c                   eval      wLDrst  =  §LDrst
028000050415      *
028100060512e   1c                   ENDSL
028200050415      *
028300060512      * Impostazione nr. serie ("nn-")
028400060512     c                   movel     §LDnrs        wLDnrt
028500060512      *
028600060512      * Sostituzione "0" (anche non significativi) del segnacollo
028700060512      *          con "O"
028800060512      * SE NON passa da sr. di Stampa ETICHETTA ITALIA
028900060512if  1c                   IF        §DEtst  =  'EST'
029000060512     c                   exsr      sr_COLZER
029100060512e   1c                   ENDIF
029200060512      *
029300060512      * Stampa ETICHETTA ESTERO
029400060512if  1c                   IF             §DEtst  =  'EST'
029500060512     c                             or   §DEtst  =  'ALL'
029600060512      *
029700060512sel 2c                   SELECT
029800060512      * - EuroExpress
029900060517      *   (gestita in sr. sr_PRINT - etichetta Italia)
030000060517w   2c                   WHEN      §DEtse  =  'EEX'
030100060517      * - DPD (Deutscher Paket Dienst)
030200060530w   2c                   WHEN      §DEtse  =  'DP3'
030300060530     c                   exsr      sr_DPDdove
030400060530     c                   exsr      sr_DPDchi
030500060530     c                   except    LV22dpd3
030600060530w   2c                   WHEN      §DEtse  =  'DP4'
030700060518     c                   exsr      sr_DP4
030800060517     c                   except    LV22dpd4
030900060512e   2c                   ENDSL
031000060512      *
031100060512e   1c                   ENDIF
031200060512      *
031300060512      * Stampa ETICHETTA ITALIA
031400060512if  1c                   IF             §DEtst  =  'ITA'
031500060512     c                             or   §DEtst  =  'ALL'
031600060512     c                   exsr      sr_PRINT
031700060517e   1c                   ENDIF
031800060512      *
031900060512     c                   eval      *inLR   =  *on
032000060512
032100060512      *---------------------------------------------------------------*
032200060516      *?Impostazione & Stampa ETICHETTA ITALIA                       ?*
032300060512      *---------------------------------------------------------------*
032400060512     c     sr_PRINT      BEGSR
032500060517      *
032600060517      *?Impostazione indicatori? - - - - - - - - - - - - - - - - - - -*
032700060517      *
032800060517      * - *in19 = Stampa Serie Segnacollo - se c'è ("..-")
032900060517     c                   eval      *in19   =  (§LDnrs > *zeros)
033000060517      *
033100060517      * - *in55 = Segnacollo Estero EuroExpress
033200060517     c                   eval      *in55   =  (§DEtse = 'EEX')
033300060512      *
033400060517      *?Impostazione campi in stampa?- - - - - - - - - - - - - - - - -*
033500060512      *
033600060512      * Copia dati da dsBLD a dsBLD7
033700060512     c                   eval      §LClnp  =  §LDlnp
033800060512     c                   eval      §LClna  =  §LDlna
033900060512     c                   eval      §LCnrs  =  §LDnrs
034000060512     c                   eval      §LCncd  =  §LDncd
034100060512     c                   eval      §LCzsc  =  §LDzsc
034200060512      *** non cancellare MOVEL     §LDdla        dsPIOPPA
034300060512      *
034400060512      * Calcola cifra di controllo (check-digit) x modulo 10 IBM
034500060512     c                   exsr      sr_M10
034600060512     c                   movel     ds_BarCode    wCDbar
034700060512      *
034800060512      * Pulizia del flag "Tipo Servizio Bolle" SE:
034900060512     c                   if             §LDtsp  =  'D'
035000060512     c                             and  §LDlna  <  300
035100060512     c                   clear                   §LDtsp
035200060512     c                   endif
035300060512      *
035400060512      *
035500060512     c                   clear                   §RX
035600060512      * Toglie *zeros non significativi dal 1° numero colli
035700060512     c                   clear                   w0007a
035800060512     c                   eval      %subst( w0007a
035900060512     c                                   : %len(w0007a)-%len(§LDnc1)+1
036000060512     c                                   : %len(§LDnc1) ) =
036100060512     c                             %editc( §LDnc1 : 'Z' )
036200060512     c                   move      w0007a        w0003a
036300060512     c                   movea     w0003a        §RX(1)
036400060512     c                   eval      §RX(4)  =  '-'
036500060512      * Toglie *zeros non significativi dall'ultimo numero colli
036600060512     c                   clear                   w0007a
036700060512     c                   eval      %subst( w0007a
036800060512     c                                   : %len(w0007a)-%len(§LDnc2)+1
036900060512     c                                   : %len(§LDnc2) ) =
037000060512     c                             %editc( §LDnc2 : 'Z' )
037100060512     c                   move      w0007a        w0003a
037200060512     c                   movea     w0003a        §RX(5)
037300060515      * Aggiunge il codice autotrasportatore
037400060515     c                   move      §LDcpd        w0003a
037500060512     c                   movea     w0003a        §RX(9)
037600060512      *
037700060512      * Sostituisce "0" (anche non significativi) con "O":
037800060512      * - Segnacollo
037900060512     c                   exsr      sr_COLZER
038000060512      * - Terminal di Arrivo
038100060512     c                   move      §LDtna        w0003a
038200060512     c     '0':'O'       xlate     w0003a        wLDtnaO
038300060512      * - P.O. di Arrivo
038400060512     c                   move      §LDlna        w0003a
038500060512     c     '0':'O'       xlate     w0003a        wLDlnaO
038600060512      * - Zona Segnacollo
038700060512     c                   movel     §LDzsc        w0002a
038800060512     c     '0':'O'       xlate     w0002a        wLDzscO
038900060512      *
039000060512      * Codice prodotto
039100060512     c                   clear                   wRif21
039200060512if  1c                   if        §LDcdp  <> *blanks
039300060512     c                   eval      wRif21  =  'Rif.: '
039400060512     c                                     +  §LDcdp
039500060512e   1c                   endif
039600060512      *
039700060517      *?Stampa?- - - - - - - - - - - - - - - - - - - - - - - - - - - -*
039800060512      *
039900060512      * Inizializzazione stampante
040000060512     c                   EXCEPT    LV22
040100060512      *
040200060512      * Stampa LINEA PARTENZA
040300060512     c                   EXCEPT    LV22H
040400060512      *
040500060512      * Stampa MITTENTE
040600060512     c                   clear                   §RI
040700060512     c                   movea     §LDrsm        §RI(4)
040800060512     c                   movea     *blanks       §RI(30)
040900060512     c                   movea     §LDprm        §RI(31)
041000060512     c                   clear                   §RI(33)
041100060512     c                   EXCEPT    LV22D1
041200060512      *
041300060512      * Stampa NUMERO SEGNACOLLO
041400060512     c                   EXCEPT    LV22D2
041500060512      *
041600060512      * Stampa LINEA ARRIVO
041700060512     c                   EXCEPT    LV22D3
041800060512      *
041900060512      * Stampa DESTINATARIO (1: ragione sociale e provincia)
042000060512     c                   clear                   §RI
042100060512     c                   movea     §LDrsd        §RI(4)
042200060512     c                   movea     *blanks       §RI(30)
042300060512     c                   movea     §LDprd        §RI(31)
042400060512     c                   clear                   §RI(33)
042500060512     c                   EXCEPT    LV22D4
042600060512      *
042700060512      * Stampa DESTINATARIO (2: indirizzo e località)
042800060512     c                   clear                   §RI
042900060512     c                   movea     §LDind        §RI(4)
043000060512      * - cerca l'ultimo carattere utilizzato dall'indirizzo in §RI
043100060512      *   (dove parte dal 4°, cioè ha 3 *blanks iniziali)
043200060512     c                   eval      XY  =  %len(%trimr(§LDind))
043300060512     c                   eval      XY  =  XY + 3
043400060512     c                   if        XY  >  25
043500060512     c                   movea     *blanks       §RI(26)
043600060512     c                   eval      XY  =  25
043700060512     c                   endif
043800060512      * - aggiunge la località del destinatario
043900060512     c                   add       3             XY
044000060512     c                   movea     §LDlod        §RI(XY)
044100060512     c                   EXCEPT    LV22D5
044200060512      *
044300060512      * Stampa TIPO SERVIZIO BOLLE (se impostato)
044400060512sel 1c                   select
044500060518w   1c                   when      §LDtsp  =  *blanks
044600090806w   1c                   when      §LDtsp  =  'E'   or
044700090806     c                             §LDtsp  =  'H'
044800060512     c                   EXCEPT    LV22X5E
044900060512x   2c                   other
045000060512     c                   EXCEPT    LV22X5
045100060512e   1c                   endsl
045200060512      *
045300060512      * Stampa BARCODE
045400060512     c                   EXCEPT    LV22D6
045500060512      *
045600060512      * Stampa RIFERIMENTO / RISTAMPA
045700060512     c                   EXCEPT    LV22D7
045800060512      *
045900060512      * "Chiusura" etichetta
046000060512     c                   EXCEPT    LV22D8
046100060512      *
046200060512     c                   ENDSR
046300060512
046400060512      *---------------------------------------------------------------*
046500060516      *?Sostituzione degli zeri NON significativi del segnacollo     ?*
046600060516      *?               con "O" (più leggibile)                       ?*
046700060512      *---------------------------------------------------------------*
046800060512     c     sr_COLZER     BEGSR
046900060511      *
047000060512     c                   move      §LDncd        §N7
047100060511     c     '0':'O'       xlate     ds7a          ds7a
047200060511      *
047300060512     c                   ENDSR
047400060512
047500060512      *---------------------------------------------------------------*
047600060516      *?Calcolo cifra di controllo secondo "modulo 10 IBM"           ?*
047700060516      *?"CHIODI": codice (di cui calcolare il check digit) lungo 17  ?*
047800060516      *?¯¯¯¯¯¯¯¯  check-digit alla 18ª posizione dello stesso campo  ?*
047900060512      *---------------------------------------------------------------*
048000060512     c     sr_M10        BEGSR
048100060512      *
048200060512     c                   clear                   §TT
048300060512     c                   clear                   §TT2
048400060512     c                   clear                   §CTR
048500060512     c                   clear                   §RST
048600060512     c                   clear                   wCHKDGT
048700060518     c                   eval      XY      =  18
048800060512      *
048900060512do  1c                   DO        17            XZ
049000060512      *
049100060512     c                   sub       1             XY
049200060512     c     XZ            div       2             §CTR
049300060512     c                   mvr                     §RST
049400060512      *
049500060518if  2c                   if        §RST    >  0
049600060512      *
049700060518     c                   eval      §CTR    =  $SegnaC(XY) * 2
049800060517     c                   move      §CTR          ds2a
049900060512     c                   xfoot     §2N           §CTR
050000060512      *
050100060512x   2c                   else
050200060512      *
050300060518     c                   eval      §CTR    =  $SegnaC(XY)
050400060512      *
050500060512e   2c                   endif
050600060512      *
050700060512     c                   add       §CTR          §TT
050800060512      *
050900060512e   1c                   ENDDO
051000060512      *
051100060512     c                   eval      §TT2    =  §TT  + 9
051200060512     c                   move      0             §TT2
051300060512     c                   eval      wCHKDGT = §TT2 - §TT
051400060512      *
051500060512     c                   movea     $SegnaC       $CB
051600060512     c                   eval      $CB(18) = wCHKDGT
051700060512      *
051800060512     c                   ENDSR
051900060512
052000060512      *---------------------------------------------------------------*
052100060516      *?Impostazione dati del "DOVE" nell'etichetta DPD              ?*
052200060512      *---------------------------------------------------------------*
052300060512     c     sr_DPDdove    BEGSR
052400060512      *
052500060518     c                   eval      DP3NSPds = §DEeur
052600060518     c                   eval      DP3XX1ds = §DExx1
052700060518     c                   eval      DP3AWBds = §DEawb
052800060518     c                   eval      wDPDc1   = wDPDc1w
052900060518     c                   eval      wDPDc3   = wDPDc3w
053000060518     c                   eval      wDPDc5   = wDPDc5w
053100060518     c                   eval      wDPDc7   = wDPDc7w
053200060518     c                   eval      dsDPDn   =
053300060518     c                             'Notifications on damages which are -
053400040520     c                             not recognizable from the outside -
053500000118     c                             have to be submitted to the transport -
053600040521     c                             organisation within 7 days in written.'
053700060512      *
053800060512     c                   ENDSR
053900060512
054000060512      *---------------------------------------------------------------*
054100060516      *?Impostazione dati del "CHI SONO" nell'etichetta DPD          ?*
054200060512      *---------------------------------------------------------------*
054300060512     c     sr_DPDchi     BEGSR
054400060512      *
054500060512     c                   movel     §DExx2        DPDteln
054600060512     c                   move      §DExx2        DPDfaxn
054700040614     c                   movel     §LDlod        WLDlod
054800060517     c                   movel     §LDrsm        WLDrsm
054900060512      *
055000060512     c                   ENDSR
055100060518
055200060518      *---------------------------------------------------------------*
055300060518      *?Impostazione dati nell'etichetta DPD "DP4"                   ?*
055400060518      *---------------------------------------------------------------*
055500060518     c     sr_DP4        BEGSR
055600060523      *
055700060523      * Preparazione nominativo mittente   ("spezzato" in 2)
055800060523     c                   movel     §LDrsm        WLDrsm1
055900060525     c                   eval      WLDrsm2 = %trim(%subst(§LDrsm :
056000060525     c                                         %len(WLDrsm1) + 1 :
056100060525     c                                         %len(§LDrsm)  -
056200060525     c                                           %len(WLDrsm1)))
056300060607      * Impostazione nazione del deposito
056400060607     c                   movel     §DEsia        WDEsia
056500060518      *
056600060518      * Preparazione BarCode+CheckDigit
056700060518     c                   movel(p)  §DEeur        dsDP4_BarCode
056800060518     c                   movel     §DEsip        DP4bc_ChkD
056900060518      *
057000060518      * Preparazione Parcel/Tracking_number+CheckDigit
057100060518     c                   movel(p)  DP4bc_TrackNr dsDP4_Parcel
057200060518     c                   move      §DEsip        DP4pn_ChkD
057300061010      *
057400061010      * Impostazione "shipping-path" (IAT code + Priority group)
057500061010      *   nel "destination text"
057600061010     c                   if        %subst(§DEawb:1:4) > *blanks
057700061010     c                   eval      wDP4shpp = '-' + %subst(§DEawb:1:4)
057800061010     c                   endif
057900060518      *
058000060518      * Impostazione n° di telefono Bartolini
058100060518     c                   movel     §DExx2        DPDteln
058200060518      *
058300060518      * Impostazione n° di fax      Bartolini
058400060518     c                   movel     §LDcdp        DPDfaxn
058500060518      *
058600060518      * Decodifica Service Code
058700060518     c                   move      §DExx2        wDPDsrvc
058800060518      *
058900060518      * Preparazione dati da XX1
059000060518     c                   eval      DP4XX1ds = §DExx1
059100060626      *
059200060626      * Impostazione indicatori utilizzati in stapa_
059300060626      * - 61 = sigla servizio <> *blanks (box da stampare)
059400060626     c                   eval      *in61 = (§DEsse  <> *blanks)
059500060518      *
059600060518     c                   ENDSR
059700060511
059800060517      *===============================================================*
059900060517      *?ETICHETTA ITALIA   &   EUROEXPRESS (EEX) - v. *in55          ?*
060000060517      *===============================================================*
060100060512      ********* INIZIALIZZAZIONE ETICHETTA
060200060512     oFNLV22P   e            LV22             01
060300060512     o                                              '^XA'
060400060512      * - rotazione 180°
060500060512     o          e            LV22        1
060600060512     o                                              '^POI'
060700060512     o          e            LV22        1
060800060512     o                                              '^LH090,20'
060900060511      *** (margine senza rotazione)
061000060512     o***                                           '^LH80,40'
061100060512     o          e            LV22        1
061200060516     o                                              '^PRC^MSY'
061300060516     o                       C_end
061400060512      ********* Mittente
061500060512     o          e            LV22D1      1
061600060512     o                                              '^FT592,500'
061700060512     o                                              '^A0B,25,22'
061800060512     o                                              '^FD'
061900060512     o                       §RI
062000060516     o                       C_end
062100060512      ********* Destinatario (Rag.Soc.)
062200060512     o          e            LV22D4      1
062300060512     o                                              '^FO595,130'
062400060512     o                                              '^GB0,350,3'
062500060516     o                       C_end
062600060512     o          e            LV22D4      1
062700060512     o                                              '^FT625,500'
062800060512     o                                              '^A0B,25,22'
062900060512     o                                              '^FD'
063000060512     o                       §RI
063100060516     o                       C_end
063200060512      ********* Destinatario (Località)
063300060512     o          e            LV22D5      1
063400060512     o                                              '^FT655,500'
063500060512     o                                              '^A0B,25,22'
063600060512     o                                              '^FD'
063700060512     o                       §RI
063800060516     o                       C_end
063900060512      *****  e  Volume spedizione
064000060512     o          e            LV22D5      1
064100060512     o                                              '^FT690,480'
064200060512     o                                              '^A0B,25,21'
064300060512     o                                              '^FD'
064400060512     o                                              'MC.'
064500060512     o                                              ' '
064600060512     o                       §LDvol                 '0 ,   '
064700060512     o                                              ' '
064800060512     o                       §LDfvr
064900060516     o                       C_end
065000060512      ********* SERVIZIO
065100060512     o          e            LV22X5      1
065200060512     o                                              '^FO000,240'
065300060512     o                                              '^GB065,065,03'
065400060516     o                       C_end
065500060512     o          e            LV22X5      1
065600060512     o                                              '^FO020,250'
065700060512     o                                              '^A0N,060,048'
065800060512     o                                              '^FD'
065900060512     o                       §LDtsp
066000060516     o                       C_end
066100060512     o          e            LV22X5E     1
066200060512     o                                              '^FO000,240'
066300060512     o                                              '^GB065,065,60'
066400060516     o                       C_end
066500060512     o          e            LV22X5E     1
066600060512     o                                              '^FO020,250'
066700060512     o                                              '^A0N,060,048'
066800060512     o                                              '^FR'
066900060512     o                                              '^FD'
067000060512     o                       §LDtsp
067100060516     o                       C_end
067200060512      ********* Linea Partenza
067300060512     o          e            LV22H       1
067400060512     o                                              '^FO000,320'
067500060512     o                                              '^ACN,54,20'
067600060512     o                                              '^FD'
067700060512     o                       §LDlnp
067800060516     o                       C_end
067900060512      *****  e  Sequenza colli
068000060512     o          e            LV22H       1
068100060512     o                                              '^FT690,320'
068200060512     o                                              '^A0B,25,22'
068300060512     o                                              '^FD'
068400060512     o                       §RX
068500060516     o                       C_end
068600060512      ********* Segnacollo
068700060512     o          e    19      LV22D2      1
068800060512     o                                              '^FO020,370'
068900060512     o                                              '^A0N,090,60'
069000060512     o                                              '^FD'
069100060512     o                       wLDnrt
069200060516     o                       C_end
069300060512     o          e            LV22D2      1
069400060512     o                                              '^FO120,370'
069500060512     o                                              '^A0N,090,084'
069600060512     o                                              '^FD'
069700060512     o                       §S4
069800060516     o                       C_end
069900060512     o          e            LV22D2      1
070000060512     o                                              '^FO305,340'
070100060512     o                                              '^A0N,145,108'
070200060512     o                                              '^FD'
070300060512     o                       §S3
070400060516     o                       C_end
070500060512      ********* TERMINAL/LINEA/ZONA ARRIVO
070600060512      ******    Terminal di Arrivo
070700060512     o          e            LV22D3      1
070800060512     o                                              '^FO090,000'
070900060512     o                                              '^GB130,120,003'
071000060516     o                       C_end
071100060512     o          e            LV22D3      1
071200060512     o                                              '^FO100,010'
071300060512     o                                              '^A0N,135,72'
071400060512     o                                              '^FR'
071500060512     o                                              '^FD'
071600060512     o                       wLDtnaO
071700060516     o                       C_end
071800060512      *****  e  Linea di Arrivo
071900060512     o          e            LV22D3      1
072000161020     o                                              '^FO230,040'
072100060512     o                                              '^A0N,090,48'
072200060512     o                                              '^FD'
072300060512     o                       wLDlnaO
072400060516     o                       C_end
072500060512      *****  e  Nazione di Arrivo (*in55 = EuroEXpress)
072600060512     o          e    55      LV22d3      1
072700161020     o                                              '^FO310,030'
072800161020     o                                              '^GB210,085,3'
072900060516     o                       C_end
073000060512     o          e    55      LV22d3      1
073100161020     o                                              '^FO320,040'
073200161020     o                                              '^A0N,085,43'
073300060512     o                                              '^FD'
073400060512     o                       §DEisa
073500060516     o                       C_end
073600060512     o          e    55      LV22d3      1
073700161020     o                                              '^FO375,040'
073800161020     o                                              '^A0N,080,43'
073900060512     o                                              '^FD'
074000060512     o                       §LDcad
074100060516     o                       C_end
074200060512      *****  e  Codice Pista
074300060512     o          e            LV22D3      1
074400060512     o                                              '^FO240,000'
074500060512     o                                              '^A0N,060,48'
074600060512     o                                              '^FD'
074700060512     o                       Pista
074800060516     o                       C_end
074900060512      *****  e  Codice Pallet
075000060512     o          e            LV22D3      1
075100060512     o                                              '^FO320,000'
075200060512     o                                              '^A0N,045,48'
075300060512     o                                              '^FD'
075400060512     o                       Pallet
075500060516     o                       C_end
075600060512      *****  e  Codice Operatore
075700060512     o          e            LV22D3      1
075800060512     o                                              '^FO430,000'
075900060512     o                                              '^A0N,045,48'
076000060512     o                                              '^FD'
076100060512     o                       Operat
076200060516     o                       C_end
076300060512      *****  e  Zona di Arrivo    (*in55 = EuroEXpress)
076400161019     o*//       e   N55      LV22D3      1
076500161019     o          e            LV22D3      1
076600161020     o                                              '^FO550,000'
076700060512     o                                              '^GB110,120,03'
076800060516     o                       C_end
076900161019     o*//       e   N55      LV22D3      1
077000161019     o          e            LV22D3      1
077100161020     o                                              '^FO560,010'
077200060512     o                                              '^A0N,135,84'
077300060512     o                                              '^FR'
077400060512     o                                              '^FD'
077500060512     o                       wLDzscO
077600060516     o                       C_end
077700041112      *
077800060512      ********* BARCODE Code128
077900060512      *****     "Chiusura" superiore
078000060512     o          e            LV22D6      1
078100060512     o                                              '^FO110,123'
078200060512     o                                              '^GBO402,010,010'
078300060516     o                       C_end
078400060512      *****  e  Bar-Code Code128
078500060512     o          e            LV22D6      1
078600060512     o                                              '^BY3,3,0'
078700060512     o          e            LV22D6      1
078800060512     o                                              '^FO110,130'
078900060512     o                                              '^BCN,200,N,N,N'
079000060512     o                                              '^FD>;'
079100060512     o                       wCDbar
079200060516     o                       C_end
079300060512      *****  e  "Chiusura" inferiore
079400060512     o          e            LV22D6      1
079500060512     o                                              '^FO110,330'
079600060512     o                                              '^GBO402,005,005'
079700060516     o                       C_end
079800060512      ********* CODICE PRODOTTO
079900060512     o          e            LV22D7      1
080000060512     o                                              '^FO000,460'
080100060512     o                                              '^ACN,18,10'
080200060512     o                                              '^FD'
080300060512     o                       wRif21
080400060516     o                       C_end
080500060512      *****  e  Peso in kg
080600060512     o          e            LV22D7      1
080700060512     o                                              '^FT690,160'
080800060512     o                                              '^A0B,25,22'
080900060512     o                                              '^FD'
081000060512     o                                              'KG.'
081100060512     o                                              ' '
081200060512     o                       §LDpkg                 '    0 , '
081300060516     o                       C_end
081400060512      *****  e  Data per stampa/RISTAMPA
081500060512     o          e            LV22D7      1
081600060512     o                                              '^FO330,460'
081700060512     o                                              '^ACN,18,10'
081800060512     o                                              '^FD'
081900060512     o                       DataSp                 '  /  /    '
082000060516     o                       C_end
082100060511      *****  e  Flag ristampa
082200060512     o          e            LV22D7      1
082300060512     o                                              '^FO460,460'
082400060512     o                                              '^ACN,18,10'
082500060512     o                                              '^FD'
082600060512     o                       wLDRST
082700060516     o                       C_end
082800060511      *****  e  2° flag ristampa "V"
082900060512     o          e            LV22D7      1
083000060512     o                                              '^FO510,445'
083100060512     o                                              '^ACN,36,20'
083200060512     o                                              '^FD'
083300060512     o                       wLDrst2
083400060516     o                       C_end
083500060512      ********* CHIUSURA ETICHETTA
083600060512     o          e            LV22D8      1
083700060512     o                                              '^XZ'
083800060517
083900060517      *===============================================================*
084000060516      *?ETICHETTA ESTERO DPD formato "DP3"                           ?*
084100060517      *===============================================================*
084200060512      * DPD: etichetta 3 grande
084300060512     o          e            LV22dpd3         01
084400060512     o                                              '^XA'
084500060512     o          e            LV22dpd3    1
084600060512     o                                              '^PON'
084700060512     o          e            LV22dpd3    1
084800060512     o                                              '^LH0,0'
084900060512     o          e            LV22dpd3    1
085000060512     o                                              '^PRC'
085100060512      *---*  Box Spedizione
085200060512     o          e            LV22dpd3    1
085300060512     o                                              '^FO170,020'
085400060512     o                                              '^GB590,105,004'
085500060516     o                       C_end
085600060512      *---*  Telefono
085700060512     o          e            LV22dpd3    1
085800060512     o                                              '^FO320,030'
085900060512     o                                              '^ACN,18,10'
086000060512     o                                              '^FD'
086100060512     o                       dsDPD2
086200060516     o                       C_end
086300040519      *---*  Fax
086400060512     o          e            LV22dpd3    1
086500060512     o                                              '^FO320,050'
086600060512     o                                              '^ACN,18,10'
086700060512     o                                              '^FD'
086800060512     o                       dsDPD2f
086900060516     o                       C_end
087000060512      *---*  Bartolini S.p.a.
087100060512     o          e            LV22dpd3    1
087200060512     o                                              '^FO560,030'
087300060512     o                                              '^ADN,18,10'
087400060512     o                                              '^FD'
087500060512     o                       C_dpd10
087600060516     o                       C_end
087700060512     o          e            LV22dpd3    1
087800060512     o                                              '^FO600,060'
087900060512     o                                              '^ADN,18,10'
088000060512     o                                              '^FD'
088100060512     o                       §LDdla
088200060516     o                       C_end
088300060512     o          e            LV22dpd3    1
088400060512     o                                              '^FO600,090'
088500060512     o                                              '^ADN,18,10'
088600060512     o                                              '^FD'
088700060512     o                       C_dpd05
088800060516     o                       C_end
088900060512      *---*  Numero Spedizione
089000060512     o          e            LV22dpd3    1
089100060512     o                                              '^FO180,060'
089200060512     o                                              '^ACN,72,30'
089300060512     o                                              '^FD'
089400060518     o                       DP3nsp_3a
089500060516     o                       C_end
089600040518      * 8 caratteri numero spedizione
089700060512     o          e            LV22dpd3    1
089800060512     o                                              '^FO285,075'
089900060512     o                                              '^ACN,54,20'
090000060512     o                                              '^FD'
090100060518     o                       DP3nsp_8a
090200060516     o                       C_end
090300060512      * ultimo carattere numero spedizione
090400060512     o          e            LV22dpd3    1
090500060512     o                                              '^FO475,090'
090600060512     o                                              '^ACN,36,20'
090700060512     o                                              '^FD'
090800060518     o                       DP3nsp_1a
090900060516     o                       C_end
091000060512      *---*  Costante "Destinatario"
091100060512     o          e            LV22dpd3    1
091200060512     o                                              '^FO070,140'
091300060512     o                                              '^ACN,18,10'
091400060512     o                                              '^FD'
091500060512     o                       C_dpd04
091600060516     o                       C_end
091700060512      *---*  Destinatario
091800060512     o          e            LV22dpd3    1
091900060512     o                                              '^FO205,140'
092000060512     o                                              '^ACN,36,10'
092100060512     o                                              '^FD'
092200060512     o                       §LDrsd
092300060516     o                       C_end
092400060512      *---*  Indirizzo Destinatario
092500060512     o          e            LV22dpd3    1
092600060512     o                                              '^FO205,190'
092700060512     o                                              '^ACN,36,10'
092800060512     o                                              '^FD'
092900060512     o                       §LDind
093000060516     o                       C_end
093100060512      *---*  Località Destinatario
093200060512     o          e            LV22dpd3    1
093300060512     o                                              '^FO205,240'
093400060512     o                                              '^A0N,50,40'
093500060512     o                                              '^FD'
093600060512     o                       wLDlod
093700060516     o                       C_end
093800060512      *---*  Cap Destinatario
093900060512     o          e            LV22dpd3    1
094000060512     o                                              '^FO070,240'
094100060512     o                                              '^A0N,50,40'
094200060512     o                                              '^FD'
094300060512     o                       §LDcad
094400060516     o                       C_end
094500060512      *---*  Nazione Destinatario
094600060512     o          e            LV22dpd3    1
094700060512     o                                              '^FO690,240'
094800060512     o                                              '^A0N,50,40'
094900060512     o                                              '^FD'
095000060512     o                       §DEsia
095100060516     o                       C_end
095200060512      *---*  linea superiore
095300060512     o          e            LV22dpd3    1
095400060512     o                                              '^FO030,300'
095500060512     o                                              '^GB750,0,003'
095600060516     o                       C_end
095700060512      *---*  costante scritta
095800060512     o          e            LV22dpd3    1
095900060512     o                                              '^FO050,310'
096000060512     o                                              '^ABN,11,07'
096100060512     o                                              '^FD'
096200060518     o                       wDPDn1
096300060516     o                       C_end
096400060512     o          e            LV22dpd3    1
096500060512     o                                              '^FO390,310'
096600060512     o                                              '^ABN,11,07'
096700060512     o                                              '^FD'
096800060518     o                       wDPDn2
096900060516     o                       C_end
097000060512     o          e            LV22dpd3    1
097100060512     o                                              '^FO050,330'
097200060512     o                                              '^ABN,11,07'
097300060512     o                                              '^FD'
097400060518     o                       wDPDn3
097500060516     o                       C_end
097600060512     o          e            LV22dpd3    1
097700060512     o                                              '^FO390,330'
097800060512     o                                              '^ABN,11,07'
097900060512     o                                              '^FD'
098000060518     o                       wDPDn4
098100060516     o                       C_end
098200060512      *---*  linea inferiore
098300060512     o          e            LV22dpd3    1
098400060512     o                                              '^FO030,350'
098500060512     o                                              '^GB750,0,003'
098600060516     o                       C_end
098700060512      *---*  chiusura superiore 1° B-C
098800060512     o          e            LV22dpd3    1
098900060512     o                                              '^FO060,420'
099000060512     o                                              '^GB370,0,008'
099100060516     o                       C_end
099200060512      *---*  costante primo B-C
099300060512     o          e            LV22dpd3    1
099400060512     o                                              '^FO060,420^BY4,2'
099500060512     o                                              '^B2N,220,N,N,N'
099600060512     o                                              '^FD'
099700060518     o                       DP3NSPds
099800060516     o                       C_end
099900060512      *---*  chiusura inferiore
100000060512     o          e            LV22dpd3    1
100100060512     o                                              '^FO060,640'
100200060512     o                                              '^GB370,0,008'
100300060516     o                       C_end
100400060518      *---*  Versione Cappario
100500060512     o          e            LV22dpd3    1
100600060512     o                                              '^FO200,660'
100700060512     o                                              '^ABN,33,21'
100800060512     o                                              '^FD'
100900060518     o                       wDP3xx1vcap
101000060516     o                       C_end
101100060512      *---*  chiusura superiore 2° B-C
101200060512     o          e            LV22dpd3    1
101300060512     o                                              '^FO100,710'
101400060512     o                                              '^GB312,0,008'
101500060516     o                       C_end
101600060512      *---*  Codice a Barre
101700060512     o          e            LV22dpd3    1
101800040624     o                                              '^FO100,710^BY4,2'
101900060512     o                                              '^B2N,240,N,N,N'
102000060512     o                                              '^FD'
102100060512     o                       §DEawb
102200060516     o                       C_end
102300060512      *---*  chiusura inferiore
102400060512     o          e            LV22dpd3    1
102500060512     o                                              '^FO100,950'
102600060512     o                                              '^GB312,0,008'
102700060516     o                       C_end
102800060512      *---*  decodifica 2° bcc
102900060516     o          e            LV22dpd3    1
103000060512     o                                              '^FO130,970'
103100060512     o                                              '^ABN,22,14'
103200060512     o                                              '^FD'
103300060518     o                       DP3DOVEds
103400060516     o                       C_end
103500060512      *---*  Costante "Mittente"
103600060512     o          e            LV22dpd3    1
103700060512     o                                              '^FT570,820'
103800060512     o                                              '^A0B,30,24'
103900060512     o                                              '^FD'
104000060512     o                       C_dpd03
104100060516     o                       C_end
104200060512      *---*  Mittente
104300060512     o          e            LV22dpd3    1
104400060512     o                                              '^FT630,820'
104500060512     o                                              '^A0B,45,36'
104600060512     o                                              '^FD'
104700060512     o                       wLDrsm
104800060516     o                       C_end
104900060512      *---*  Provincia Mittente
105000060512     o          e            LV22dpd3    1
105100060512     o                                              '^FT690,820'
105200060512     o                                              '^A0B,45,36'
105300060512     o                                              '^FD'
105400060512     o                       §LDprm
105500060516     o                       C_end
105600060512      *---*  box laterale mittente
105700060512     o          e            LV22dpd3    1
105800060512     o                                              '^FO500,350'
105900060512     o                                              '^GB260,820,003'
106000060516     o                       C_end
106100060512      * linea interna box verticale
106200060512     o          e            LV22dpd3    1
106300060512     o                                              '^FO630,840'
106400060512     o                                              '^GB0,270,003'
106500060516     o                       C_end
106600060512      * linea interna box laterale
106700060512     o          e            LV22dpd3    1
106800060512     o                                              '^FO500,840'
106900060512     o                                              '^GB260,0,003'
107000060516     o                       C_end
107100060512      *---*  costanti primi quadranti
107200060512     o          e            LV22dpd3    1
107300060512     o                                              '^FO530,850'
107400060512     o                                              '^ABN,11,07'
107500060512     o                                              '^FD'
107600060512     o                       C_dpd11
107700060516     o                       C_end
107800060512     o          e            LV22dpd3    1
107900060512     o                                              '^FO515,870'
108000060512     o                                              '^ABN,11,07'
108100060512     o                                              '^FD'
108200060512     o                       C_dpd12
108300060516     o                       C_end
108400060512     o          e            LV22dpd3    1
108500060512     o                                              '^FO520,890'
108600060512     o                                              '^ACN,18,10'
108700060512     o                                              '^FD'
108800060512     o                       C_dpd14
108900060516     o                       C_end
109000060512     o          e            LV22dpd3    1
109100060512     o                                              '^FO650,860'
109200060512     o                                              '^ABN,11,07'
109300060512     o                                              '^FD'
109400060512     o                       C_dpd13
109500060516     o                       C_end
109600060512      *---*  linea interna box laterale 2
109700060512     o          e            LV22dpd3    1
109800060512     o                                              '^FO500,930'
109900060512     o                                              '^GB260,0,003'
110000060516     o                       C_end
110100060512      *---*  costanti secondi quadranti
110200060516     o          e            LV22dpd3    1
110300060512     o                                              '^FO530,950'
110400060512     o                                              '^ABN,11,07'
110500060512     o                                              '^FD'
110600060512     o                       C_dpd15
110700060516     o                       C_end
110800060516     o          e            LV22dpd3    1
110900060512     o                                              '^FO660,950'
111000060512     o                                              '^ABN,11,07'
111100060512     o                                              '^FD'
111200060512     o                       C_dpd16
111300060516     o                       C_end
111400060512      *---*  Data Spedizione
111500060516     o          e            LV22dpd3    1
111600060512     o                                              '^FO635,970'
111700060512     o                                              '^ADN,18,10'
111800060512     o                                              '^FD'
111900060512     o                       DataSp                 '  /  /    '
112000060516     o                       C_end
112100060512      *---*  linea interna box laterale 3
112200060512     o          e            LV22dpd3    1
112300060512     o                                              '^FO500,1020'
112400060512     o                                              '^GB260,0,003'
112500060516     o                       C_end
112600060512      *---*  costanti terzi quadranti
112700060516     o          e            LV22dpd3    1
112800060512     o                                              '^FO510,1040'
112900060512     o                                              '^ABN,11,07'
113000060512     o                                              '^FD'
113100060512     o                       C_dpd17
113200060516     o                       C_end
113300060512      *---*  Peso (kg)
113400060512     o          e            LV22dpd3    1
113500060512     o                                              '^FO515,1060'
113600060512     o                                              '^ADN,18,10'
113700060512     o                                              '^FD'
113800060512     o                       §LDpkg        2
113900060516     o                       C_end
114000060512     o          e            LV22dpd3    1
114100060512     o                                              '^FO645,1040'
114200060512     o                                              '^ABN,11,07'
114300060512     o                                              '^FD'
114400060512     o                       C_dpd18
114500060516     o                       C_end
114600060512      *---*  Linea interna box laterale 4
114700060512     o          e            LV22dpd3    1
114800060512     o                                              '^FO500,1110'
114900060512     o                                              '^GB260,0,003'
115000060516     o                       C_end
115100060512      *---*  quarto quadrante Rif. Sped. Bartolini
115200060512     o          e            LV22dpd3    1
115300060512     o                                              '^FO505,1125'
115400060512     o                                              '^ABN,33,14'
115500060512     o                                              '^FD'
115600060512     o                       §LDlnp
115700060512     o                                              ' '
115800060512     o                       §LDnrs
115900060512     o                                              ' '
116000060512     o                       §LDncd
116100060516     o                       C_end
116200060512      *---*  costante x
116300060512     o          e            LV22dpd3    1
116400060512     o                                              '^FO060,1020'
116500060512     o                                              '^ACN,72,30'
116600060512     o                                              '^FD'
116700060512     o                       §DEsse
116800060516     o                       C_end
116900060512      *---*  costante 09
117000060512     o          e            LV22dpd3    1
117100060512     o                                              '^FO060,1110'
117200060512     o                                              '^ACN,72,30'
117300060512     o                                              '^FD'
117400060512     o                       C_dpd09
117500060516     o                       C_end
117600060512      *---*  Linea Arrivo
117700060512     o          e            LV22dpd3    1
117800060512     o                                              '^FO160,1060'
117900060512     o                                              '^ACN,126,40'
118000060512     o                                              '^FD'
118100060518     o                       wDP3xx1lna
118200060516     o                       C_end
118300060512      *---*  Zona Arrivo
118400060512     o          e            LV22dpd3    1
118500060512     o                                              '^FO335,1110'
118600060512     o                                              '^ACN,72,30'
118700060512     o                                              '^FD'
118800060512     o                                              '. '
118900060518     o                       wDP3xx1zna
119000060516     o                       C_end
119100060519      *---*  FINE
119200060512     o          e            LV22dpd3    1
119300060512     o                                              '^XZ'
119400060517
119500060517      *===============================================================*
119600060516      *?ETICHETTA ESTERO DPD formato "DP4"                           ?*
119700060517      *===============================================================*
119800060516      * DPD: etichetta 4 grande
119900060523      *
120000060523      *===* START?
120100060523      *
120200060519      *---* ?INIZIO etichetta?
120300060516     o          e            LV22dpd4         01
120400060516     o                                              '^XA'
120500060516     o          e            LV22dpd4    1
120600060516     o                                              '^PON'
120700060516     o          e            LV22dpd4    1
120800060516     o                                              '^LH0,0'
120900060516     o          e            LV22dpd4    1
121000060516     o                                              '^PRC'
121100060523      *
121200060523      *===* SHIPMENT INFORMATION FIELD?
121300060518      *
121400060518      *---* ?costante: Notification of Dameges?
121500060516     o          e            LV22dpd4    1
121600060519     o                                              '^FO190,030'
121700060519     o                                              '^ACN,15,10'
121800060516     o                                              '^FD'
121900060518     o                       C_dpd00a
122000060516     o                       C_end
122100060516     o          e            LV22dpd4    1
122200060519     o                                              '^FO190,045'
122300060519     o                                              '^ACN,15,10'
122400060516     o                                              '^FD'
122500060518     o                       C_DPD00b
122600060516     o                       C_end
122700060516     o          e            LV22dpd4    1
122800060519     o                                              '^FO190,060'
122900060519     o                                              '^ACN,15,10'
123000060516     o                                              '^FD'
123100060518     o                       C_dpd00c
123200060516     o                       C_end
123300060519     o          e            LV22dpd4    1
123400060519     o                                              '^FO190,075'
123500060519     o                                              '^ACN,15,10'
123600060519     o                                              '^FD'
123700060519     o                       C_dpd00d
123800060519     o                       C_end
123900060518      *
124000060517      *---*  _ linea orizzontale separatrice
124100060516      *      Notification of Damages / Delivery Address
124200060516     o          e            LV22dpd4    1
124300060523     o                                              '^FO020,110'
124400060523     o                                              '^GB780,0,003'
124500060516     o                       C_end
124600060517      *---*  | linea verticale separatrice
124700060523      *      Delivery Address / Consignor Address (Sender)
124800060517     o          e            LV22dpd4    1
124900060531     o                                              '^FO555,110'
125000060523     o                                              '^GB0,340,003'
125100060517     o                       C_end
125200060517      *---*  | linea verticale separatrice
125300060523      *      Consignor Address (Sender) / dispatching depot address
125400060517     o          e            LV22dpd4    1
125500060531     o                                              '^FO650,110'
125600060523     o                                              '^GB0,340,003'
125700060517     o                       C_end
125800060523      *---*  _ linea orizzontale separatrice
125900060523      *      Delivery Address / Customer specific details (References)
126000060523     o          e            LV22dpd4    1
126100060523     o                                              '^FO020,300'
126200060531     o                                              '^GB535,0,003'
126300060523     o                       C_end
126400060523      *---*  | linea verticale separatrice
126500060523      *      Customer specific details (Referces) / Parcel details
126600060523     o          e            LV22dpd4    1
126700060531     o                                              '^FO410,300'
126800060523     o                                              '^GB0,150,003'
126900060523     o                       C_end
127000060518      *
127100060518      *---* ?DELIVERY ADDRESS?
127200060517      * - -  costante: Delivery Address (verticale)
127300060517     o          e            LV22dpd4    1
127400060531     o                                              '^FO530,120'
127500060519     o                                              '^A0R,20,15'
127600060517     o                                              '^FD'
127700060517     o                       C_dpd01
127800060517     o                       C_end
127900060517      * - -  Nominativo Destinatario
128000060517     o          e            LV22dpd4    1
128100060530     o                                              '^FO040,130'
128200060523     o                                              '^ADN,25,10'
128300060517     o                                              '^FD'
128400060517     o                       §LDrsd
128500060517     o                       C_end
128600060518      * - -  Indirizzo Destinatario
128700060517     o          e            LV22dpd4    1
128800060530     o                                              '^FO040,170'
128900060523     o                                              '^ADN,25,10'
129000060517     o                                              '^FD'
129100060517     o                       §LDind
129200060517     o                       C_end
129300060518      * - -  Nazione-Cap Destinatario
129400060518     o          e            LV22dpd4    1
129500060530     o                                              '^FO040,210'
129600060523     o                                              '^ACN,30,20'
129700060518     o                                              '^FD'
129800060518     o                       §DEisa
129900060518     o                                              '-'
130000060518     o                       §LDcad
130100060518     o                       C_end
130200060518      * - -  Località Destinatario
130300060517     o          e            LV22dpd4    1
130400060530     o                                              '^FO040,260'
130500060523     o                                              '^ADN,25,10'
130600060523     o                                              '^FD'
130700060530     o                       §LDlod
130800060523     o                       C_end
130900060518      *
131000060518      *---* ?SENDER?(verticale)
131100060517      * - -  costante: Sender (verticale)
131200060517     o          e            LV22dpd4    1
131300060531     o                                              '^FO625,120'
131400060519     o                                              '^A0R,20,15'
131500060517     o                                              '^FD'
131600060517     o                       C_dpd03
131700060517     o                       C_end
131800060517      * - -  Nominativo Mittente (verticale)
131900060517     o          e            LV22dpd4    1
132000060531     o                                              '^FO605,130'
132100060523     o                                              '^ACR,15,10'
132200060517     o                                              '^FD'
132300060523     o                       wLDrsm1
132400060517     o                       C_end
132500060523     o          e            LV22dpd4    1
132600060531     o                                              '^FO585,130'
132700060523     o                                              '^ACR,15,10'
132800060523     o                                              '^FD'
132900060523     o                       wLDrsm2
133000060523     o                       C_end
133100060517      * - -  Provincia Mittente (verticale)
133200060517     o          e            LV22dpd4    1
133300060531     o                                              '^FT565,130'
133400060523     o                                              '^ACR,15,10'
133500060517     o                                              '^FD'
133600060517     o                       §LDprm
133700060517     o                       C_end
133800060518      *---* ?CORRIERE "BARTOLINI S.p.A."?(verticale)
133900060517      * - -  costante: BARTOLINI S.p.A.
134000060517     o          e            LV22dpd4    1
134100060531     o                                              '^FO720,130'
134200060523     o                                              '^ACR,15,08'
134300060517     o                                              '^FD'
134400060517     o                       C_dpd10
134500060517     o                       C_end
134600060517      * - -  P.O. Bartolini
134700060517     o          e            LV22dpd4    1
134800060531     o                                              '^FO700,130'
134900060523     o                                              '^ACR,13,05'
135000060517     o                                              '^FD'
135100060517     o                       §LDdla
135200060517     o                       C_end
135300060517      * - -  costante: Italy
135400060517     o          e            LV22dpd4    1
135500060531     o                                              '^FO700,310'
135600060523     o                                              '^ACR,13,05'
135700060517     o                                              '^FD'
135800060517     o                       C_dpd05
135900060517     o                       C_end
136000060517      * - -  Telefono
136100060517     o          e            LV22dpd4    1
136200060531     o                                              '^FO680,130'
136300060523     o                                              '^ACR,13,05'
136400060517     o                                              '^FD'
136500060517     o                       DPDtel
136600060517     o                                              '. '
136700060517     o                       DPDteln
136800060517     o                       C_end
136900060517      * - -  Fax
137000060517     o          e            LV22dpd4    1
137100060531     o                                              '^FO660,130'
137200060523     o                                              '^ACR,15,08'
137300060517     o                                              '^FD'
137400060517     o                       DPDfax
137500060523     o                                              '  '
137600060518     o                       §LDcdp
137700060517     o                       C_end
137800060518      *
137900060518      *---* ?CUSTOMER SPECIFIC DETAILS?
138000060518      * - -  costante: Riferimenti
138100060517     o          e            LV22dpd4    1
138200060523     o                                              '^FO040,310'
138300060523     o                                              '^A0N,20,15'
138400060517     o                                              '^FD'
138500060518     o                                              'References:'
138600060517     o                       C_end
138700060518      * - -  Numero Segnacollo (LNP+NRS+NCD)
138800060518     o          e            LV22dpd4    1
138900060523     o                                              '^FO040,340'
139000060523     o                                              '^ADN,40,20'
139100060518     o                                              '^FD'
139200060518     o                       §LDlnp
139300060518     o                                              ' '
139400060518     o                       §LDnrs
139500060518     o                                              ' '
139600060518     o                       §LDncd
139700060518     o                       C_end
139800060518     *** * - -  costante: ...
139900060518     ***o          e            LV22dpd4    1
140000060519     ***o                                              '^FO040,370'
140100060523     ***o                                              '^A0N,20,10'
140200060518     ***o                                              '^FD'
140300060518     ***o                                              ' '
140400060518     ***o                       C_end
140500060518      *---* ?PARCEL DETAILS?
140600060517      * - -  costante: Collo  Nr / Di
140700060517     o          e            LV22dpd4    1
140800060523     o                                              '^FO440,310'
140900060523     o                                              '^A0N,20,15'
141000060517     o                                              '^FD'
141100060518     o***                                           'Parcel Nr / Of'
141200060518     o                                              'Lieferung'
141300060517     o                       C_end
141400060517      * - -  costante: 1 / 1
141500060517     o          e            LV22dpd4    1
141600060523     o                                              '^FO440,340'
141700060523     o                                              '^A0N,30,25'
141800060517     o                                              '^FD'
141900060523     o                                              '  1 / 1  '
142000060517     o                       C_end
142100060517      * - -  costante: Peso
142200060517     o          e            LV22dpd4    1
142300060523     o                                              '^FO440,380'
142400060523     o                                              '^A0N,20,15'
142500060517     o                                              '^FD'
142600060518     o***                                           'Weight'
142700060519     o                                              'Gewicht'
142800060517     o                       C_end
142900060517      * - -  Peso in kg
143000060517     o          e            LV22dpd4    1
143100060531     o                                              '^FO420,410'
143200060517     o                                              '^A0N,30,25'
143300060517     o                                              '^FD'
143400060531     o                       §LDpkg        3
143500060517     o                                              ' kg'
143600060517     o                       C_end
143700060523      *
143800060523      *===* SERVICE FIELD?
143900060518      *
144000060517      *---*  _ linea orizzontale separatrice
144100060523      *      Delivey Address / Service field
144200060516     o          e            LV22dpd4    1
144300060519     o                                              '^FO020,450'
144400060523     o                                              '^GB780,0,003'
144500060516     o                       C_end
144600060518      *
144700060517      *---*  _ linea orizzontale separatrice
144800060523      *      Service field / Route filed
144900060517     o          e            LV22dpd4    1
145000060523     o                                              '^FO020,620'
145100061012     o                                              '^GB780,0,003'
145200060517     o                       C_end
145300060523      *
145400060523      *===* ROUTE FIELD?
145500060518      *
145600060518      *---* ?TRACKING NUMBER?
145700060518      * . .  Numero Parcel
145800060518     o          e            LV22dpd4    1
145900060628     o                                              '^FO020,650'
146000060628     o                                              '^A0N,50,60'
146100060518     o                                              '^FD'
146200060518     o                       DP4pn_a
146300060518     o                       C_end
146400060518     o          e            LV22dpd4    1
146500060628     o                                              '^FO150,650'
146600060628     o                                              '^A0N,40,40'
146700060518     o                                              '^FD'
146800060518     o                       DP4pn_b
146900060518     o                                              ' '
147000060518     o                       DP4pn_c
147100060518     o                                              ' '
147200060518     o                       DP4pn_d
147300060518     o                       C_end
147400060518     o          e            LV22dpd4    1
147500060628     o                                              '^FO380,650'
147600060628     o                                              '^A0N,30,30'
147700060518     o                                              '^FD'
147800060518     o                       DP4pn_ChkD
147900060518     o                       C_end
148000060523      *
148100060523      *---* ?SERVICE CODE?
148200060523     o          e            LV22dpd4    1
148300060523     o                                              '^FO360,650'
148400060523     o                                              '^ADN,30,20'
148500060523     o                                              '^FD'
148600060523     o                       wDPDsrvc
148700060523     o                       C_end
148800060523      *
148900060523      *---*  costante: Track
149000060523     o          e            LV22dpd4    1
149100060523     o                                              '^FO040,700'
149200060523     o                                              '^A0N,20,15'
149300060518     o                                              '^FD'
149400060518     o                                              'Track'
149500060518     o                       C_end
149600060523      *---*  costante: Servizio
149700060519     o          e            LV22dpd4    1
149800060523     o                                              '^FO690,700'
149900060523     o                                              '^A0N,20,15'
150000060518     o                                              '^FD'
150100060518     o                                              'Service'
150200060518     o                       C_end
150300060518      *
150400060518      *---* ?IDENTIFICATION?
150500060518      *      Sigla servizio: "X" = small parcel, "E" = express parcel
150600060626     o          e            LV22dpd4    1
150700061010     o                                              '^FO030,740'
150800060523     o                                              '^ADN,50,25'
150900060518     o                                              '^FD'
151000060518     o                       §DEsse
151100060518     o                       C_end
151200060626      *     ?   ?
151300060626      *---* ?| |?box per sigla servizio (identification)
151400060626     o          e    61      LV22dpd4    1
151500061010     o                                              '^FO020,730'
151600060628     o                                              '^GB050,063,003'
151700060626     o                       C_end
151800060518      *
151900060518      *---* ?DESTINATION TEXT?
152000060523     o          e            LV22dpd4    1
152100061010     o                                              '^FO100,730'
152200061010     o                                              '^ADN,100,42'
152300060518     o                                              '^FD'
152400060607     *** * . .  Destination-Country code
152500060607     ***o                       §DEisa
152600060607      * . .  Depot Country code
152700060607     o                       wDEsia
152800060518      * . .  Recipient Depot code
152900060518     o                                              '-'
153000060518     o                       wDP4xx1depo
153100061010      * . .  Shipping path
153200061010     o*** solo se shipping-path <> *blanks:         '-'
153300061010     o                       wDP4shpp
153400060518     o                       C_end
153500060518      *
153600060518      *---* ?SERVICE CODE-COUNTRY-RECIPIENT-ZIP?
153700060518     o          e            LV22dpd4    1
153800061010     o                                              '^FO200,835'
153900060523     o                                              '^ADN,28,14'
154000060518     o                                              '^FD'
154100060518      * . .  Service code
154200060518     o                       DP4bc_SrvCd
154300060518      * . .  Destination-country code (nazione destinatario)
154400060523     o                                              ' - '
154500060518     o                       §DEisa
154600060518      * . .  Destination-postal code  (cap destinatario)
154700060523     o                                              ' - '
154800060518     o                       §LDcad
154900060518     o                       C_end
155000060518      *
155100060518      *---* ?VERSIONE CAPPARIO?
155200060518     o          e            LV22dpd4    1
155300060523     o                                              '^FO450,840'
155400060523     o                                              '^ADN,15,07'
155500060518     o                                              '^FD'
155600060523     o                       wDP4xx1vcap
155700060518     o                       C_end
155800060518      *
155900060518      *---* ?DESTINATION SORT?
156000060518     o          e            LV22dpd4    1
156100061010     o                                              '^FO600,830'
156200060523     o                                              '^ADN,50,30'
156300060518     o                                              '^FD'
156400060523     o                       wDP4xx1dsor
156500060518     o                       C_end
156600060523      *
156700060523      *===* BARCODE FIELD?
156800060518      *
156900060518      *---* ?BARCODE Code128?
157000060518      * - -  _ linea orizzontale superiore
157100060518     o          e            LV22dpd4    1
157200061010     o                                              '^FO010,880'
157300060628     o                                              '^GB732,0,006'
157400060518     o                       C_end
157500060518      * - -  Codice a barre (con identification tag, senza check-digit)
157600060518     o          e            LV22dpd4    1
157700061010     o                                              '^FO010,885'
157800060627     o                                              '^BY3,,240'
157900060525     o          e            LV22dpd4    1
158000060525     o                                              '^BCN,200,N,N,N'
158100060518     o                                              '^FD'
158200060529     o*** per inizio con campi B-alfanumerici:      '>:'
158300060529     o*** per inizio con campi C-numerici:          '>;'
158400060529     o                                              '>:'
158500060529     o                       DP4bc_IdTag
158600060529     o                       DP4bc_DPC
158700060530     o*** per campi alfanumerici dopo numerici:     '>6'
158800060530     o*** per campi numerici dopo alfanumerici:     '>5'
158900060530     o                                              '>5'
159000060627     o                       DP4bc_TrackNr
159100060529     o                       DP4bc_SrvCd
159200060529     o                       DP4bc_DCC
159300060529     o                       C_end
159400060518      * - -  Decodifica del Codice a barre (senza identification tag,
159500060518      *                                     con   check-digit)
159600060518     o          e            LV22dpd4    1
159700060628     o                                              '^FO060,1110'
159800060628     o                                              '^ABN,22,14'
159900060518     o                                              '^FD'
160000060523     o                       DP4bc_01
160100060518     o                                              ' '
160200060518     o                       DP4bc_02
160300060518     o                                              ' '
160400060518     o                       DP4bc_03
160500060518     o                                              ' '
160600060518     o                       DP4bc_04
160700060518     o                                              ' '
160800060518     o                       DP4bc_05
160900060518     o                                              ' '
161000060518     o                       DP4bc_06
161100060518     o                                              ' '
161200060518     o                       DP4bc_07
161300060518     o                                              ' '
161400060518     o                       DP4bc_08
161500060518     o                                              ' '
161600060529     o                       DP4bc_ChkD
161700060518     o                       C_end
161800060523      *
161900060523      *===* END?
162000060523      *
162100060519      *---* ?FINE etichetta?
162200060519     o          e            LV22dpd4    1
162300060519     o                                              '^XZ'
