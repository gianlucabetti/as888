000100090108     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300020305
000400020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000500020305
000600090108     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000700090107     F                                     INFDS(TAR)
000800090406     Ffifgt03l  if   e           k disk
000900090406
001000090907     Ffifpt03l  if   e           k disk
001100020304     Ffifpd01l  if   e           k disk
001200021203     Ffiapd01l  if   e           k disk
001300020305     Ftabel00f  if   e           k disk
001400080918     Fancdp01l  if   e           k disk
001500080918     Fanfrn01l  if   e           k disk
001600080915     FanRCO98J  iF   E           K DISK
001700090902      **----------
001800090902     Fficn14rP  O    E             PRINTER INFDS(PRTFBK) usropn
001900090902      **----------
002000090428     d prtFBK          ds
002100090428     D ODP_TYPE               81     82                                         * ODP Type
002200090428     D FILE_NAME              83     92                                         * File name
002300090428     D LIBRARY                93    102                                         * Library name
002400090428     D SPOOL_FILE            103    112                                         * Spool file name
002500090428     D SPOOL_LIB             113    122                                         * Spool file lib
002600090428     D SPOOL_NUM             123    124I 0                                      * Spool file num
002700090428     D RCD_LEN               125    126I 0                                      * Max record len
002800090428     D KEY_LEN               127    128I 0                                      * Max key len
002900090428     D MEMBER                129    138                                         * Member name
003000090428     D TYPE                  147    148I 0                                      * File type
003100090428     D ROWS                  152    153I 0                                      * Num PRT/DSP rows
003200090428     D COLUMNS               154    155I 0                                      * Num PRT/DSP cols
003300090428     D NUM_RCDS              156    159I 0                                      * Num of records
003400090428     D ACC_TYPE              160    161                                         * Access type
003500090428     D DUP_KEY               162    162                                         * Duplicate key?
003600090428     D SRC_FILE              163    163                                         * Source file?
003700090428     D VOL_OFF               184    185I 0                                      * Vol label offset
003800090428     D BLK_RCDS              186    187I 0                                      * Max rcds in blk
003900090902      *
004000090428     D OVERFLOW              188    189I 0                                      * Overflow line
004100090902      *
004200090428     D BLK_INCR              190    191I 0                                      * Blk increment
004300090428     D FLAGS1                196    196                                         * Misc flags
004400090428     D REQUESTER             197    206                                         * Requester name
004500090428     D OPEN_COUNT            207    208I 0                                      * Open count
004600090428     D BASED_MBRS            211    212I 0                                      * Num based mbrs
004700090428     D FLAGS2                213    213                                         * Misc flags
004800090428     D OPEN_ID               214    215                                         * Open identifier
004900090428     D RCDFMT_LEN            216    217I 0                                      * Max rcd fmt len
005000090428     D CCSID                 218    219I 0                                      * Database CCSID
005100090428     D FLAGS3                220    220                                         * Misc flags
005200090428     D NUM_DEVS              227    228I 0                                      * Num devs defined
005300090428      *
005400090428     D CUR_LINE              367    368I 0                                      * Current line num
005500090428     D CUR_PAGE              369    372I 0                                      * Current page cnt
005600090428      *
005700090428     d prtds           ds
005800090428     d   spab                         3  0
005900090428     d   spaa                         3  0
006000090428     d   skab                         3  0
006100090428     d   skaa                         3  0
006200090428     d   line                         3  0
006300090904      ***
006400090904     c*controllo check digits
006500090904     D trul28ds      E DS
006600090902      *
006700120314     D trul33ds      E DS
006800120314      *
006900160316     D trtcm1ds      E DS
007000160316      *
007100111212     D trul13ds      E DS
007200090428     d savpdr          s                   like(apdpdr)
007300090428      *
007400080915     D DVCOBADS      E DS
007500090623     D DapdFLR       E DS
007600080915     D                                     INZ
007700090107     D TAR             DS
007800090107     D  tgtNRR               397    400B 0
007900080915     D CBAIBANDS     E DS
008000020304     dkpjba          e ds
008100080915     ddgpi           e ds
008200090113     dficn05ds       e ds
008300020305     dds8a           e ds
008400020305     dds1z           e ds
008500020305     ddstr           e ds
008600020304
008700020304     dparam            ds
008800020308     D lancio                         1
008900020304     d codpadr                        7s 0
009000020308     D codpadr2                       7s 0
009100020304     d alladat                        8s 0
009200090424     d pardtcerta                     8s 0
009300090428     d parmNRC                        7s 0
009400090428     d parmFIL                        3s 0
009500090428     D OUTQOVR                       10
009600090428     D errore                         1
009700090526     D msgError                      35
009800091015     D pgmChiamante                  10
009900111201     d pardfa                         8s 0
010000121130     d parsocprt                      3
010100020304
010200120322     d savkpjbu        s                   like(kpjbu)
010300160503     D filpad          s              3
010400020402     d modo            s              8
010500020114     d base            s              1
010600111214     d cost3d          s            120
010700111214     d cost3e          s            120
010800111214     d prezzo          s              5  3
010900111214     d cost8a          s            120
011000090903     d cost8c          s            120
011100090903     d cost8d          s            120
011200090903     d cost8e          s            120
011300090903     d cost13          s             70
011400090113     d carb01          s            120
011500090113     d carb02          s            120
011600090113     d carb03          s            120
011700090113     d carb04          s            120
011800090113     d carb05          s            120
011900090113     d carb06          s            120
012000090113     d carb07          s            120
012100080915     d locemi          s             40
012200080305     d Data_testata    s               d   datfmt(*iso)
012300090108     d savprg          s                   like(tgtprg)
012400090108     d fgtdatas        s                   like(fgtddt)
012500020305     d fgtdsts         s                   like(fgtdst)
012600020305     d fgtduvs         s                   like(fgtduv)
012700020305     d fgtdcns         s                   like(fgtdcn)
012800020305     d fgtdtss         s                   like(fgtdts)
012900090904     d Stampato_Barcode...
013000090904     d                 s              1
013100090907     d Cliente_TTL     s              1
013200090505     d Non_trovato_dettaglio...
013300090623     d                 s              1
013400090623     d Allegato_x_Barca...
013500090505     d                 s              1
013600020116      * ds di comodo per impostare campo filler in caso di presenza BONUS
013700020116     d dsbonus         ds
013800020116     d   tpbonus                      1
013900020116     d   pabonus                      3  0
014000020116     d   sabonus                      3  0
014100020118     d   pmbonus                      1
014200020305     d dataiso         s               d   datfmt(*iso)
014300121130     d dataisos        s               d   datfmt(*iso)
014400020305     d dataeur         s               d   datfmt(*eur)
014500020305     D TPS             S              1    DIM(50)                              TIPO PRESTAZIONE
014600020305     D DSR             S             15    DIM(50)                              DESCR.TIPO PRESTAZIO
014700020305     D CDT             S              3    DIM(50)                              COD.TAR. DEL TIPO PR
014800020305     D TSM             S              1    DIM(50)                              TIPO SOMMA PESI
014900020305     D DSM             S             15    DIM(50)                              DESCR.TIPO SOMMA PES
015000160316     D CMD             S             85    DIM(3) CTDATA PERRCD(1)              OVRPRTF
015100090420     ***************************************************************************
015200090420     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
015300090420     D ESITO_OK...
015400090420     D                 C                   0
015500090420     ***************************************************************************
015600090420     **
015700090420     ** Dichiarazione prototipi procedure esterne.
015800090420     **
015900090420     ***************************************************************************
016000090420      /DEFINE DFTACTGRP_YES
016100090420     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
016200090420      /UNDEFINE DFTACTGRP_YES
016300090420     ***************************************************************************
016400090420     **
016500090420     ** Definizione strutture dati.
016600090420     **
016700090420      **************************************************************************
016800090420     D tibsSocI0...
016900090420     D               E DS                  QUALIFIED
017000090420     D                                     INZ
017100090420     D tibsSocO0...
017200090420     D               E DS                  QUALIFIED
017300090420     D                                     INZ
017400090420     ***************************************************************************
017500090420     **
017600090420     ** Definizione variabili modulo/programma.
017700090420     **
017800090420     ***************************************************************************
017900090420     D prmRqsOpCode...
018000090420     D                 S             10A
018100090420     D prmRpyOpCode...
018200090420     D                 S             10A
018300090420     D prmRpyIdMsg...
018400090420     D                 S             10I 0
018500090420     D prmRqsFormato...
018600090420     D                 S             10A
018700090420     D prmRqsDataSize...
018800090420     D                 S             10I 0
018900090420     D prmRpyFormato...
019000090420     D                 S             10A
019100090420     D prmRpyDataSize...
019200090420     D                 S             10I 0
019300090420     D Wsocg           S                   like(apdCSF)
019400090420     D Wsocgd          S                   like(desbrev)
019500090420
019600000000     C*---------------------------------------------------------------*
019700090511      **
019800090511      **  Chiama programma di controllo per verificare se tutto OK
019900090511      **   per stampare altrimenti deve uscire DANDO l'ERRORE
020000091015     c                   eval      pgmChiamante = 'FICN14R'
020100091015     c                   eval       kpjbu = param
020200090511     c                   CALL      'FICN14R0'
020300090511     c                   parm                    kpjba
020400090511     c                   eval      param = kpjbu
020500090902     c                   z-add     parmNRC       prtfNRC
020600090902     c                   z-add     parmFIL       prtfFIL
020700090511      **
020800090511      **  Quindi un programma identico a questo di stampa esegue i controlli
020900090511      **   sulla tariffa in anticipo per non far stampare nulla in caso di
021000090511      **   errore.
021100090511     c                   if        errore <> *blank
021200090511     c                   goto      NO_STAMPA
021300090511     c                   end
021400090511      **
021500090420     ** Inizializzo il programma.
021600090420     C                   CALL      'TIBSSOCR'
021700090420     C                   PARM      'INIT'        prmRqsOpCode
021800090420     C                   PARM                    prmRpyOpCode
021900090420     C                   PARM                    prmRpyIdMsg
022000020304      *
022100020304     c                   z-add     codpadr       kpdr
022200021203     c     kapd          setll     fiapd01l
022300090608     c*
022400020308     c                   do        *hival
022500090622     c*
022600021203     c     apdtip        reade     fiapd01l                               99
022700090622     c*
022800020308     c   99              leave
022900020308     c                   if        apdpdr > codpadr2
023000020308     c                   leave
023100020308     c                   endif
023200090428      *
023300090428      *  A rottura di AUTista
023400090428     c                   if        apdpdr <> savpdr
023500090428     c                   eval      savpdr = apdpdr
023600090505     c                   clear                   Manca_tariffa     1
023700090505     c                   eval       Non_trovato_dettaglio =' '
023800090622      *
023900090622      *  Se si tratta di autisti Marittimi ossia conducenti di motoscafi
024000090622      *   in base ad un flag in anagrafica
024100090623     c                   eval      Allegato_x_Barca = *blank
024200090623     c                   setoff                                       44
024300090623      *
024400090623     c                   movel     apdFLR        DapdFLR
024500090623      *
024600090623     c                   if        §apdMEZZO = 'B'
024700090623     c                   eval      Allegato_x_Barca = 'S'
024800090622     c                   seton                                        44
024900090622     c                   end
025000160316      *----------
025100160316     c                   clear                   primo             1
025200160316     c                   exsr      inviaPDF
025300090428      *
025400090428     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
025500090903     C                   close     FICN14RP
025600160316     C                   MOVEL(p)  CMD(1)        COMMAN170       170
025700160316     C                   MOVE      CMD(2)        COMMAN170
025800090428      *
025900090428      *  se deve essere indirizzata ad una specifica coda di stampa
026000160316     c***                if        OUTQovr = *blank
026100160316     c***                eval      OUTQovr = 'P046B052AU'
026200160316     c***                end
026300160316     c***                if        OUTQovr <> *blank
026400160316     c***                eval      COMMAN160 = %trim(COMMAN160) + ' OUTQ(' +
026500160316     c***                          %Trim(OUTQovr) + ')'
026600160316     c***                end
026700090428      *
026800160316     c                   eval      commanl = %trim(Comman170)+ trtcm1ds + ''')'
026900160316     c                   eval      lung = %len(%trim(commanl))
027000160316     C***                Z-ADD     160           LUNG             15 5
027100090428     C                   CALL      'QCMDEXC'
027200160316     C                   PARM                    COMMANl         500
027300090428     C                   PARM                    LUNG
027400090903     C                   OPEN      FICN14RP
027500090428     c                   endif
027600090428      *
027700080915     c* imposto costanti nel caso non reperisco anagrafico
027800080915     c                   eval      costa = 'Allegato tariffario al contratto +
027900080915     c                             di trasporto sottoscritto tra la società ___+
028000080915     c                             ____________________________'
028100080915     c                   eval      costia= 'Ragione sociale ___________________+
028200080915     c                             ____________________________________________'
028300080915
028400080915     c                   eval      costib= 'Indirizzo__________________________+
028500080915     c                             ____________________________________________'
028600080915
028700080915     c                   eval      costic= 'CAP______________ Città____________+
028800080915     c                             ____________________________________________'
028900080915
029000080915     c                   eval      costid= 'Partita IVA _______________________+
029100080915     c                             ____________________________________________'
029200080915     c                   eval      cost8iban= '|__|__|__|__|  -
029300080915     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
029400080915     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
029500090608      ***
029600090608      ***  Con l'AUtista esegue l'elaborazione
029700090608     c                   z-add     apdpdr        kpdr
029800090624     c                   exsr      Costanti
029900020308     c                   exsr      elabora
030000090608      ***
030100020308     c                   enddo
030200090113      *
030300020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
030400090903     C                   close     FICN14RP
030500160316     c                   exsr      generapdf
030600160316      *
030700090911     C                   MOVEL(p)  CMD(3)        COMMAN80         80
030800020308     C                   Z-ADD     80            LUNG             15 5
030900020308     C                   CALL      'QCMDEXC'
031000090428     C                   PARM                    COMMAN80
031100020308     C                   PARM                    LUNG
031200090420     C*
031300090420     ** Chiudo il programma.
031400090420     C                   CALL      'TIBSSOCR'
031500090420     C                   PARM      'FINALIZE'    prmRqsOpCode
031600090420     C                   PARM                    prmRpyOpCode
031700090420     C                   PARM                    prmRpyIdMsg
031800090504      *
031900090504      *** significa che non ha stampato dettaglio
032000090505     c                   if         Non_trovato_dettaglio ='S'
032100090504     c                   eval      errore ='E'
032200090504     c                   end
032300020304
032400090511     c     NO_STAMPA     tag
032500090429     c                   eval      kpjbu = param
032600020308     c                   seton                                        lr
032700160316     C*------------------------------------------------------*
032800160316     C*  reperisce dati necessari preparazione PDF
032900160316     C*------------------------------------------------------*
033000160316     C     inviaPDF      begsr
033100160316      * verifico se l'invio è tramite PDF
033200160316     c                   clear                   trtcm1ds
033300160316     c                   move      046           §cm1po
033400160316     c                   move      '0'           §cm1sts
033500160316     c                   eval      §cm1idp = '*'
033600160316     c                   eval      §cm1pcl = 'S'
033700160316     c                   if        primo = *blank
033800160503     c                   movel     codpadr       filpad
033900160316     c                   eval      §cm1tips= 'AAC'
034000160316     c                   eval      §cm1var = '*FOUT*' + parsocprt + '_'
034100160503     c                             +  filpad  + '_'
034200160329     c                             +  %editc(parmnrc:'X') + '_'
034300160329     c                             +  %editc(codpadr:'X') + '_'
034400160420     c                             +  %editc(alladat:'X')
034500160316     C* Imposto i parametri x il primo pdf con firma
034600160316     C                   EVAL      pin_tla    = *blanks
034700160316     C                   EVAL      pin_tips   = §cm1tips
034800160316     C                   EVAL      pin_NomeFile = %subst(§cm1var:7)
034900160316     C                   EVAL      pin_Extfile  = '.PDF'
035000160316     C                   EVAL      pOut_esito  = ' '
035100160316     c                   else
035200160503     c                   movel     tgtpdr        filpad
035300160316     c                   eval      §cm1tips = 'AAS'
035400160316     c                   eval      §cm1var = '*FOUT*' + tgtsoc + '_'
035500160503     c                             +  filpad  + '_'
035600160329     c                             +  %editc(parmnrc:'X') + '_'
035700160329     c                             +  %editc(tgtpdr:'X') + '_'
035800160420     c                             +  %editc(tgtddt:'X')
035900160316     C                   EVAL      pin_tla2   = *blanks
036000160316     C                   EVAL      pin_tips2  = §cm1tips
036100160316     C                   EVAL      pin_NomeFile2 = %subst(§cm1var:7)
036200160316     C                   EVAL      pin_Extfile2  = '.PDF'
036300160316     C                   EVAL      pOut_esito2  = ' '
036400160316     c                   endif
036500160316     C*
036600160316     C                   endsr
036700160316     c*______________________________________________________________
036800160316     c     GeneraPDF     begsr
036900160316     c*______________________________________________________________
037000160316     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
037100160316     C*
037200160316     C                   CALL      'TIS7P2SR4'
037300160316     C                   PARM                    pin_tla           1
037400160316     C                   PARM                    pin_tips          3
037500160316     C                   PARM                    pin_NomeFile     60
037600160316     C                   PARM                    pin_Extfile      11
037700160316     C                   PARM                    pOut_esito        1
037800160316     C*
037900160316     C                   CALL      'TIS7P2SR4'
038000160316     C                   PARM                    pin_tla2          1
038100160316     C                   PARM                    pin_tips2         3
038200160316     C                   PARM                    pin_NomeFile2    60
038300160316     C                   PARM                    pin_Extfile2     11
038400160316     C                   PARM                    pOut_esito2       1
038500160316      *non trovato indirizzo mail destinatario invio la mail allo stesso
038600160316      * utente che sta inviando
038700160316     c
038800160316     C                   endsr
038900020308      *--------------------------------------------------
039000020308     c     elabora       begsr
039100020308      *--------------------------------------------------
039200090113      *
039300090113     c                   clear                   TGT_trovato       1
039400090113     c                   clear                   Kmetri            5 0
039500090403     c                   clear                   tmp_caric         4 0
039600090403     c                   clear                   tmp_scar          4 0
039700090907     c                   clear                   Cliente_TTL       1
039800090113      *
039900090902     c                   clear                   SOCTAR
040000090902     c                   clear                   FORTAR
040100090608      ***
040200090107     c                   eval      savprg = *hival
040300090109     c                   clear                   prezzo
040400090113      *
040500020411     c                   do        *hival
040600090107     c* cerco il progressivo
040700090107     c     ktgt          setll     fitgt01l
040800090113      *
040900090107     c                   do        *hival
041000090107     c     ktgt          reade     fitgt01l
041100090113      *
041200090107     c                   if        %eof(fitgt01l)
041300090107     c                   leave
041400090107     c                   end
041500090113      *
041600090109     c* escludo le annullate
041700090109     c                   if        tgtatb <> ' '
041800090109     c                   iter
041900090109     c                   end
042000090113      *
042100090107      * non considero le tariffe che risultano fuori periodo
042200090107     c                   if        tgtddt > alladat or  tgtdst < alladat
042300090107     c                   iter
042400090107     c                   end
042500090113      *
042600090904      *
042700090904      * IMPOSTA il CODICE a BARRE composto con l'autista + la data decorrenza tariffa
042800090904     c     tgtpdr        mult      100000000     Cbarre15         15 0
042900090113     c                   move      'S'           TGT_trovato       1
043000090107     c                   eval      savprg = tgtprg
043100090107     c                   leave
043200090608      **
043300090107     c                   enddo
043400090113      **
043500090608      ** Decodifica Società e Fornitore
043600090608     c                   if        tgtcdf <> *blank
043700090608     c                   move      tgtcdf        FORTAR
043800090608     c                   exsr      Dec_SOC_FOR
043900090608     c                   end
044000090608      **
044100090113     c                   if        tgtKM > 0
044200090113     c                   eval      Kmetri = tgtKM
044300090113     c                   end
044400090608      **
044500090403     c                   if        TGTTMPC > 0
044600090403     c                   eval      tmp_caric = TGTTMPC
044700090403     c                   end
044800090608      **
044900090403     c                   if        TGTTMPS > 0
045000090403     c                   eval      tmp_scar  = TGTTMPS
045100090403     c                   end
045200090113      **
045300090109     c* prezzo iniziale carburante legge 133
045400090109     c                   if        tgtpcar <> 0
045500090109     c                   eval      prezzo = tgtpcar
045600090109     c                   else
045700090113      **
045800090113     c                   clear                   ficn05ds
045900090113     c                   call      'FICN05R'
046000090113     c                   parm                    ficn05ds
046100090113      **
046200090113      ** se non ha agganciato la tabella imposta fisso il prezzo
046300090113     c                   if        o05err = 'E'
046400090113     c                   eval      prezzo = 1,518
046500090113     c                   else
046600090113      **  altrimenti lo prende dalla tabella
046700090113     c                   movel     o05gpi        dgpi
046800090902     c                   z-add     d§gpigpi      prezzo
046900090113     c                   end
047000090113      **
047100090109     c                   end
047200020304      * per giornata
047300020304     c                   move      'G'           ktsr
047400011026     c                   exsr      fifgt
047500090113      *
047600090113      * Se problemi con Supertestata non deve stampare altri FGT
047700090113     c                   if        salta_altriFGT = *blank
047800020304      * per consegna
047900020304     c                   move      'C'           ktsr
048000020304     c                   exsr      fifgt
048100090113     c                   ENDif
048200090113      *
048300090113      * Se problemi con Supertestata non deve stampare altri FGT
048400090113     c                   if        salta_altriFGT = *blank
048500020304      * per ritiro
048600020304     c                   move      'R'           ktsr
048700020304     c                   exsr      fifgt
048800090113     c                   ENDif
048900090113      **
049000090113     c*  Solo in presenza della supertestata stampa la "Clausola" aggiuntiva
049100090113     c*   x esprimere i KM.
049200090113     c                   setoff                                       33
049300090113     c                   if        TGT_trovato ='S'
049400090113     c                   seton                                        33
049500090113     c                   end
049600090904      *
049700090904      * stampo il fondo pagina solo se ho trovato qualcosa per il codice
049800090904      * che sto elaborando e ripristino la variabile per l'intestazione del
049900090904      * successivo
050000090904     c                   if        prima <> *blank
050100090904     c* imposta il codice a BARRE
050200090904     c                   add       datates       Cbarre15
050300090904     c                   exsr      srchkdgt
050400090904     c                   movel(p)  Cbarre16      BCD128
050500090904     c*
050600090904      *  Stampa fine allegato
050700090904     c                   exsr      emette_piede
050800090428      *
050900090428      * deve stampare la nuova pagina con il bollo
051000090428     c                   seton                                        66
051100090428      *
051200090428      *   MA SENZA LA FIRMA
051300090903     C                   close     FICN14RP
051400160316      *
051500160316     c                   move      'X'           primo
051600160316     c                   exsr      inviaPDF
051700160316     C                   MOVEL(p)  CMD(1)        COMMAN170
051800160316     C                   MOVE      CMD(2)        COMMAN170
051900090428      *
052000090428      *  se deve essere indirizzata ad una specifica coda di stampa
052100160316     c****               if        OUTQovr = *blank
052200160316     c****               eval      OUTQovr = 'P046B052AU'
052300160316     c****               end
052400160316     c****               if        OUTQovr <> *blank
052500160316     c****               eval      COMMAN160 = %trim(COMMAN160) + ' OUTQ(' +
052600160316     c****                         %Trim(OUTQovr) + ')'
052700160316     c****               end
052800090428      *
052900160316     c                   eval      commanl = %trim(Comman170)+ trtcm1ds + ''')'
053000160316     c                   eval      lung = %len(%trim(commanl))
053100160316     C*                  Z-ADD     160           LUNG             15 5
053200090428     C                   CALL      'QCMDEXC'
053300160316     C                   PARM                    COMMANl
053400090428     C                   PARM                    LUNG
053500090428     C*
053600090428     C*  Riapre il PRINTER senza la FIRMA
053700090903     C                   OPEN      FICN14RP
053800090428     C*
053900020308     c                   clear                   prima             1
054000020308     c                   endif
054100090428     C*
054200020412     c                   z-add     0             page
054300020411      *forzatura uscita
054400080915     c                   if        lancio = 'P' or lancio = 'R' or *in71
054500090420     c                   leave
054600080915     c                   endif
054700090428     C*
054800090420     c                   seton                                        71
054900020411     c                   enddo
055000090428     C*
055100020411     c                   setoff                                       71
055200090904
055300090904     c                   endsr
055400090904
055500090904     c*-------------------------------------------------
055600090904     c     emette_piede  begsr
055700090904     c*-------------------------------------------------
055800090904     c                   if        CUR_line > 93
055900090904     c                   exsr      X_Overflow
056000090904     c                   endif
056100090904     c                   write     piede
056200090907      *
056300090907      *  se il cliente è TTL deve stampare la frase aggiuntiva
056400090907     c                   if        Cliente_TTL = 'S'
056500090907     c                   if        CUR_line > 95
056600090907     c                   exsr      X_Overflow
056700090907     c                   endif
056800090907     c                   write     piedeTTL
056900090907     c                   end
057000090904      *
057100090904      *  il 44 determina se è contratto dei marittimi (Venezia)
057200111214     c*                  if        not *in44 and prezzo <> 0
057300111214     c*                  if        CUR_line > 95
057400111214     c*                  exsr      X_Overflow
057500111214     c*                  endif
057600111214     c*                  write     piede1a
057700111214     c*                  end
057800090904      *
057900090907      *
058000090904     c                   if        CUR_line > 93
058100090904     c                   exsr      X_Overflow
058200090904     c                   endif
058300090904     c                   write     piede1c
058400090904      *
058500090904     c                   if        not *in44
058600090904     c                   if        CUR_line > 94
058700090904     c                   exsr      X_Overflow
058800090904     c                   endif
058900090904     c                   write     piede1b
059000090904     c                   end
059100090904      *
059200090904     c                   if        CUR_line > 94
059300090904     c                   exsr      X_Overflow
059400090904     c                   endif
059500090904     c                   write     piede2A
059600090904      *
059700090904     c                   if        CUR_line > 94
059800090904     c                   exsr      X_Overflow
059900090904     c                   endif
060000090904     c                   write     piede2B
060100090904      *
060200090904     c                   if        CUR_line > 90
060300090904     c                   exsr      X_Overflow
060400090904     c                   endif
060500090904     c                   write     piede3
060600090904      *
060700090904     c                   if        CUR_line > 95
060800090904     c                   exsr      X_Overflow
060900090904     c                   endif
061000090904     c                   write     piede4A
061100090904      *
061200090904     c                   if        CUR_line > 95
061300090904     c                   exsr      X_Overflow
061400090904     c                   endif
061500090904     c                   write     piede4B
061600090904      *
061700090904     c                   if        CUR_line > 93
061800090904     c                   exsr      X_Overflow
061900090904     c                   endif
062000090904     c                   write     piede5A
062100090904      *
062200090904     c                   if        CUR_line > 94
062300090904     c                   exsr      X_Overflow
062400090904     c                   endif
062500090904     c                   write     piede5B
062600090904      *
062700090904     c                   if        CUR_line > 95
062800090904     c                   exsr      X_Overflow
062900090904     c                   endif
063000090904     c                   write     piede5C
063100090904      *
063200090914     c                   if        CUR_line > 87
063300090904     c                   exsr      X_Overflow
063400090904     c                   endif
063500111212     c* data e firma
063600111212     c* DATA vuota se non passata, altrimenti nella copia dove c'è
063700111212     c* la firma di Vignocchi la data passata, dove c'è il barcode
063800111212     c* data passata + 1 gg
063900111212     c* LOC. vuota se non passata, altrimenti nella copia dove c'è
064000111212     c* la firma di Vignocchi la ns. società, dove c'è il barcode
064100111212     c* la società del vettore
064200111212     c* nella copia con il barcode ci sarà anche luogo e data
064300111212     c* come nella copia della firma di Vignocchi
064400111212     c*
064500111215     c                   if        datafirma = ' '
064600111213     c                   eval      cost9 = 'Luogo _____________________________'
064700111212     c                   eval      cost9a= 'Data  _____________________________'
064800111215     c                   end
064900111212     c* BARCODE
065000111202     c                   if        *in71
065100111212     c                   if        datafirma <> ' '
065200111212     c                   eval      cost12= tibsSocO0.SEDlegLOC+', '
065300111212     c                             + datafirma
065400111215     c                   eval      cost9=%trimr(indlocalit)+', ' +
065500111215     c                             datafirma1
065600111212     c                   else
065700111215     c                   eval      cost9=indlocalit
065800111212     c                   clear                   cost12
065900111212     c                   end
066000111202     c                   else
066100111212     c* VIGNOCCHI
066200111212     c                   if        datafirma <> ' '
066300111215     c                   eval      cost9  =
066400111215     c                             tibsSocO0.SEDlegLOC +', ' +
066500111215     c                             datafirma
066600111215     c                   else
066700111215     c                   eval      cost9  =
066800111215     c                             tibsSocO0.SEDlegLOC
066900111202     c                   end
067000111215     c                   end
067100111212     c*
067200090904     c                   write     piede6
067300090904      *
067400090904     c                   write     FIRMATXT
067500090904     c                   if        Stampato_Barcode <> 'S' and *in71
067600090904     c                   write     BARCODE
067700090904     c                   eval      Stampato_Barcode  = 'S'
067800090904     c                   end
067900090904     c  N71              write     FIRMAIMG
068000090909      *
068100090909     c                   exsr      test_pagina
068200160414     c*mn66              write     paginaBLK
068300090904      *
068400020308     c                   endsr
068500990113     c*-------------------------------------------------
068600090904     c     X_Overflow    begsr
068700090904      *
068800090904      *  il 71 determina l'emissione della firma e della descrizione società
068900090904     c                   exsr      test_pagina
069000090904     c                   write     FIRMATXT
069100090904     c                   if        Stampato_Barcode <> 'S' and *in71
069200090904     c                   write     BARCODE
069300090904     c                   eval      Stampato_Barcode  = 'S'
069400090904     c                   end
069500090904     c  N71              write     FIRMAIMG
069600120315     c   66              exsr      repNUM
069700090904     c                   write     testata
069800160412     c*m 66              write     DTCERTABOX
069900090904     c   66              write     DTCERTATXT
070000090904      *
070100090904     c                   endsr
070200090904     c*-------------------------------------------------
070300011026     c     fifgt         begsr
070400990113     c*-------------------------------------------------
070500090114     c                   clear                   salta_altriFGT    1
070600080305      * data x allegato in vigore al
070700080305     c                   exsr      data_in_vigore
070800080305      *
070900990113     c                   setoff                                           99
071000020305     c     kfgt          setll     fifgt03l
071100090113      *
071200981029     c                   do        *hival
071300020305     c     kfgt          reade     fifgt03l                               99
071400981029     c   99              leave
071500090113      *
071600090109     c* solo il progressivo della supertestata se l'ho trovata
071700090109     c                   if        fgtprg <>savprg and savprg <> *hival
071800090107     c                   iter
071900090107     c                   end
072000090113      *
072100080918      * non considero le tariffe che risultano fuori periodo
072200020305     c                   if        fgtddt > alladat or  fgtdst < alladat
072300020114     c                   iter
072400080225     c                   else
072500090113      *
072600080225      *giro le date per la stampa
072700080305     c                   move      Data_testata  dataeur
072800090608     c                   move      dataeur       DataTes
072900090903     c                   move      dataeur       DataTes1
073000080225
073100080225     c                   end
073200090113
073300020308      *trovata tariffa valida per il padroncino stampo intestazione
073400020308     c                   if        prima = *blank
073500090428      *
073600090428      *  prima pagina con i BOX per bollo e data certa
073700090428     c                   seton                                        66
073800090902     c                   movel(p)  apdrsf        prtrsf
073900090902     c                   z-add     apdpdr        prtpdr
074000090930     c                   z-add     apdpdr        prtpdr1
074100120315     c   66              exsr      repNUM
074200090902     c                   write     testata
074300160412     c*m 66              write     DTCERTABOX
074400090903     c   66              write     DTCERTATXT
074500090902     c                   write     testo
074600090424     c                   setoff                                       66
074700020308     c                   move      'X'           prima
074800020308     c                   endif
074900090113      **
075000020305     c                   z-add     1             x                 3 0
075100020306     c     fgttsr        lookup    tps(x)                                 91
075200090902     c   91              movel     dsr(x)        destsr
075300090113      *
075400020305      *giro le date per la stampa
075500080305     c                   move      Data_testata  dataeur
075600090608     c                   move      dataeur       DataTes
075700090903     c                   move      dataeur       DataTes1
075800020305
075900090904     c                   if        CUR_line > 97
076000090904     c                   exsr      X_Overflow
076100020306     c                   endif
076200090113      ****
076300090113     c                   clear                   salta_altriFGT    1
076400090113      *>>>>>>>>>
076500090113      *  se già la tariffa doveva avere la supertestata (quindi >= 1/2/2009)
076600090113      * occorre controllare e segnalare
076700090113      *  se non ci sono i KM della supertestata o se non c'è la supertestata.
076800090113      *
076900090511     c                   if        fgtDDT >= 20090501
077000090427     c                   IF          TGT_trovato = *blank
077100090113      **
077200090113      ** per non stampare in seguito il piede
077300090113     c                   clear                   prima
077400090113      ****
077500090113      **   se non c'è la supertestata dopo la data 1/2/2009 c'è stato un problema
077600090113      **   tecnico nella conversione o nell'inserimento della nuova tariffa, quindi,
077700090113      **   deve essere segnalato.
077800090113      ****
077900090113      * Non c'è la superTestata
078000090113     c                   if        TGT_trovato = *blank
078100090902     c                   write     NO_SuperT
078200090429     c                   move      'E'           errore
078300090113     c                   move      'S'           salta_altriFGT    1
078400090113     c                   LEAVE
078500090113     c                   else
078600090113     c                   end
078700090113      *
078800090113     c                   end
078900090113     c                   end
079000090113      *>>>>>>>>>
079100090902     c                   z-add     fgtctr        prtctr
079200090902     c                   movel(p)  fgttsr        prttsr
079300090902     c                   z-add     fgtprg        prtprg
079400090902     c                   movel(p)  fgtdiv        prtdiv
079500090902     c                   movel(p)  fgtdcv        prtdcv
079600090902      ***
079700090902     c                   write     testa
079800020304     c                   exsr      fifpt
079900090113      ***
080000020304     c                   enddo
080100020304
080200020304     c                   endsr
080300080305     c*-------------------------------------------------
080400080305     c     data_in_vigorebegsr
080500080305     c*-------------------------------------------------
080600080305      *  esegue un ciclo prima di stampare x prendere la
080700080305      *  data più recente da mostrare in stampa
080800080305      *
080900080305     c                   clear                   Data_testata
081000080305     c                   do        3             tsr               3 0
081100090608      *
081200080305     c                   if        tsr = 1
081300080305     c                   eval      TipSer ='G'
081400080305     c                   elseIF    tsr = 2
081500080305     c                   eval      TipSer ='C'
081600080305     c                   elseIF    tsr = 3
081700080305     c                   eval      TipSer ='R'
081800080305     c                   endIF
081900090608      *
082000080305     c     kfgtsr        setll     fifgt03l
082100080305     c                   do        *hival
082200080305     c     kfgtsr        reade     fifgt03l                               99
082300080305     c   99              leave
082400090608      *
082500090109     c* solo il progressivo della supertestata se l'ho trovata
082600090109     c                   if        fgtprg <>savprg and savprg <> *hival
082700090109     c                   iter
082800090109     c                   end
082900080305      *
083000080305      * non considero le tariffe che risultano fuori periodo
083100080305     c                   if        fgtddt > alladat or  fgtdst < alladat
083200080305     c                   iter
083300080305     c                   else
083400080305      *
083500080305      * giro le date per la stampa
083600080305     c                   if        fgtddt > 0
083700080305     c                   move      fgtddt        dataiso
083800090608      *
083900080305     c                   if        dataiso > Data_testata
084000080305     c                   eval      Data_testata = dataiso
084100080305     c                   end
084200090608      *
084300080305     c                   end
084400080305     c                   end
084500080305     c                   endDO
084600080305
084700080305     c                   endDO
084800080305
084900080305     c                   endsr
085000990113     c*-------------------------------------------------
085100011026     c     fifpt         begsr
085200990113     c*-------------------------------------------------
085300090505     c                   eval      Manca_Tariffa  = 'S'
085400020109     c
085500020304     c                   setoff                                           98
085600090907     c     kfpt          setll     fifpt03l
085700020304     c                   do        *hival
085800090907     c     kfpt          reade     fifpt03l                               98
085900090505      *
086000020304     c   98              leave
086100020304     c                   if        fptatb <> *blank
086200020304     c                   iter
086300020304     c                   endif
086400090505     c                   eval      Manca_Tariffa  = ' '
086500020117
086600020305      *descrizione particolarità
086700020305     c                   clear                   key
086800020305     c                   movel     '8A'          cod
086900020305     c                   movel     fptctd        key
087000020305     c     ktab          chain     tabel                              95
087100020305     c  n95              movel     tbluni        ds8a
087200090902     c  n95              movel     §8ades        desctd
087300020305     c   95              clear                   desctd
087400020305      *desrizione tipo tariffa
087500020305     c                   clear                   key
087600020305     c                   movel     'TR'          cod
087700020305     c                   movel     fpttpg        key
087800020305     c     ktab          chain     tabel
087900020305     c  n95              movel     tbluni        dstr
088000090902     c  n95              movel     §trdes        destpg
088100020305     c   95              clear                   destpg
088200020305
088300020307     c                   clear                   dsbonus
088400020307     c     fptctd        comp      'BON'                                  82
088500020307     c   82              movel     fptflr        dsbonus
088600090907      *
088700090907      *  Eccezione x il cliente TTL che viene pagato contariffe particolari
088800090907      *   sullo STOP di consegna e sulla maggiorazione dello STOP
088900090907     c                   if         fptctd = 'STT' or fptctd = 'XT'
089000090907     c                   eval      Cliente_TTL = 'S'
089100090907     c                   end
089200090907      *
089300020311     c     fpttpg        comp      'V'                                    83
089400020311     c                   select
089500020311     c                   when      fpttsm = 'G'
089600090902     c                   movel(p)  'Giornata'    destsm
089700020311
089800020311     c                   when      fpttsm = 'P'
089900020311     c                   movel(p)  'Prestazione' destsm
090000020311
090100020311     c                   when      fpttsm = 'T'
090200020311     c                   movel(p)  'Stop'        destsm
090300020311
090400020311     c                   when      fpttsm = 'S'
090500020311     c                   movel(p)  'Spedizione'  destsm
090600020311
090700020311     c                   endsl
090800020311
090900020313     c     fptctd        comp      'BAC'                                  77
091000020313     c  n77fptctd        comp      'BAR'                                  77
091100091202     c                   z-add(h)  fptata        fptatas
091200020311     c     fptatas       comp      0                                      78
091300020305     c                   z-add     fptarl        fptarls
091400020305     c                   z-add     fptarf        fptarfs
091500020305     c                   z-add     fptaro        fptaros
091600090902     c                   z-add     fptrpv        fptrpvl
091700020306     c
091800020306     c     fptarls       comp      0                                  81
091900020306     c  n81fptarfs       comp      0                                  81
092000020306     c  n81fptaros       comp      0                                  81
092100020306     c  n81fptrpv        comp      0                                  81
092200020305
092300090904     c                   if        CUR_line > 93
092400090904     c                   exsr      X_Overflow
092500020306     c                   endif
092600090608
092700090902     c                   movel(p)  fpttpg        prttpg
092800090902     c                   movel(p)  tpbonus       tpbonu
092900090902     c                   z-add     pabonus       pabonu
093000090902     c                   movel(p)  pmbonus       pmbonu
093100090902     c                   z-add     sabonus       sabonu
093200090902
093300090902     c                   write     detta
093400020117
093500020304     c                   exsr      fifpd
093600020304     c                   enddo
093700090505
093800090505     c*  se manca tariffa memorizzo il problema x restituire l'errore
093900090505     c*  a fine pgm.
094000090505     c                   If        Manca_Tariffa  = 'S'
094100090505     c                   eval      Non_trovato_dettaglio ='S'
094200090505     c                   end
094300011026
094400990113     c                   endsr
094500990113     c*-------------------------------------------------
094600011026     c     fifpd         begsr
094700990113     c*-------------------------------------------------
094800090505     c                   eval      Manca_Tariffa  = *blank
094900090505      *
095000090505      * Solo x AUT e x tipi servizio a dettaglio
095100090505     c                   if        fgtctr <> 999
095200090505     c                   if        fgtTSR = 'C' or fgtTSR = 'R'
095300090506     c********           eval      Manca_Tariffa  = 'S'
095400090505     c                   end
095500090505     c                   end
095600020114
095700020304     c                   setoff                                           97
095800020305     c     kfpd          setll     fifpd01l
095900020304     c                   do        *hival
096000020305     c     kfpd          reade     fifpd01l                               97
096100020304     c   97              leave
096200020304     c                   if        fpdatb <> *blank
096300020304     c                   iter
096400020304     c                   endif
096500090505     c                   eval      Manca_Tariffa  = ' '
096600020304
096700091202     c                   z-add(h)  fpditr        fpditrs
096800091202     c                   z-add(h)  fpdmin        fpdmins
096900091202     c                   z-add(h)  fpdmax        fpdmaxs
097000020305     c                   z-add     fpdsgl        fpdsgls
097100020305
097200090904     c                   if        CUR_line > 93
097300090904     c                   exsr      X_Overflow
097400090608     c                   endif
097500090608
097600090902     c                   eval      prtcap = fpdcap
097700090902     c                   eval      prtain = fpdain
097800090902
097900090902     c                   write     dettab
098000020304
098100020304     c                   enddo
098200090505
098300090505     c*  se manca tariffa memorizzo il problema x restituire l'errore
098400090505     c*  a fine pgm.
098500090505     c                   If        Manca_Tariffa  = 'S'
098600090505     c                   eval      Non_trovato_dettaglio ='S'
098700090505     c                   end
098800090505
098900090505     c*
099000990113     c                   endsr
099100981029      *
099200011026     c*-------------------------------------------------
099300011026     c     *inzsr        begsr
099400011026     c*-------------------------------------------------
099500020304     c     *entry        plist
099600020304     c                   parm                    kpjba
099700020304     c                   movel     kpjbu         param
099800090511      **
099900020304     c                   z-add     codpadr       kpdr
100000111205     c                   if        pardfa <> 0
100100111201     c                   move      pardfa        dataiso
100200111201     c                   move      dataiso       dataeur
100300111201     c                   move      dataeur       datafirma        10
100400111212     c* la data firma + 1
100500111212     C                   clear                   trul13ds
100600111212     c                   eval      i13fil       = parmfil
100700111212     C                   move      dataiso       i13din
100800111212     C                   EVAL      i13gio = 1
100900111212     C                   CALL      'TRUL13R'
101000111212     C                   PARM                    trul13ds
101100111212     C                   IF        o13err <> 'E'
101200111212     c                   move      o13dfi        dataiso
101300111212     c                   move      dataiso       dataeur
101400111212     c                   move      dataeur       datafirma1       10
101500111212     c                   else
101600111212     c                   eval      datafirma1 = datafirma
101700111212     c                   end
101800111212     c                   else
101900111212     c                   clear                   datafirma
102000111212     c                   clear                   datafirma1
102100111212     c                   end
102200090424     c                   seton                                        66
102300020304     c
102400090108     c     ktgt          klist
102500020304     c                   kfld                    kpdr
102600020304     c                   kfld                    ksml
102700090108     c     kfgt          klist
102800090108     c                   kfld                    kpdr
102900090108     c                   kfld                    ksml
103000090108     c                   kfld                    ktsr
103100011026
103200080305     c     kfgtsr        klist
103300080305     c                   kfld                    kpdr
103400080305     c                   kfld                    ksml
103500080305     c                   kfld                    TipSer            1
103600020313
103700020304     c     kfpt          klist
103800020304     c                   kfld                    fgtpdr
103900020304     c                   kfld                    fgtsml
104000020304     c                   kfld                    fgttsr
104100020304     c                   kfld                    fgtctr
104200020304     c                   kfld                    fgtprg
104300020117
104400020304     c     kfpd          klist
104500020304     c                   kfld                    fptpdr
104600020304     c                   kfld                    fptsml
104700020304     c                   kfld                    fpttsr
104800020304     c                   kfld                    fptctr
104900020304     c                   kfld                    fptprg
105000020304     c                   kfld                    fptctd
105100020304
105200020305     C     ktab2         klist
105300020305     C                   kfld                    codut             1 0
105400020305     C                   kfld                    cod
105500020305
105600020305     C     ktab          klist
105700020305     C                   kfld                    codut
105800020305     C                   kfld                    cod
105900020305     C                   kfld                    key
106000080915
106100080915     C     Krco          KLIST
106200090608     C                   KFLD                    tgtSOC
106300080915     C                   KFLD                    rcosnatura
106400080915     C                   KFLD                    rcoksc
106500080915     c                   eval      rcosnatura = 'F'
106600080915
106700080918     C     Kfrn          KLIST
106800090608     C                   KFLD                    tgtSOC
106900080918     C                   KFLD                    rcoksc
107000080918     C     Kcdr          KLIST
107100090608     C                   KFLD                    tgtSOC
107200080918     C                   KFLD                    FRNCONDPAG
107300080918
107400021203     c     kapd          klist
107500021203     c                   kfld                    apdtip
107600021203     c                   kfld                    kpdr
107700021203     c                   movel     'A'           apdtip
107800020402
107900020305     c                   z-add     1             codut
108000020313     C     *LIKE         DEFINE    TBLCOD        COD
108100020305     C     *LIKE         DEFINE    TBLKEY        KEY
108200020305     C     *LIKE         DEFINE    fgtpdr        kpdr
108300020305     C     *LIKE         DEFINE    fgtsml        Ksml
108400020305     C     *LIKE         DEFINE    fgttsr        Ktsr
108500020305
108600020218     C                   time                    w0120            14 0
108700020218     C                   move      w0120         wdat              8 0
108800020308      * data da impostare su data stampa e convalida se lancio = 'C'
108900020308     c                   move      wdat          dataeur
109000020308     c                   move      dataeur       dataiso
109100020308     c                   move      dataiso       dataoggi          8 0
109200020308      * data da elaborare
109300020304     c                   move      alladat       dataiso
109400020304     c                   move      dataiso       dataeur
109500020304     c                   move      dataeur       alladata          8 0
109600011026
109700090902     c                   z-add     99            CUR_line
109800090902      *
109900090902     c                   move      *all'_'       separa
110000020402     c                   select
110100020402     c                   when      lancio = 'C'
110200020411     c                   clear                   modo
110300020411     c                   seton                                        70
110400020402     c                   when      lancio = 'R'
110500090417     c******             eval      modo = 'Ristampa'
110600020402     c                   when      lancio = 'P'
110700090417     c*******            eval      modo = 'Prova   '
110800020402     c                   endsl
110900020304
111000020305     C****  TIPI PRESTAZIONE E DESCRIZIONI: TPS / DSR / CDT  ****
111100020305     C                   Z-ADD     1             X
111200020305     C                   MOVEL     '1Z'          COD
111300020305     C     KTAB2         SETLL     TABEL
111400020305     C     KTAB2         READE     TABEL                                  32
111500020305     C*
111600020305     C     *IN32         DOWEQ     *OFF
111700020305     C     TBLFLG        IFEQ      ' '
111800020305     C                   MOVEL     TBLKEY        TPS(X)
111900020305     C                   MOVEL     TBLUNI        DS1Z
112000020305     C                   MOVEL     §1ZDES        DSR(X)
112100020305     C* CODICE TARIFFA DEL TIPO PRESTAZIONE
112200020305     C                   MOVEL     §1ZCTR        CDT(X)
112300020305     C                   ADD       1             X
112400020305     C                   ENDIF
112500020305     C     KTAB2         READE     TABEL                                  32
112600020305     C                   ENDDO
112700020305     C*
112800020305     C****  TIPI SOMMA PESI E DESCRIZIONI: TSM / DSM  ****
112900020305     C                   Z-ADD     1             X
113000020305     C                   MOVEL     '1N'          COD
113100020305     C     KTAB2         SETLL     TABEL
113200020305     C     KTAB2         READE     TABEL                                  32
113300020305     C*
113400020305     C     *IN32         DOWEQ     *OFF
113500020305     C     TBLFLG        IFEQ      ' '
113600020305     C                   MOVEL     TBLKEY        TSM(X)
113700020305     C                   MOVEL     TBLUNI        DSM(X)
113800020305     C                   ADD       1             X
113900020305     C                   ENDIF
114000020305     C     KTAB2         READE     TABEL                                  32
114100020305     C                   ENDDO
114200090624      ***
114300090624     c                   endsr
114400090624     C************************************************************
114500090624     c     Costanti      begsr
114600090624     c*-------------------------------------------------
114700020412     c                   eval      costt = 'ALLEGATO TARIFFARIO IN VIGORE DAL'
114800020412     c
114900020411     c*costanti pretariffa
115000020411     c                   eval      costa2= '(di seguito denominata MITTENTE) e +
115100020411     c                             ___________________________________ codice f+
115200020411     c                             ornitore'
115300090622      ***  per autisti
115400090623     c                   IF         Allegato_x_Barca     = *blank
115500090622      ***
115600020411     c                   eval      costa3= '(di seguito denominata VETTORE) e r+
115700020411     c                             elativo alle prestazioni dell''autotrasporta+
115800020411     c                             tore __________________________'
115900020412     c                   eval      costa4= '(codice autista             ).'
116000020412     c                   eval      cost1 = 'Le parti come convenuto al punto +
116100020411     c                             12 del suddetto contratto di trasporto, con+
116200111202     c                             cordano i corrispettivi per i trasporti,'
116300111202     c                   eval      cost2 = 'comprensivi delle operazioni connes-
116400111202     c                             se al trasporto +
116500111202     c                             stesso individuate al punto 3 dello-
116600111202     c                              stesso contratto quali, a '
116700111205     c                   eval      costb = 'titolo -
116800111205     c                             puramente esemplificativo, i serviz-
116900111205     c                             i di carriaggio, selezione/identificazione d-
117000111202     c                             ei pacchi (picking) e '
117100111205     c                   eval      costb2= 'applicazione -
117200111205     c                             etichette, nonché i tempi di carico e scari-
117300111202     c                             co, secondo la seguente tariffa :'
117400090622     c                   ELSE
117500090622      ***  per Marittimi
117600090622     c                   eval      costa3= '(di seguito denominata VETTORE) e r+
117700090622     c                             elativo alle prestazioni dell''impresa _____+
117800090622     c                             _______________________________'
117900090622     c                   eval      costa4= '(codice conducente natante         -
118000090622     c                                 ).'
118100090622     c                   eval      cost1 = 'Le parti come convenuto al punto 12-
118200090622     c                              del suddetto contratto di trasporto per via-
118300090622     c                              d''acqua, concordano i corrispettivi'
118400111202     c                   eval      cost2  = 'per i trasporti e le -
118500111202     c                             operazioni connesse al trasporto stesso -
118600111202     c                             individuat-
118700090622     c                             e al punto 3 dello stesso contratto'
118800090622     c                   eval      costb = 'quali, a titolo puramente esemplifi-
118900090622     c                             cativo, i servizi di carriaggio, selezione/i-
119000090622     c                             dentificazione dei pacchi (picking)'
119100111202     c                   eval      costb2= 'e applicazione etichette, -
119200111202     c                             secondo la seguente tariffa :'
119300090622     c                   END
119400020411     c*costanti piede
119500020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
119600020411     c                             he i corrispettivi indicati nelle sopraccita+
119700020411     c                             te tariffe possano essere oggetto di'
119800020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
119900020411     c                             one, a seguito di modifiche pattuite tra le +
120000020412     c                             parti del progetto operativo'
120100020412     c                   eval      cost3c= 'originariamente concordato.'
120200090907      *
120300090907      *--------  Solo x il cliente TTL
120400090907     c                   eval      cost3TTL= 'Le parti espressamente dichiarano-
120500090907     c                              che i corrispettivi sopra indicati relativi-
120600090907     c                              alle merci affidate dalla mittente'
120700090907     c                   eval      cost3TTL1= 'TTL srl si intendono sostitutivi-
120800090907     c                              delle corrispondenti voci standard.'
120900090622      *--------
121000090622      ***  Solo per autisti
121100090623     c                   IF         Allegato_x_Barca     = *blank
121200090622      ***
121300080915     c                   eval      cost3d= 'Il vettore dichiara espressamente-
121400080915     c                              che i corrispettivi di cui al presente-
121500080915     c                              allegato sono stati concordati'
121600080915     c                   eval      cost3e= 'con il mittente sulla base del cost-
121700090903     c                             o del carburante pari a euro'
121800020306
121900090403     c                   eval      cost3_1 = 'Ai sensi e per gli effetti dell''+
122000090403     c                             art.6, comma 3 lett. e-bis D.Lgs. 286/05,il -
122100090420     c                             contratto di trasporto di cui il presente'
122200090403     c                   eval      cost3_2 = 'costituisce allegato, deve intend+
122300090420     c                             ersi integrato con l''indicazione dei +
122400090420     c                             tempi massimi di carico e scarico che'
122500090417     c                   eval      cost3_3 = 'le stesse parti indicano in compl+
122600111202     c                             essive ore 3 (tre) in relazione ai +
122700111202     c                             trasporti da effettuarsi nell''arco   '
122800090417     c                   eval      cost3_4 = 'della giornata.'
122900090417      *--------
123000090622     c                   END
123100090622      **
123200020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
123300020402     c                              società da questi delegata ad emetter+
123400020411     c                              e alla fine di ogni mese in nome e per '
123500020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
123600111202     c                             a relativa ai trasporti ed alle o+
123700111212     c                             perazioni connesse effettuati'
123800020411     c                   eval      cost5b= 'in tale periodo.'
123900020306
124000020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
124100020411     c                             di valorizzazioni giornaliere che contengon+
124200111202     c                             o in dettaglio i trasporti e '
124300020411
124400111202     c                   eval      cost6a= 'le operazioni connesse +
124500111212     c                             effettuati.'
124600090417
124700090417     c                   eval      cost6a1= 'La fattura sarà emessa con IVA ad +
124800090417     c                              esigibilità immediata.'
124900090417
125000020423     c                   eval      cost6b='Tali valorizzazioni verranno +
125100020423     c                             messe a disposizione e/o consegnate giornal+
125200080917     c                             mente al vettore il quale, a pena di'
125300020411
125400080917     c                   eval      cost6c= 'decadenza, potrà formulare le event+
125500080917     c                             uali riserve, eccezioni o osservazioni sul c+
125600080917     c                             ontenuto di ogni singola'
125700020411
125800080917     c                   eval      cost6d= 'valorizzazione entro e non oltre 10-
125900080917     c                              (dieci) giorni di calendario dalla data di -
126000080917     c                             emissione della stessa.'
126100020411
126200020411     c                   eval      costi = 'L''intestazione del vettore da rip+
126300020411     c                             ortare sulla fattura emessa è:'
126400020411
126500020411
126600080915     c                   eval      costif= 'Recapito __________________________+
126700080915     c                             ____________________________________________'
126800020411     c                   eval      costie= 'Il vettore in caso di variazione de+
126900020412     c                             ll''intestazione ne darà immediata comunic+
127000020412     c                             azione al mittente.'
127100020411
127200020423     c                   eval      cost7 = 'Il presente allegato decorre d+
127300020423     c                             alla data del __________ e costituisce part+
127400020412     c                             e integrante del contratto di trasporto'
127500020412     c                   eval      cost7b= 'suddetto.'
127600020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
127700020412     c                             la piena validità ed efficacia del contenut+
127800020412     c                             o degli articoli 12 e 13 del'
127900020412     c                   eval      cost7d='contratto di trasporto.'
128000020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
128100020411     c                             pagamento dei corrispettivi fatturati verrà +
128200090403     c                             effettuato dalla mittente mediante '
128300090403     c                   eval      cost8a = 'a _____ giorni mediante bonifico b+
128400090403     c                             ancario disposto su codice IBAN:'
128500020402
128600080208      ***
128700080208      ***  Vecchia informazione di C/C
128800050125     c                   eval      cost8c= 'Conto corrente numero  |__|__|__|__+
128900050125     c                             |__|__|__|__|__|__|__|__|                   '
129000020411     c                   eval      cost8d= 'presso banca ______________________+
129100020411     c                             ____________________________________________'
129200050125     c                   eval      cost8e= 'codice ABI   |__|__|__|__|__|      +
129300050125     c                             codice CAB   |__|__|__|__|__|  CIN   |__|   '
129400080208      ***
129500020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
129600020411     c                             lle coordinate bancarie ne darà +
129700020412     c                             immediata comunicazione al mittente.'
129800020423     c                   eval      cost8g= 'Il vettore approva espressamente +
129900020423     c                             il contenuto del presente allegato ed in +
130000020423     c                             particolare le pattuizioni relative'
130100090417      *
130200020423     c                   eval      cost8h= 'al termine di decadenza, pari a 10 +
130300090417     c                             gg di calendario, alla quantificazione, ed +
130400090417     c                              alle modalità e '
130500090417     c                   eval      cost8h1=
130600090417     c                             'termini di pagamento dei corrispettivi.'
130700090417      *
130800080917     c                   eval      cost8i= 'A decorrere dalla data odierna, -
130900080915     c                             il presente allegato annulla e sostituisce -
131000080915     c                             ogni precedente accordo'
131100080915     c                   eval      cost8l= 'scritto o verbale relativo al -
131200080915     c                             corrispettivo.'
131300020306     c
131400020411     c                   eval      cost10= 'Firma del Vettore _________________+
131500020411     c                             ____________________________________________'
131600020411     c                   eval      cost11= 'Firma del Mittente_________________+
131700020411     c                             ____________________________________________'
131800020313     c                   eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
131900020313     c                              VALIDA CON I REFERIMENTI RICHIESTI'
132000090113      ***
132100090113     c                   eval      carb01 ='Per espressa convenzione tra le par-
132200090113     c                             ti, si specifica che, non essendo possibile,-
132300090113     c                             per la tipologia dei servizi di'
132400090113     c                   eval      carb02 ='trasporto effettuati, desumere l''a-
132500090113     c                             mmontare dei chilometri effettivamente perco-
132600090113     c                             rsi per detti servizi, al fine'
132700090113     c                   eval      carb03 ='di non eludere il meccanismo di ade-
132800090113     c                             guamento automatico del costo del gasolio pr-
132900090113     c                             evisto dalla legge, si indica e'
133000090113     c                   eval      carb04 ='si forfettizza in Km.       il nume-
133100090113     c                             ro dei chilometri da prendere in considerazi-
133200090113     c                             one al fine della determinazione'
133300090113     c                   eval      carb05 ='della parte del corrispettivo dovut-
133400090113     c                             o dal mittente e corrispondente al costo del-
133500090113     c                              carburante sostenuto dal vettore.'
133600090113     c                   eval      carb06 ='Detto numero è stato individuato pr-
133700090113     c                             endendo in considerazione la percorrenza med-
133800090113     c                             ia giornaliera in base alla zona'
133900090113     c                   eval      carb07 ='di ritiro/distribuzione assegnata.'
134000090113      ***
134100011026     c                   endsr
134200090420     C************************************************************
134300090420     C*  decodifica società
134400090420     C************************************************************
134500090420     C     Decod_societa begSR
134600090420      *
134700090420     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
134800090420     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
134900090420     C                   RESET                   tibsSocI0
135000121130     C                   EVAL      tibsSocI0.idSocieta = parsocprt
135100121130     c                   move      pardfa        dataisos
135200121130     C                   EVAL      tibsSocI0.dtVALIDITA= dataisos
135300090420     C
135400090420     C                   CALL      'TIBSSOCR'
135500090420     C                   PARM      'GETANAGRAF'  prmRqsOpCode
135600090420     C                   PARM      *BLANK        prmRpyOpCode
135700090420     C                   PARM      *ZERO         prmRpyIdMsg
135800090420     C                   PARM      'TIBSSOCI0'   prmRqsFormato
135900090420     C                   PARM                    tibsSocI0
136000090420     C                   PARM                    prmRqsDataSize
136100090420     C                   PARM      'TIBSSOCO0'   prmRpyFormato
136200090420     C                   PARM                    tibsSocO0
136300090420     C                   PARM                    prmRpyDataSize
136400090420     c                   if         PRMRPYIDMSG >= 0
136500090420     c                              and TIBSSOCO0.IDSOCIETA <> *blank
136600090420     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
136700090420     c                   else
136800090420     c                   eval      wsocGd = 'Errore'
136900090429     c                   move      'E'           errore
137000090420     c                   end
137100090420      *
137200090420     C                   ENDSR
137300090420     C************************************************************
137400090608     C     Dec_SOC_FOR   begSR
137500090608      *
137600090608     C*---------- RICERCA società
137700090902     c****               clear                   desbrev          40
137800090902     c                   clear                   desbrev
137900090608      *
138000090608      ***  dalla Tariffa Supertestata
138100090608     c                   eval      wsocG = tgtSOC
138200110510     c                   move      tgtDDT        dataiso
138300090608     c                   exsr      Decod_societa
138400090608     c                   eval      desbrev = wsocGd
138500090608      ***
138600090608     c                   if        desbrev <> ' '
138700090608     c                   eval      costa = 'Allegato tariffario al contratto +
138800090608     c                             di trasporto sottoscritto tra la società ' +
138900090608     c                             DESBREV
139000090608     c                   end
139100090608     c* fornitore
139200090608     c                   movel     *all'0'       rcoksc
139300090608     c                   move      tgtCDF        rcoksc
139400090608     C     Krco          CHAIN     anrco98j                           20
139500090608     c                   if        not *in20
139600090608      *
139700090608      *  qui reimposta i 2 campi se li ha trovati su PROJ
139800090608     c                   eval      SOCTAR  = sogDES
139900090608      *
140000090608     C                   eval      costia = 'Ragione sociale ' + %trimR(sogdes)
140100090608     c                             + ' ' + %trim(sogdesagg)
140200090608     C                   eval      costib = 'Indirizzo ' + indindriz
140300090608     c                   if        indprov <> ' '
140400090608     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
140500090608     c                   else
140600090608     C                   eval      locemi = indlocalit
140700090608     c                   end
140800090608     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
140900090608     c                             locemi
141000090608     C                   eval      costid = 'Partita IVA ' + sogpartiva
141100091216      **
141200091216      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
141300091216      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
141400091216      **
141500091217      **  Aggiunge il Codice Fiscale
141600091217     c                   if        sogcdfisc <> ' ' and indprov <> ' '
141700091217     c                   eval      costiD = %Trim(costiD) +
141800091217     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
141900091217     C                             ': ' + %trimr(sogcdfisc)
142000091217     c                   end
142100091217      **
142200091217     c                   if        indtelex <> ' ' and
142300091217     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
142400091217     c                   eval      costiD = %Trim(costiD) + ' ' + indtelex
142500091217     c
142600091217     c                   end
142700091217      **
142800090608     c                   end
142900090608     c* condizioni di pagamento
143000090608     c                   movel     *all'_'       cdpdescr
143100090608     C     Kfrn          CHAIN     anfrn01l
143200090608     c                   if        %found(anfrn01l)
143300090608      *
143400090608     C     Kcdr          CHAIN     ancdp01l
143500090608     c                   end
143600090608      *
143700090608     c                   eval      cost8b= %trimr(cdpdescr) +' '+
143800090608     c                             'disposto su codice IBAN:'
143900090608     c* reperisco IBAN
144000090608     C                   RESET                   DVCOBADS
144100090608     C                   RESET                   CBAIBANDS
144200090608
144300090608     C                   EVAL      CoBSoc = tgtSOC
144400090608     C                   EVAL      CoBCOF = 'F'
144500090608     C                   EVAL      CoBKsc = rcoksc
144600090608     C                   CALL      'DVCOBA'
144700090608     C                   PARM                    DVCOBADS
144800090608A0846C                   PARM                    CBAIBANDS
144900090608     C                   IF        CoBErr = *OFF
145000090608     C                   EVAL      cost8iban = CBaStato + CbaCheck
145100090608     C                             + %TRIMR(CBaCoorI)
145200090608     c                   end
145300090608      *
145400090608     C                   ENDSR
145500090608     C************************************************************
145600090428     C     test_Pagina   begsr
145700090428      *
145800090428      * attenzione le pagine dispari hanno la casella
145900090428      *     x la posta x la data certa
146000090428     c     cur_page      div       2             pagmeta           5 0
146100090428     c                   mvr                     Pari              3 0
146200090428      *   Se l'attuale pagina è pari e c'è un salto pagina x in testazione
146300090428      *    vado a stampare una dispari con incasellamento data certa
146400090428      * Quindi accendo 66 per andare a stampare una Dispari
146500090428     c     Pari          comp      0                                      66
146600090428      *  Quindi l'accensione del 66 è invertito poichè sarà quello
146700090428      *  che condizionerà la testata futura
146800090428      *
146900090428     C                   ENDSR
147000090904     C*-----------------------------------------------------*
147100090904     C* controllo check digits
147200090904     C*-----------------------------------------------------*
147300090904     C     srchkdgt      BEGSR
147400090904      *
147500090904     c                   movel(p)  cbarre15      cbarre16         16
147600090904     C                   clear                   trul28ds
147700090904     c                   eval      i28mod = 'BAR'
147800090904     c                   eval      i28cod = cbarre16
147900090904     c                   call      'TRUL28R1'
148000090904     c                   parm                    trul28ds
148100090904     c* se reperito barcode imposto i campi in stampa
148200090904     c                   if        o28err = *blanks
148300090904     c                   move      O28CKD        cbarre16
148400090904     c                   end
148500090904      *
148600090904     C                   ENDSR
148700090904     C*-----------------------------------------------------*
148800120314     C/EJECT
148900120314     c**********************************************************************
149000120314     c* reperimento numero progressivo pagina
149100120314     c**********************************************************************
149200120314     C     repnum        BEGSR
149300120322     c                   movel     kpjbu         savkpjbu
149400120315     c                   clear                   numPROGR
149500120314     c                   clear                   trul33ds
149600120314     c                   move      ' '           i33tla
149700120314     c                   z-add     80            i33cnu
149800120314     c                   z-add     1             i33num
149900120314     c                   movel     trul33ds      kpjbu
150000120314     c                   call      'TRUL33R'
150100120314     c                   PARM                    kpjba
150200120314     c                   movel     kpjbu         trul33ds
150300120314     c                   if        o33err <> *zeros
150400120314     c                   dump
150500120314     c                   else
150600120314     c                   z-add     o33nrf        numPROGR
150700120314     c                   endif
150800120322     c                   movel     savkpjbu      kpjbu
150900120314     C                   ENDSR
151000120314     C************************************************************
151100020313**
151200160316OVRPRTF FILE(FICN14RP) TOFILE(*LIBL/FICN14RP) LPI(9) CPI(15) PAGRTT(0) SAVE(*YES)
151300160316OUTPTY(3) FORMTYPE(DOCAUTIST) PAGESIZE(115 198) OVRFLW(101) OUTQ(EMAILIN) USRDFNDTA('
151400090903DLTOVR FILE(FICN14RP)
