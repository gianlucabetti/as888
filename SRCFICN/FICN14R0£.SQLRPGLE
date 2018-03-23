000100090108     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200000000     H*--------------------------------------------------------------*
000300090108     Ffitgt01l  if   e           k disk    rename(fitgt000:fitgt001)
000400090406     Ffifgt03l  if   e           k disk
000500090406
000600020311     Ffifpt03l  if   e           k disk
000700020304     Ffifpd01l  if   e           k disk
000800021203     Ffiapd01l  if   e           k disk
000900080918     Fancdp01l  if   e           k disk
001000080918     Fanfrn01l  if   e           k disk
001100080915     FanRCO98J  iF   E           K DISK
001200090428      *
001300090428     d savpdr          s                   like(apdpdr)
001400090428      *
001500080915     D DVCOBADS      E DS
001600080915     D                                     INZ
001700080915     D CBAIBANDS     E DS
001800020304     dkpjba          e ds
001900090909     dkpjbus           S                   like (KPJBU)
002000020304
002100020304     dparam            ds
002200020308     D lancio                         1
002300020304     d codpadr                        7s 0
002400020308     D codpadr2                       7s 0
002500020304     d alladat                        8s 0
002600090424     d pardtcerta                     8s 0
002700090428     d parmNRC                        7s 0
002800090428     d parmFIL                        3s 0
002900090428     D OUTQOVR                       10
003000090428     D errore                         1
003100090526     D msgerrore                     35
003200091015     D pgmChiamante                  10
003300090909      *
003400090909      * Controllo tariffe particolari x cliente
003500090909     D ficn14DS2     E DS
003600020304
003700090108     d savprg          s                   like(tgtprg)
003800090505     d Non_trovato_dettaglio...
003900090505     d                 s              1
004000020305     d dataiso         s               d   datfmt(*iso)
004100020305     d dataeur         s               d   datfmt(*eur)
004200090420     ***************************************************************************
004300090420     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
004400090420     D ESITO_OK...
004500090420     D                 C                   0
004600090420     ***************************************************************************
004700090420     **
004800090420     ** Dichiarazione prototipi procedure esterne.
004900090420     **
005000090420     ***************************************************************************
005100090420      /DEFINE DFTACTGRP_YES
005200090420     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
005300090420      /UNDEFINE DFTACTGRP_YES
005400090420     ***************************************************************************
005500090420     **
005600090420     ** Definizione strutture dati.
005700090420     **
005800090420      **************************************************************************
005900090420     D tibsSocI0...
006000090420     D               E DS                  QUALIFIED
006100090420     D                                     INZ
006200090420     D tibsSocO0...
006300090420     D               E DS                  QUALIFIED
006400090420     D                                     INZ
006500090420     ***************************************************************************
006600090420     **
006700090420     ** Definizione variabili modulo/programma.
006800090420     **
006900090420     ***************************************************************************
007000090420     D prmRqsOpCode...
007100090420     D                 S             10A
007200090420     D prmRpyOpCode...
007300090420     D                 S             10A
007400090420     D prmRpyIdMsg...
007500090420     D                 S             10I 0
007600090420     D prmRqsFormato...
007700090420     D                 S             10A
007800090420     D prmRqsDataSize...
007900090420     D                 S             10I 0
008000090420     D prmRpyFormato...
008100090420     D                 S             10A
008200090420     D prmRpyDataSize...
008300090420     D                 S             10I 0
008400090420     D Wsocg           S                   like(apdCSF)
008500090420     D Wsocgd          S                   like(desbrev)
008600090420
008700000000     C*---------------------------------------------------------------*
008800090511     c                   seton                                        lr
008900090511      *
009000090420     ** Inizializzo il programma.
009100090420     C                   CALL      'TIBSSOCR'
009200090420     C                   PARM      'INIT'        prmRqsOpCode
009300090420     C                   PARM                    prmRpyOpCode
009400090420     C                   PARM                    prmRpyIdMsg
009500020304      *
009600020304     c                   z-add     codpadr       kpdr
009700021203     c     kapd          setll     fiapd01l
009800090511      *
009900020308     c                   do        *hival
010000021203     c     apdtip        reade     fiapd01l                               99
010100020308     c   99              leave
010200020308     c                   if        apdpdr > codpadr2
010300020308     c                   leave
010400020308     c                   endif
010500090428      *
010600090428      *  A rottura di AUTista
010700090428     c                   if        apdpdr <> savpdr
010800090428     c                   eval      savpdr = apdpdr
010900090505     c                   clear                   Manca_tariffa     1
011000090505     c                   eval       Non_trovato_dettaglio =' '
011100090428     c                   endif
011200090428      *
011300020308     c                   z-add     apdpdr        kpdr
011400090511      *
011500080915     C*---------- RICERCA DITTA :
011600080915     c                   clear                   desbrev          20
011700090420     c                   eval      wsocG = apdCSF
011800090420     c                   exsr      Decod_societa
011900090420     c                   eval      desbrev = wsocGd
012000090420
012100080915     c* fornitore
012200080915     c                   movel     *all'0'       rcoksc
012300080915     c                   move      apdksc        rcoksc
012400090511     C     Krco          CHAIN     anrco98j
012500090511     c                   if        not %Found(anrco98j)
012600090511     c                   move      'E'           errore
012700090527     c                   eval      msgerrore='TAR: ANRCO Mancante(Fornitore)'
012800090511     c                   eval      kpjbu = param
012900090511     c                   RETURN
013000090511     c                   end
013100090408      *
013200080918     c* condizioni di pagamento
013300080918     c                   movel     *all'_'       cdpdescr
013400080918     C     Kfrn          CHAIN     anfrn01l
013500080918     c                   if        %found(anfrn01l)
013600080918     C     Kcdr          CHAIN     ancdp01l
013700090511     c                   if        not %Found(ancdp01l)
013800090511     c                   move      'E'           errore
013900090527     c                   eval      msgerrore='TAR: ANCDP Mancante (Cond.Pag.)'
014000090511     c                   eval      kpjbu = param
014100090511     c                   RETURN
014200090511     c                   end
014300090511      *
014400090511     c                   else
014500090511      * manca ANFRN
014600090511     c                   move      'E'           errore
014700090527     c                   eval      msgerrore='TAR: ANFR Mancante (Fornitore)'
014800090511     c                   eval      kpjbu = param
014900090511     c                   RETURN
015000090511     c                   end
015100080915     c* reperisco IBAN
015200080915     C                   RESET                   DVCOBADS
015300080915     C                   RESET                   CBAIBANDS
015400080915
015500080915     C                   EVAL      CoBSoc = apdcsf
015600080915     C                   EVAL      CoBCOF = 'F'
015700080915     C                   EVAL      CoBKsc = rcoksc
015800080915     C                   CALL      'DVCOBA'
015900080915     C                   PARM                    DVCOBADS
016000080915A0846C                   PARM                    CBAIBANDS
016100090511     C                   IF        CoBErr <>*OFF
016200090511     c                   move      'E'           errore
016300121008     c********           eval      msgerrore='TAR:DVCOBA Manca (Reperisce IBAN)'
016400121008     c                   eval      msgerrore='TAR:Non reperisce le Coord.Bancar-
016500121008     c                             ie'
016600090511     c                   eval      kpjbu = param
016700090511     c                   RETURN
016800080915     c                   end
016900090511      *               *----------------------*
017000020308     c                   exsr      elabora
017100090511      *               *----------------------*
017200020308     c                   enddo
017300090909     C*
017400090420     ** Chiudo il programma.
017500090420     C                   CALL      'TIBSSOCR'
017600090420     C                   PARM      'FINALIZE'    prmRqsOpCode
017700090420     C                   PARM                    prmRpyOpCode
017800090420     C                   PARM                    prmRpyIdMsg
017900090504      *
018000090504      *** significa che non ha stampato dettaglio
018100090505     c                   if         Non_trovato_dettaglio ='S'
018200090511     c                   move      'E'           errore
018300090527     c                   eval      msgerrore='TAR: Manca Dettaglio Tariffa'
018400090511     c                   eval      kpjbu = param
018500090511     c                   RETURN
018600090504     c                   end
018700020304
018800090429     c                   eval      kpjbu = param
018900090511     c                   return
019000090909      *--------------------------------------------------
019100090909      *  Controlla le tariffe PARTICOLARI
019200090909      *--------------------------------------------------
019300090909     c     chk_CTP_x_Cli begSR
019400090909      *
019500090909     c                   clear                   ficn14ds2
019600090909     c                   eval      I14AUTISTA =  fgtpdr
019700090909     c                   eval      I14SIMULA  =  fgtsml
019800090909     c                   eval      I14TIPOSER =  fgttsr
019900090909     c                   eval      I14CODTARI =  fgtctr
020000090909     c                   eval      I14PROGRES =  fgtprg
020100090918     c                   eval      I14VISUALI =  'N'
020200091015     c                   eval      I14CHIAMAN =  pgmChiamante
020300090909     c
020400090909     c                   eval      kpjbuS= kpjbu
020500090909     c                   eval      kpjbu = ficn14ds2
020600090909     c                   call      'FICN14R2'
020700090909     c                   parm                    KPJBA
020800090909     c                   eval      ficn14ds2 = kpjbu
020900090909     c                   eval      kpjbu = kpjbuS
021000090909      *
021100090909     c                   if        O14Errore ='S'
021200090909     c                   move      'E'           errore
021300090909     c                   eval      msgerrore = O14ErrM35
021400090909     c                   eval      kpjbu = param
021500090909     c                   RETURN
021600090909     c                   end
021700090909      *
021800090909     c                   endSR
021900090511      *--------------------------------------------------
022000090511      *  Controlla la tariffa
022100020308      *--------------------------------------------------
022200020308     c     elabora       begsr
022300090511      *
022400090113     c                   clear                   TGT_trovato       1
022500090107     c                   eval      savprg = *hival
022600090113      *
022700090107     c* cerco il progressivo
022800090107     c     ktgt          setll     fitgt01l
022900090113      *
023000090107     c                   do        *hival
023100090107     c     ktgt          reade     fitgt01l
023200090107     c                   if        %eof(fitgt01l)
023300090107     c                   leave
023400090107     c                   end
023500090109     c* escludo le annullate
023600090109     c                   if        tgtatb <> ' '
023700090109     c                   iter
023800090109     c                   end
023900090113      *
024000090107      * non considero le tariffe che risultano fuori periodo
024100090107     c                   if        tgtddt > alladat or  tgtdst < alladat
024200090107     c                   iter
024300090107     c                   end
024400090113      *
024500090113     c                   move      'S'           TGT_trovato       1
024600090107     c                   eval      savprg = tgtprg
024700090107     c                   leave
024800090107     c                   enddo
024900090527      ****
025000090527      * Non c'è la superTestata
025100090527     c                   if        TGT_trovato = *blank
025200090527     c                   move      'S'           salta_altriFGT    1
025300090527     c                   move      'E'           errore
025400090527     c                   eval      msgerrore='TAR: FITGT mancante TESTATA TAR'
025500090527     c                   eval      kpjbu = param
025600090527     c                   RETURN
025700090527     c                   end
025800090527      *
025900090113      **
026000020304      * per giornata
026100020304     c                   move      'G'           ktsr
026200011026     c                   exsr      fifgt
026300090113      *
026400090113      * Se problemi con Supertestata non deve stampare altri FGT
026500090113     c                   if        salta_altriFGT = *blank
026600020304      * per consegna
026700020304     c                   move      'C'           ktsr
026800020304     c                   exsr      fifgt
026900090113     c                   ENDif
027000090113      *
027100090113      * Se problemi con Supertestata non deve stampare altri FGT
027200090113     c                   if        salta_altriFGT = *blank
027300020304      * per ritiro
027400020304     c                   move      'R'           ktsr
027500020304     c                   exsr      fifgt
027600090113     c                   ENDif
027700090113      **
027800020308     c                   endsr
027900090511     c*-------------------------------------------------
028000090511     C*  Controlla la testata
028100990113     c*-------------------------------------------------
028200011026     c     fifgt         begsr
028300090511     c*
028400090114     c                   clear                   salta_altriFGT    1
028500990113     c                   setoff                                           99
028600020305     c     kfgt          setll     fifgt03l
028700090113      *
028800981029     c                   do        *hival
028900020305     c     kfgt          reade     fifgt03l                               99
029000981029     c   99              leave
029100090113      *
029200090109     c* solo il progressivo della supertestata se l'ho trovata
029300090109     c                   if        fgtprg <>savprg and savprg <> *hival
029400090107     c                   iter
029500090107     c                   end
029600080918      * non considero le tariffe che risultano fuori periodo
029700020305     c                   if        fgtddt > alladat or  fgtdst < alladat
029800020114     c                   iter
029900080225     c                   end
030000090113      **
030100090113     c                   clear                   salta_altriFGT    1
030200090113      *>>>>>>>>>
030300090511      *  se già la tariffa doveva avere la supertestata (quindi >= 1/5/2009)
030400090113      * occorre controllare e segnalare
030500090113      *
030600090511     c                   if        fgtDDT >= 20090501
030700090427     c                   IF          TGT_trovato = *blank
030800090113      **
030900090113      ****
031000090511      **   se non c'è la supertestata dopo la data 1/5/2009 c'è stato un problema
031100090113      **   tecnico nella conversione o nell'inserimento della nuova tariffa, quindi,
031200090113      **   deve essere segnalato.
031300090113      ****
031400090113      * Non c'è la superTestata
031500090113     c                   if        TGT_trovato = *blank
031600090113     c                   move      'S'           salta_altriFGT    1
031700090511     c                   move      'E'           errore
031800090527     c                   eval      msgerrore='TAR: FITGT mancante TESTATA TAR'
031900090511     c                   eval      kpjbu = param
032000090511     c                   RETURN
032100090113     c                   end
032200090113      *
032300090113     c                   end
032400090113     c                   end
032500090113      *>>>>>>>>>
032600020304     c                   exsr      fifpt
032700090113      ***
032800020304     c                   enddo
032900020304
033000020304     c                   endsr
033100990113     c*-------------------------------------------------
033200011026     c     fifpt         begsr
033300990113     c*-------------------------------------------------
033400090505     c                   eval      Manca_Tariffa  = 'S'
033500020109     c
033600020304     c                   setoff                                           98
033700020311     c     kfpt          setll     fifpt03l
033800020304     c                   do        *hival
033900020311     c     kfpt          reade     fifpt03l                               98
034000020304     c   98              leave
034100020304     c                   if        fptatb <> *blank
034200020304     c                   iter
034300020304     c                   endif
034400090527     c                   eval      Manca_Tariffa  = *blank
034500090511      *
034600020304     c                   exsr      fifpd
034700090511
034800020304     c                   enddo
034900090505
035000090505     c*  se manca tariffa memorizzo il problema x restituire l'errore
035100090505     c*  a fine pgm.
035200090505     c                   If        Manca_Tariffa  = 'S'
035300090505     c                   eval      Non_trovato_dettaglio ='S'
035400090527     c                   eval      msgerrore='TAR: FIFPT Mancante Dettaglio TAR'
035500090511     c                   move      'E'           errore
035600090511     c                   eval      kpjbu = param
035700090511     c                   RETURN
035800090505     c                   end
035900090909     C*
036000090918     C*  CONTROLLA LE TARIFFE PERSONALIZZATE sul cliente:
036100090918     c                   if        TgtDTS  = 0  or
036200090918     c                             TgtDTS >= 20090918
036300090909     c                   exsr      chk_CTP_x_Cli
036400090918     c                   end
036500011026
036600990113     c                   endsr
036700990113     c*-------------------------------------------------
036800011026     c     fifpd         begsr
036900990113     c*-------------------------------------------------
037000090505     c                   eval      Manca_Tariffa  = *blank
037100090505      *
037200090505      * Solo x AUT e x tipi servizio a dettaglio
037300090505     c                   if        fgtctr <> 999
037400090505     c                   if        fgtTSR = 'C' or fgtTSR = 'R'
037500090506     c********           eval      Manca_Tariffa  = 'S'
037600090505     c                   end
037700090505     c                   end
037800020114
037900020304     c                   setoff                                           97
038000020305     c     kfpd          setll     fifpd01l
038100020304     c                   do        *hival
038200020305     c     kfpd          reade     fifpd01l                               97
038300020304     c   97              leave
038400020304     c                   if        fpdatb <> *blank
038500020304     c                   iter
038600020304     c                   endif
038700090505     c                   eval      Manca_Tariffa  = ' '
038800020304     c                   enddo
038900090505
039000090505     c*  se manca tariffa memorizzo il problema x restituire l'errore
039100090505     c*  a fine pgm.
039200090505     c                   If        Manca_Tariffa  = 'S'
039300090505     c                   eval      Non_trovato_dettaglio ='S'
039400090527     c                   eval      msgerrore='TAR: FIFPD Manca Dettaglio TAR'
039500090511     c                   move      'E'           errore
039600090511     c                   eval      kpjbu = param
039700090511     c                   RETURN
039800090505     c                   end
039900090505
040000990113     c                   endsr
040100011026     c*-------------------------------------------------
040200011026     c     *inzsr        begsr
040300011026     c*-------------------------------------------------
040400020304     c     *entry        plist
040500020304     c                   parm                    kpjba
040600020304     c                   movel     kpjbu         param
040700020304
040800020304     c                   z-add     codpadr       kpdr
040900020304     c
041000090108     c     ktgt          klist
041100020304     c                   kfld                    kpdr
041200020304     c                   kfld                    ksml
041300090108     c     kfgt          klist
041400090108     c                   kfld                    kpdr
041500090108     c                   kfld                    ksml
041600090108     c                   kfld                    ktsr
041700011026
041800020304     c     kfpt          klist
041900020304     c                   kfld                    fgtpdr
042000020304     c                   kfld                    fgtsml
042100020304     c                   kfld                    fgttsr
042200020304     c                   kfld                    fgtctr
042300020304     c                   kfld                    fgtprg
042400020117
042500020304     c     kfpd          klist
042600020304     c                   kfld                    fptpdr
042700020304     c                   kfld                    fptsml
042800020304     c                   kfld                    fpttsr
042900020304     c                   kfld                    fptctr
043000020304     c                   kfld                    fptprg
043100020304     c                   kfld                    fptctd
043200020304
043300080915     C     Krco          KLIST
043400080915     C                   KFLD                    apdcsf
043500080915     C                   KFLD                    rcosnatura
043600080915     C                   KFLD                    rcoksc
043700080915     c                   eval      rcosnatura = 'F'
043800080915
043900080918     C     Kfrn          KLIST
044000080918     C                   KFLD                    apdcsf
044100080918     C                   KFLD                    rcoksc
044200080918     C     Kcdr          KLIST
044300080918     C                   KFLD                    apdcsf
044400080918     C                   KFLD                    FRNCONDPAG
044500080918
044600021203     c     kapd          klist
044700021203     c                   kfld                    apdtip
044800021203     c                   kfld                    kpdr
044900021203     c                   movel     'A'           apdtip
045000020402
045100020305     C     *LIKE         DEFINE    fgtpdr        kpdr
045200020305     C     *LIKE         DEFINE    fgtsml        Ksml
045300020305     C     *LIKE         DEFINE    fgttsr        Ktsr
045400090113      ***
045500011026     c                   endsr
045600090420     C************************************************************
045700090420     C*  decodifica società
045800090420     C************************************************************
045900090420     C     Decod_societa begSR
046000090420      *
046100090420     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
046200090420     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
046300090420     C                   RESET                   tibsSocI0
046400090420     C                   EVAL      tibsSocI0.idSocieta = wSOCG
046500090420     C
046600090420     C                   CALL      'TIBSSOCR'
046700090420     C                   PARM      'GETANAGRAF'  prmRqsOpCode
046800090420     C                   PARM      *BLANK        prmRpyOpCode
046900090420     C                   PARM      *ZERO         prmRpyIdMsg
047000090420     C                   PARM      'TIBSSOCI0'   prmRqsFormato
047100090420     C                   PARM                    tibsSocI0
047200090420     C                   PARM                    prmRqsDataSize
047300090420     C                   PARM      'TIBSSOCO0'   prmRpyFormato
047400090420     C                   PARM                    tibsSocO0
047500090420     C                   PARM                    prmRpyDataSize
047600090420     c                   if         PRMRPYIDMSG >= 0
047700090420     c                              and TIBSSOCO0.IDSOCIETA <> *blank
047800090420     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
047900090420     c                   else
048000090527     c                   eval      msgerrore='TAR: TIBSSOCR error Decod Società'
048100090429     c                   move      'E'           errore
048200090511     c                   eval      kpjbu = param
048300090511     c                   RETURN
048400090420     c                   end
048500090420      *
048600090420     C                   ENDSR
048700090428     C************************************************************
