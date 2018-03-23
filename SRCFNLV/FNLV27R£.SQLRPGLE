000100920914     H DECEDIT('0,') DATEDIT(*DMY.)
000200120723     h dftactgrp(*no) actgrp(*caller)
000300120723ab   H BNDDIR('TIBS') EXTBININT(*YES)
000400950321?     *--------------------------------------------------------------*
000500090326?     *     Richiesta data scadenza tariffe aut x cambio P.I.        *
000600950321?     *--------------------------------------------------------------*
000700090423     Ftabel00f  iF   E           K DISK
000800090326     FFNLv27D   CF   E             WORKSTN
000900071113     Fansog01l  IF   E           K DISK
001000071113     Fanfrn01l  IF   E           K DISK
001100090330     Faitra03l  IF   E           K DISK    usropn
001200090330     Faitra05l  IF   E           K DISK    usropn
001300090330     f                                     rename(aitra000:aitra500)
001400090330     Faitrs01l  IF   E           K DISK
001500090331     Ffitgt01l  IF   E           K DISK    infds(tgtds) usropn
001600090330     Ffitgt00f  uF   E             DISK    rename(fitgt000:fitgt00)
001700090331     f                                     commit(flgcmt) usropn
001800090331     Ffiadt01l  IF   E           K DISK    infds(adtds) usropn
001900090331     Ffiadt00f  uF   E             DISK    rename(fiadt000:fiadt00)
002000090331     f                                     commit(flgcmt) usropn
002100090326     d fnlv27ds      e ds
002200071113     d kpjba         e ds
002300090330     d tgtds           ds
002400090330     D  tgtNRR               397    400B 0
002500090331     d adtds           ds
002600090331     D  adtNRR               397    400B 0
002700090330     D WLBDAT          DS
002800090330     D  G02DAT                 1      8  0
002900090330     D  G02INV                 9     16  0
003000090330     D  G02ERR                17     17
003100090330     D  G02TGI                18     22  0
003200090423     D DS5Aaut       E DS
003300090330     d dataiso         s               d   datfmt(*iso)
003400121002     d data_DecorTAR   s              8  0 inz
003500121002     d data_VIDscad    s              8  0 inz
003600090331     d maxval          s              8  0 inz
003700121001      *--
003800120607     D DSaiats       E DS                  EXTNAME(AIATS00F)
003900121001     D DSaiatsnew    E DS                  EXTNAME(AIATS00F)
004000121001     d                                     prefix(NEW)
004100121001     D DSaiatsold    E DS                  EXTNAME(AIATS00F)
004200121001     d                                     prefix(OLD)
004300121001      *--
004400120607     D partraz         DS
004500120607     D  parsoc                        3s 0
004600120607     D  pariva                       16
004700120607     D  pardtin                       8s 0
004800120607     D  pardtfi                       8s 0
004900120607     d  esito                         1
005000120607     d  errmsgt                      75
005100121001      *--
005200120723ab   ***************************************************************************
005300120723ab   **
005400120723ab   ** Prototipi.
005500120723ab   **
005600120723ab   ***************************************************************************
005700120723ab    **
005800120723ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
005900120723ab    /DEFINE DFTACTGRP_NO
006000120723ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
006100120723ab    /UNDEFINE DFTACTGRP_NO
006200120723ab   **
006300120723ab   ***************************************************************************
006400120723ab   **
006500120723ab   ** Strutture dati.
006600120723ab   **
006700120723ab   ***************************************************************************
006800120723ab   **
006900120723ab   ***************************************************************************
007000120723ab   **
007100120723ab   ** Campi.
007200120723ab   **
007300120723ab   ***************************************************************************
007400120723ab   D TibsSof_esito...
007500120723ab   D                 S             10I 0 IMPORT
007600120723ab   D idSocieta...
007700120723EDPDCD                 S              3A
007800120723ab   **
007900120723EDPDCd dtVALIDITA      s               D
008000120723EDPDCd VALdatFINE      s               D
008100120723EDPDCd VALdatINIZ      s               D
008200120723ab   **
008300120723EDPDCd dtvideo_scad    s               D
008400121002EDPDCd dtvideo_decor   s               D
008500121002EDPDCd dtViDECORR      s              8s 0
008600120723EDPDCd Filiale         s              3S 0
008700120928EDPDCd OldPartiva      s                   like(SogPARTIVA)
008800121001EDPDCd esitonew        s                   like(esito)
008900121001EDPDCd esitoold        s                   like(esito)
009000061012     c*---------------------------------------------------------------*
009100061012     c* FLUSSO PRINCIPALE                                             *
009200061012     c*---------------------------------------------------------------*
009300121001     c                   clear                   esitoold
009400121001     c                   clear                   esitonew
009500121001     c                   clear                   dsaiatsold
009600121001     c                   clear                   dsaiatsnew
009700121001      *
009800121002      **  aggancia il nuovo fornitore
009900121002     c                   exsr      SR_NEWfornit
010000121002      *  CONTRATTo
010100121002     c                   exsr      SR_contratto
010200121002      *  TARIFFa
010300121002     c                   if        d27tip = 'A'
010400121002     c                   exsr      SR_tariffa_CIT
010500121002     C                   else
010600121002     c                   exsr      SR_tariffa_AFF
010700121002     C                   end
010800061012     c*
010900090326     C                   MOVEL     FNLV27ds      KPJBU
011000061012     C                   SETON                                        LR
011100061012     c*---------------------------------------------------------------*
011200121002     c     SR_NEWfornit  begsr
011300061012     c*---------------------------------------------------------------*
011400121001     c* reperisco la p.iva del fornitore immesso in apd
011500121001     c     krco          chain     anfrn01l
011600121001     7*
011700121001     c                   if        not %found(anfrn01l)
011800121001     c                   eval      d27err = '1'
011900121002     c                   exsr      srfine
012000121001      *
012100121001     c                   else
012200121001      *
012300121001     c     frnsogg       chain     ansog01l
012400121001     c                   if        not %found(ansog01l)
012500121001     c                   eval      d27err = '1'
012600121002     c                   exsr      srfine
012700121001     c                   end
012800121001      *
012900121001     c                   end
013000121001      *
013100121002     c                   endsr
013200121001     c*---------------------------------------------------------------*
013300121002     c     SR_contratto  begsr
013400121001     c*---------------------------------------------------------------*
013500121002     c* verifico se esiste un contratto attivo, se non esiste ancora vado a
013600121002     c* fine
013700121002     c                   clear                   tranrc
013800121002     c                   clear                   trsiva
013900121002      *  CITTA'
014000121002     c                   if        d27tip = 'A'
014100121002      *
014200121002     c                   open      aitra03l
014300121002     c     ktra          chain     aitra03l
014400121002      * se non esiste oppure è stato disaccreditato precedentemente
014500121002     c                   if        not %found(aitra03l) or
014600121002     c                             (%found(aitra03l) and tradfi < oggi
014700121002     c                             and tradfi <>0)
014800121002     c                   clear                   tranrc
014900121002     c                   end
015000121002      *  AFFL.DEFL.
015100121002     c                   else
015200121002      *
015300121002     c                   open      aitra05l
015400121002     c     ktra          chain     aitra05l
015500121002      * se non esiste oppure è stato disaccreditato precedentemente
015600121002     c                   if        not %found(aitra05l) or
015700121002     c                             (%found(aitra05l) and tradfi <>0
015800121002     c                             and tradfi < oggi)
015900121002     c                   clear                   tranrc
016000121002     c                   end
016100121002      *
016200121002     c                   end
016300121002      *--------------
016400121002     c* se non trovo il contratto per l'aut/aff pulisco la partita iva
016500121002     c                   if        tranrc = 0
016600121002     c                   clear                   trsiva
016700121002     c                   else
016800121002     c     tranrc        chain     aitrs01l
016900121002     c                   if        not %found(aitrs01l) or
017000121002     c                             (%found(aitrs01l) and trsdfc <>0
017100121002     c                             and trsdfc < oggi)
017200121002     c                   exsr      srfine
017300121002     c                   end
017400121002     c                   end
017500121002      *
017600121002     c                   endsr
017700121002     c*---------------------------------------------------------------*
017800121002     c     SR_tariffa_CITbegsr
017900121002     c*---------------------------------------------------------------*
018000121002     c* se la P.IVA o il codice fornitore sono differenti
018100121002     c*
018200121002     c* verifico se esistono tariffe in vigore e nel qual caso chiedo
018300121002     c* la data di scadenza solo se allegato già confermato
018400121002      *
018500121002     c     ktgt          setgt     fitgt01l
018600121002     c                   do        *hival
018700121002     c     ktgt          readpe    fitgt01l
018800121002     c                   if        %eof(fitgt01l)
018900121002     c                   exsr      srfine
019000121002     c                   end
019100121002      *
019200121002     c                   if        TGTDST > oggi and tgtatb = ' '
019300121002      *
019400121002      * imposta i campi di confronto per l'eventuale cambio società.
019500121002     c                   eval      Filiale = %div(tgtPDR : 10000)
019600121002     c                   eval      data_DECORtar = tgtDDT
019700121002      *------
019800121002     c* se esiste il contratto con p.i. diversa ma l'ultima tariffa non è
019900121002     c* ancora convalidato emetto l'errore perchè bisogna agire prima
020000121002     c* dalle tariffe
020100121002     c                   if        (trsiva <> sogpartiva and trsiva <> ' '
020200121002     c                                    and tgtcdf = ' ')
020300121002     c                   eval      d27err = '1'
020400121002     c                   exsr      srfine
020500121002     c                   end
020600121002      *------
020700121002     c                   if        (tgtnrc <> trsnrc  and tgtnrc <> 0)
020800121002     c                                               or
020900121002     c                             (tgtcdf <> d27ksc and tgtcdf <> ' ')
021000121002     c                                             or
021100121002     c                             (tgtsoc <> d27soc and tgtsoc <> ' ')
021200121002     c                                             or
021300121002     c                             (trsiva <> sogpartiva and trsiva <> ' ')
021400121002     c                   exsr      SR_data_Scad
021500121002     c                   leave
021600121002     c                   end
021700121002      *
021800121002     c                   end
021900121002      *
022000121002     c                   enddo
022100121002      *
022200121002     c                   endsr
022300121002     c*---------------------------------------------------------------*
022400121002     c     SR_tariffa_AFFbegsr
022500121002     c*---------------------------------------------------------------*
022600121002     c* se la P.IVA o il codice fornitore sono differenti
022700121002     c*
022800121002     c* verifico se esistono tariffe in vigore e nel qual caso chiedo
022900121002     c* la data di scadenza solo se allegato già confermato
023000121002      *
023100121002     c     d27pdr        setgt     fiadt01l
023200121002     c                   do        *hival
023300121002      *
023400121002     c     d27pdr        readpe    fiadt01l
023500121002     c                   if        %eof(fiadt01l)
023600121002     c                   exsr      srfine
023700121002     c                   end
023800121002      *
023900121002     c                   if        adtDST > oggi and adtatb = ' '
024000121002      *
024100121002      * imposta i campi di confronto per il eventuale cambio società.
024200121002     c                   eval      Filiale = %div(adtPDR : 10000)
024300121002     c                   eval      data_DECORtar = adtDDT
024400121002     c                   clear                   oldPartiva
024500121002     C/EXEC SQL
024600121002     C+ SELECT sogPARTIVA INTO :oldPartiva FROM anrco98J WHERE
024700121002     C+ RCOSOCIETA = :adtCSF and RCOKSC = :adtCDF
024800121002     C/END-EXEC
024900121002     c                   exsr      sr_trulOLD
025000121002      *
025100121002      *  se non è un trazionista
025200121002     C                   IF        esitoOLD = *blank
025300121002      *
025400121002     c* se esiste il contratto con p.i. diversa ma l'ultima tariffa non è
025500121002     c* ancora convalidato emetto l'errore perchè bisogna agire prima
025600121002     c* dalle tariffe
025700121002     c                   if        (trsiva <> sogpartiva and trsiva <> ' '
025800121002     c                                    and adtcdf = ' ')
025900121002     c                   eval      d27err = '1'
026000121002     c                   exsr      srfine
026100121002     c                   end
026200121002      *
026300121002     c                   if        (adtnrc <> trsnrc and adtnrc <> 0)
026400121002     c                                               or
026500121002     c                             (adtcdf <> d27ksc and adtcdf <> ' ')
026600121002     c                                             or
026700121002     c                             (adtcsf <> d27soc and adtcsf <> ' ')
026800121002     c                                             or
026900121002     c                             (trsiva <> sogpartiva and trsiva <> ' ')
027000121002     c                   exsr      SR_data_Scad
027100121002     c                   leave
027200121002     c                   end
027300121002      *else trultraz
027400121002     c                   else
027500121002      *
027600121002     c                   if        (OLDatspiva <> sogpartiva and
027700121002     c                              OLDatspiva <> ' ' and adtcdf = ' ')
027800121002     c                   eval      d27err = '1'
027900121002     c                   exsr      srfine
028000121002     c                   end
028100121002      *
028200121002     c                   if        (adtnrc <> OLDatsnrc and adtnrc <> 0)
028300121002     c                                               or
028400121002     c                             (adtcdf <> d27ksc and adtcdf <> ' ')
028500121002     c                                             or
028600121002     c                             (adtcsf <> d27soc and adtcsf <> ' ')
028700121002     c                                             or
028800121002     c                             (OLDatspiva <> sogpartiva and
028900121002     c                              OLDatspiva <> ' ')
029000121002     c                   exsr      SR_data_Scad
029100121002     c                   leave
029200121002     c                   end
029300121002     c                   end
029400121002      *
029500121002     c                   end
029600121002      *
029700121002     c                   enddo
029800121002      *
029900121002     c                   endsr
030000121002     c*---------------------------------------------------------------*
030100121002     c     SR_data_Scad  begsr
030200121002     c*---------------------------------------------------------------*
030300121002     c                   do        *hival
030400121002     C                   EXFMT     LRZ1W01
030500121002     C*
030600121002     c                   if        *inkl
030700121002     c                   eval      d27err = '1'
030800121002     c                   exsr      srfine
030900121002     c                   end
031000121002     c*
031100121002     c                   exsr      SR_CTRdata
031200121002     c*
031300121002     c                   if        not *in01
031400121002     c                             and *inkf
031500121002     c                   exsr      sraggio
031600121002     c                   leave
031700121002     c                   end
031800121002     c                   enddo
031900121002      *
032000121002     c                   endsr
032100121002     c*---------------------------------------------------------------*
032200121002     c     SR_CTRdata    begsr
032300121002     c*---------------------------------------------------------------*
032400121002     c                   setoff                                       01
032500121002      **
032600121002     c* controllo la data immessa
032700121002     c                   if        v1dst = 0
032800121002     C                   SETON                                        01
032900121002     c                   leaveSR
033000121002     c                   else
033100121002      **
033200121002     C                   Z-ADD     V1dst         G02DAT
033300121002     C                   MOVEL     *BLANKS       G02ERR
033400121002     C                   CALL      'XSRDA8'
033500121002     C                   PARM                    WLBDAT
033600121002     C     G02ERR        IFEQ      '1'
033700121002     C                   SETON                                        01
033800121002     c                   leaveSR
033900121002     C                   ELSE
034000121002     C                   MOVE      G02INV        data_VIDscad
034100121002     c                   eval      dtvideo_scad = %date(G02inv:*iso)
034200121002      *
034300121002      * x testare la società in vigore sul nuovo periodo
034400121002      *   data scadenza + 1 gg.
034500121002     c     dtvideo_scad  adddur    1:*d          dtvideo_decor
034600121002     c                   eval      dtViDECORR = %dec(dtvideo_decor:*iso)
034700121002      *
034800121002     C                   ENDIF
034900121002      **
035000121002     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
035100121002     C                   Z-ADD     G02DAT        V1dst
035200121002     c                   end
035300121002      **
035400121002     c* deve essere > ultima valorizzazione e minore di due mesi avanti
035500121002      * e Non può essere inferiore della data Decorrenza della Tariffa
035600121002     c                   if        data_VIDscad < maxval
035700121002     c                               or
035800121002     c                             data_VIDscad > oggi2m
035900121002     c                               or
036000121002     c                             data_VIDscad < data_decorTAR
036100121002     c                   seton                                        01
036200121002     c                   leaveSR
036300121002     c                   end
036400121002      **
036500121002      ** A FRONTE DI UN CAMBIO SOCIETA OPERATIVA SULLA FILIALE:
036600121002      ** -----------------------------------------------------
036700121002      **  Controlla solo SE NON è TRAZIONISTA:
036800121002      **   La data di scadenza è corretta se appartiene al periodo della
036900121002      **   vecchia Società ossia la data scadenza NON PUO' essere maggiore
037000121002      **   alla data di fine validità della società operativa.
037100121002     C                   IF        esitoOLD = *blank and data_DECORtar > 0
037200121002     c                   eval      dtVALIDITA = %date(data_DECORtar:*iso)
037300121002     c                   exsr      CHK_SOC_OPE
037400121002      * errore ricerca società operativa
037500121002     c                   if        *in02 = *on
037600121002     c                   leaveSR
037700121002     c                   end
037800121002      *
037900121002      ** Ritorna con la data limite della società in presenza
038000121002      **  di un cambio societario o di semplice limite scadenza Società
038100121002     c                   if        dtvideo_scad > valDatFine
038200121002     c                   seton                                        01
038300121002     c                   leaveSR
038400121002     c                   end
038500121002      **
038600121002     c                   end
038700121002      **
038800121002      ** Allo STESSO MODO  deve controllare che il nuovo Fornitore
038900121002      ** A FRONTE DI UN CAMBIO SOCIETA OPERATIVA SULLA FILIALE:
039000121002      ** -----------------------------------------------------
039100121002      **  Controlla che il nuovo FORNITORE NON sia un TRAZIONISTA:
039200121002      **   per verificare che la data di scadenza della tariffa non finisca
039300121002      **  sul periodo della nuova Società Operativa
039400121002     c                   exsr      sr_trulNEW
039500121002      **
039600121002      **  Controlla solo se NON E'un TRAZIONISTA
039700121002      *
039800121002     C                   IF        esitoNEW = *blank and dtViDECORR > 0
039900121002     c                   eval      dtVALIDITA = dtvideo_decor
040000121002     c                   exsr      CHK_SOC_OPE
040100121003      * Se la società NON corrisponde a quella del nuovo FORNITORE
040200121003     c                   if        D27SOC <> idSocieta
040300121003     c                   seton                                        02
040400121003     c                   end
040500121002      * errore ricerca società operativa
040600121002     c                   if        *in02 = *on
040700121002     c                   leaveSR
040800121002     c                   end
040900121002      *
041000121002      **  la data scadenza non deve essere superiore alla data di inizio
041100121002      **  validità della società operativa
041200121002     c                   if        dtvideo_scad > valDatIniz
041300121018     c                             and valDatIniz > *loval
041400121002     c                   seton                                        01
041500121002     c                   leaveSR
041600121002     c                   end
041700121002      **
041800121002     c                   end
041900121002      **
042000121002     c                   endsr
042100121002     c*---------------------------------------------------------------*
042200121002     c     sr_trulNEW    begsr
042300121002     c*---------------------------------------------------------------*
042400121002      *
042500121002      *  identifico se Trazionista nel periodo seguente a quello da far
042600121002      *   scadere impostando la data scadenza + 1 ossia la possibile
042700121002      *   data decorrenza.
042800121002     c                   clear                   partraz
042900121002     c                   move      frnSOCIETA    parsoc
043000121002     c                   eval      pardtIN = %dec(dtvideo_decor:*iso)
043100121002     c                   eval      pardtFI = %dec(dtvideo_decor:*iso)
043200121002     c                   eval      pariva  = sogPARTIVA
043300121002      *
043400121002     c                   exsr      sr_trultraz
043500121002      *
043600121002     c                   eval        esitoNEW = esito
043700121002     c                   eval      dsaiatsNEW = dsaiats
043800121002      *
043900121002     c                   endsr
044000121002     c*---------------------------------------------------------------*
044100121002     c     sr_trulOLD    begsr
044200121002     c*---------------------------------------------------------------*
044300121002      *
044400121002     c                   clear                   partraz
044500121002     c                   move      adtcsf        parsoc
044600121002     c                   move      adtddt        pardtin
044700121002     c                   move      adtdst        pardtfi
044800121002     c                   movel     oldpartiva    pariva
044900121002      *
045000121002     c                   exsr      sr_trultraz
045100121002      *
045200121002     c                   eval        esitoOLD = esito
045300121002     c                   eval      dsaiatsOLD = dsaiats
045400121002      *
045500121002     c                   endsr
045600121002     c*---------------------------------------------------------------*
045700121002     c     sr_trultraz   begsr
045800121002     c*---------------------------------------------------------------*
045900121002      *
046000121001     c                   clear                   esito
046100121001     c                   clear                   dsAIATS
046200121001     c                   call      'TRULTRAZ'
046300121001     c                   parm                    partraz
046400121001     c                   parm                    dsAIATS
046500121001      *
046600121001     c                   endsr
046700090330     c*---------------------------------------------------------------*
046800090330     c     srfine        begsr
046900090330     c*---------------------------------------------------------------*
047000090330     C                   MOVEL     FNLV27ds      KPJBU
047100090330     C                   SETON                                        LR
047200090330     c                   return
047300090330     c                   endsr
047400090330     c*---------------------------------------------------------------*
047500090330     c     sraggio       begsr
047600090330     c*---------------------------------------------------------------*
047700090331     c                   if        d27tip = 'A'
047800121002      **
047900090330     c     tgtnrr        chain     fitgt00f
048000090330     c                   if        %found(fitgt00f)
048100121002     c                   eval      tgtdst = data_VIDscad
048200090330     c                   eval      tgtduv = oggi
048300090330     c                   eval      tgtpru = knmus
048400090330     c                   update    fitgt00
048500090330     c                   end
048600090331     c* aggiorno il FIFGT
048700090331     c                   call      'FICN03R'
048800090331     c                   parm                    tgtpdr
048900090331     c                   parm                    tgtsml
049000090331     c                   parm                    tgtprg
049100090331     c                   parm                    d27cmt
049200121002      **
049300090331     c                   else
049400121002      **
049500090331     c     adtnrr        chain     fiadt00f
049600090331     c                   if        %found(fiadt00f)
049700121002     c                   eval      adtdst = data_VIDscad
049800090331     c                   eval      adtduv = oggi
049900090331     c                   update    fiadt00
050000090331     c                   end
050100121002      **
050200090331     c                   end
050300121002      **
050400090330     c                   endsr
050500090331     c**********************************************************************
050600090331     c* reperimento la data maggiore di valorizzazione
050700090331     c**********************************************************************
050800090331     C     srmaxval      BEGSR
050900121002      **
051000090331     c                   if        d27tip = 'A'
051100090331     C/EXEC SQL
051200090428     C+ SELECT max(fttddc) INTO :maxval FROM fiftt00f WHERE FTTPDR =
051300090717     C+ :d27pdr
051400090331     C/END-EXEC
051500090331     c                   else
051600090331     C/EXEC SQL
051700090428     C+ SELECT max(attddc) INTO :maxval FROM fiatt00f WHERE aTTPDR =
051800090717     C+ :d27pdr
051900090331     C/END-EXEC
052000090331     c                   end
052100121002      **
052200090331     C                   ENDSR
052300071113     c*---------------------------------------------------------------*
052400071113     c     *inzsr        begsr
052500071113     c*---------------------------------------------------------------*
052600061012     c*  Operazioni iniziali
052700061012     C     *ENTRY        PLIST
052800061012     C                   PARM                    KPJBA
052900090330     C                   MOVEL     KPJBU         FNLv27ds
053000121001      **
053100090330     c                   clear                   d27err
053200090331     c                   if        d27cmt = '1'
053300090331     c                   eval      flgcmt = *on
053400090331     c                   else
053500090331     c                   eval      flgcmt = *off
053600090331     c                   end
053700121001      **
053800090331     c                   if        d27tip = 'A'
053900090331     c                   open      fitgt00f
054000090331     c                   open      fitgt01l
054100090331     c                   else
054200090331     c                   open      fiadt01l
054300090331     c                   open      fiadt00f
054400090331     c                   end
054500090331     c* reperisce la data max valorizzata, la scadenza non può essere <
054600090331     c* delle valorizzaz.
054700090331     c                   exsr      srmaxval
054800090331     c*
054900071113     C     Krco          KLIST
055000090330     C                   KFLD                    d27soc
055100090330     C                   KFLD                    d27ksc
055200090330     C     Ktgt          KLIST
055300090330     C                   KFLD                    d27pdr
055400090330     C                   KFLD                    tgtsml
055500090330     c                   clear                   tgtsml
055600090330     C     Ktra          KLIST
055700090330     C                   KFLD                    d27pdr
055800090330     C                   KFLD                    tradfi
055900090330     c                   clear                   tradfi
056000090423     C     Ktab          KLIST
056100090423     C                   KFLD                    tblkut
056200090423     C                   KFLD                    tblcod
056300090423     C                   KFLD                    tblkey
056400090330     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
056500090330     C                   TIME                    W0120            14 0
056600090330     C                   MOVE      W0120         WDAT              8 0
056700090330     C*
056800090330     C                   Z-ADD     WDAT          G02DAT
056900090330     C                   MOVEL     *BLANK        G02ERR
057000090330     C                   CALL      'XSRDA8'
057100090330     C                   PARM                    WLBDAT
057200090330     c                   movel     G02INV        oggi              8 0
057300090423     c* reperisce gg min durata allegato
057400090423     C                   movel     '5A'          tblcod
057500090423     C                   MOVEL(P)  'AUT'         tblKEY
057600090423     c                   z-add     1             tblkut
057700090423     c                   z-add     60            giomin
057800090423     C     KTAB          CHAIN     TABEL00F
057900090423     c                   if        %found(tabel00f)
058000090423     C                   movel     tbluni        ds5Aaut
058100090423     c                   if        §5AGGcpi <> 0
058200090423     C                   z-add     §5AGGcpi      giomin            3 0
058300090423     c                   end
058400090423     c                   end
058500090423     c                   move      oggi          dataiso
058600090423     c                   adddur    giomin:*d     dataiso
058700090423     c                   move      dataiso       oggi2m            8 0
058800120723ab   C*     Inizializza
058900120723ab    /FREE
059000120723ab     TibsSof_Init();
059100120723ab    /END-FREE
059200120723     C*
059300061012     c                   endsr
059400120723ab   C**************************************************************************
059500120723ab    * Controlla la Società operativa della Filiale e quella dei trazionisti
059600120723ab   C**************************************************************************
059700120723ab   C     CHK_SOC_OPE   begsr
059800120723ab   **
059900121002ab   ** verificare la società operativa in vigore
060000120723ab    /FREE
060100121002
060200120723EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
060300120723EDPDC                                             : dtVALIDITA
060400120723EDPDC                                             : 'O'
060500120723EDPDC                                             : valDatIniz
060600120723EDPDC                                             : valDatFine
060700120723ab                                                );
060800120723ab
060900121002EDPDC    // Gestire l'errore.  errore sulla data.
061000120723EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
061100120724           *in02 = '1';
061200120723ab     ENDIF;
061300120723ab
061400120723ab    /END-FREE
061500120723ab   C                   ENDSR
061600120723     C**************************************************************************
