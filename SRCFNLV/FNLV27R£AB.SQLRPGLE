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
003500121214     d data_ScadeTAR   s              8  0 inz
003600121002     d data_VIDscad    s              8  0 inz
003700090331     d maxval          s              8  0 inz
003800121001      *--
003900120607     D DSaiats       E DS                  EXTNAME(AIATS00F)
004000121001     D DSaiatsnew    E DS                  EXTNAME(AIATS00F)
004100121001     d                                     prefix(NEW)
004200121001     D DSaiatsold    E DS                  EXTNAME(AIATS00F)
004300121001     d                                     prefix(OLD)
004400121001      *--
004500120607     D partraz         DS
004600120607     D  parsoc                        3s 0
004700120607     D  pariva                       16
004800120607     D  pardtin                       8s 0
004900120607     D  pardtfi                       8s 0
005000120607     d  esito                         1
005100120607     d  errmsgt                      75
005200121001      *--
005300120723ab   ***************************************************************************
005400120723ab   **
005500120723ab   ** Prototipi.
005600120723ab   **
005700120723ab   ***************************************************************************
005800120723ab    **
005900120723ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
006000120723ab    /DEFINE DFTACTGRP_NO
006100120723ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
006200120723ab    /UNDEFINE DFTACTGRP_NO
006300120723ab   **
006400120723ab   ***************************************************************************
006500120723ab   **
006600120723ab   ** Strutture dati.
006700120723ab   **
006800120723ab   ***************************************************************************
006900120723ab   **
007000120723ab   ***************************************************************************
007100120723ab   **
007200120723ab   ** Campi.
007300120723ab   **
007400120723ab   ***************************************************************************
007500120723ab   D TibsSof_esito...
007600120723ab   D                 S             10I 0 IMPORT
007700120723ab   D idSocieta...
007800120723EDPDCD                 S              3A
007900120723ab   **
008000120723EDPDCd dtVALIDITA      s               D
008100120723EDPDCd VALdatFINE      s               D
008200120723EDPDCd VALdatINIZ      s               D
008300120723ab   **
008400120723EDPDCd dtvideo_scad    s               D
008500121002EDPDCd dtvideo_decor   s               D
008600121002EDPDCd dtViDECORR      s              8s 0
008700120723EDPDCd Filiale         s              3S 0
008800120928EDPDCd OldPartiva      s                   like(SogPARTIVA)
008900121001EDPDCd esitonew        s                   like(esito)
009000121001EDPDCd esitoold        s                   like(esito)
009100061012     c*---------------------------------------------------------------*
009200061012     c* FLUSSO PRINCIPALE                                             *
009300061012     c*---------------------------------------------------------------*
009400121001     c                   clear                   esitoold
009500121001     c                   clear                   esitonew
009600121001     c                   clear                   dsaiatsold
009700121001     c                   clear                   dsaiatsnew
009800121001      *
009900121002      **  aggancia il nuovo fornitore
010000121002     c                   exsr      SR_NEWfornit
010100121002      *  CONTRATTo
010200121002     c                   exsr      SR_contratto
010300121002      *  TARIFFa
010400121002     c                   if        d27tip = 'A'
010500121002     c                   exsr      SR_tariffa_CIT
010600121002     C                   else
010700121002     c                   exsr      SR_tariffa_AFF
010800121002     C                   end
010900061012     c*
011000090326     C                   MOVEL     FNLV27ds      KPJBU
011100061012     C                   SETON                                        LR
011200061012     c*---------------------------------------------------------------*
011300121002     c     SR_NEWfornit  begsr
011400061012     c*---------------------------------------------------------------*
011500121001     c* reperisco la p.iva del fornitore immesso in apd
011600121001     c     krco          chain     anfrn01l
011700121001     7*
011800121001     c                   if        not %found(anfrn01l)
011900121001     c                   eval      d27err = '1'
012000121002     c                   exsr      srfine
012100121001      *
012200121001     c                   else
012300121001      *
012400121001     c     frnsogg       chain     ansog01l
012500121001     c                   if        not %found(ansog01l)
012600121001     c                   eval      d27err = '1'
012700121002     c                   exsr      srfine
012800121001     c                   end
012900121001      *
013000121001     c                   end
013100121001      *
013200121002     c                   endsr
013300121001     c*---------------------------------------------------------------*
013400121002     c     SR_contratto  begsr
013500121001     c*---------------------------------------------------------------*
013600121002     c* verifico se esiste un contratto attivo, se non esiste ancora vado a
013700121002     c* fine
013800121002     c                   clear                   tranrc
013900121002     c                   clear                   trsiva
014000121002      *  CITTA'
014100121002     c                   if        d27tip = 'A'
014200121002      *
014300121002     c                   open      aitra03l
014400121002     c     ktra          chain     aitra03l
014500121002      * se non esiste oppure è stato disaccreditato precedentemente
014600121002     c                   if        not %found(aitra03l) or
014700121002     c                             (%found(aitra03l) and tradfi < oggi
014800121002     c                             and tradfi <>0)
014900121002     c                   clear                   tranrc
015000121002     c                   end
015100121002      *  AFFL.DEFL.
015200121002     c                   else
015300121002      *
015400121002     c                   open      aitra05l
015500121002     c     ktra          chain     aitra05l
015600121002      * se non esiste oppure è stato disaccreditato precedentemente
015700121002     c                   if        not %found(aitra05l) or
015800121002     c                             (%found(aitra05l) and tradfi <>0
015900121002     c                             and tradfi < oggi)
016000121002     c                   clear                   tranrc
016100121002     c                   end
016200121002      *
016300121002     c                   end
016400121002      *--------------
016500121002     c* se non trovo il contratto per l'aut/aff pulisco la partita iva
016600121002     c                   if        tranrc = 0
016700121002     c                   clear                   trsiva
016800121002     c                   else
016900121002     c     tranrc        chain     aitrs01l
017000121002     c                   if        not %found(aitrs01l) or
017100121002     c                             (%found(aitrs01l) and trsdfc <>0
017200121002     c                             and trsdfc < oggi)
017300121002     c                   exsr      srfine
017400121002     c                   end
017500121002     c                   end
017600121002      *
017700121002     c                   endsr
017800121002     c*---------------------------------------------------------------*
017900121002     c     SR_tariffa_CITbegsr
018000121002     c*---------------------------------------------------------------*
018100121002     c* se la P.IVA o il codice fornitore sono differenti
018200121002     c*
018300121002     c* verifico se esistono tariffe in vigore e nel qual caso chiedo
018400121002     c* la data di scadenza solo se allegato già confermato
018500121002      *
018600121002     c     ktgt          setgt     fitgt01l
018700121002     c                   do        *hival
018800121002     c     ktgt          readpe    fitgt01l
018900121002     c                   if        %eof(fitgt01l)
019000121002     c                   exsr      srfine
019100121002     c                   end
019200121002      *
019300121002     c                   if        TGTDST > oggi and tgtatb = ' '
019400121002      *
019500121002      * imposta i campi di confronto per l'eventuale cambio società.
019600121002     c                   eval      Filiale = %div(tgtPDR : 10000)
019700121002     c                   eval      data_DECORtar = tgtDDT
019800121214     c                   eval      data_SCADEtar = tgtDST
019900121002      *------
020000121002     c* se esiste il contratto con p.i. diversa ma l'ultima tariffa non è
020100121002     c* ancora convalidato emetto l'errore perchè bisogna agire prima
020200121002     c* dalle tariffe
020300121002     c                   if        (trsiva <> sogpartiva and trsiva <> ' '
020400121002     c                                    and tgtcdf = ' ')
020500121002     c                   eval      d27err = '1'
020600121002     c                   exsr      srfine
020700121002     c                   end
020800121002      *------
020900121002     c                   if        (tgtnrc <> trsnrc  and tgtnrc <> 0)
021000121002     c                                               or
021100121002     c                             (tgtcdf <> d27ksc and tgtcdf <> ' ')
021200121002     c                                             or
021300121002     c                             (tgtsoc <> d27soc and tgtsoc <> ' ')
021400121002     c                                             or
021500121002     c                             (trsiva <> sogpartiva and trsiva <> ' ')
021600121002     c                   exsr      SR_data_Scad
021700121002     c                   leave
021800121002     c                   end
021900121002      *
022000121002     c                   end
022100121002      *
022200121002     c                   enddo
022300121002      *
022400121002     c                   endsr
022500121002     c*---------------------------------------------------------------*
022600121002     c     SR_tariffa_AFFbegsr
022700121002     c*---------------------------------------------------------------*
022800121002     c* se la P.IVA o il codice fornitore sono differenti
022900121002     c*
023000121002     c* verifico se esistono tariffe in vigore e nel qual caso chiedo
023100121002     c* la data di scadenza solo se allegato già confermato
023200121002      *
023300121002     c     d27pdr        setgt     fiadt01l
023400121002     c                   do        *hival
023500121002      *
023600121002     c     d27pdr        readpe    fiadt01l
023700121002     c                   if        %eof(fiadt01l)
023800121002     c                   exsr      srfine
023900121002     c                   end
024000121002      *
024100121002     c                   if        adtDST > oggi and adtatb = ' '
024200121002      *
024300121002      * imposta i campi di confronto per il eventuale cambio società.
024400121214     c                   eval      Filiale = %div(adtPDR : 10000)
024500121002     c                   eval      data_DECORtar = adtDDT
024600121214     c                   eval      data_SCADEtar = adtDST
024700121002     c                   clear                   oldPartiva
024800121002     C/EXEC SQL
024900121002     C+ SELECT sogPARTIVA INTO :oldPartiva FROM anrco98J WHERE
025000121002     C+ RCOSOCIETA = :adtCSF and RCOKSC = :adtCDF
025100121002     C/END-EXEC
025200121002     c                   exsr      sr_trulOLD
025300121002      *
025400121002      *  se non è un trazionista
025500121002     C                   IF        esitoOLD = *blank
025600121002      *
025700121002     c* se esiste il contratto con p.i. diversa ma l'ultima tariffa non è
025800121002     c* ancora convalidato emetto l'errore perchè bisogna agire prima
025900121002     c* dalle tariffe
026000121002     c                   if        (trsiva <> sogpartiva and trsiva <> ' '
026100121002     c                                    and adtcdf = ' ')
026200121002     c                   eval      d27err = '1'
026300121002     c                   exsr      srfine
026400121002     c                   end
026500121002      *
026600121002     c                   if        (adtnrc <> trsnrc and adtnrc <> 0)
026700121002     c                                               or
026800121002     c                             (adtcdf <> d27ksc and adtcdf <> ' ')
026900121002     c                                             or
027000121002     c                             (adtcsf <> d27soc and adtcsf <> ' ')
027100121002     c                                             or
027200121002     c                             (trsiva <> sogpartiva and trsiva <> ' ')
027300121002     c                   exsr      SR_data_Scad
027400121002     c                   leave
027500121002     c                   end
027600121002      *else trultraz
027700121002     c                   else
027800121002      *
027900121002     c                   if        (OLDatspiva <> sogpartiva and
028000121002     c                              OLDatspiva <> ' ' and adtcdf = ' ')
028100121002     c                   eval      d27err = '1'
028200121002     c                   exsr      srfine
028300121002     c                   end
028400121002      *
028500121002     c                   if        (adtnrc <> OLDatsnrc and adtnrc <> 0)
028600121002     c                                               or
028700121002     c                             (adtcdf <> d27ksc and adtcdf <> ' ')
028800121002     c                                             or
028900121002     c                             (adtcsf <> d27soc and adtcsf <> ' ')
029000121002     c                                             or
029100121002     c                             (OLDatspiva <> sogpartiva and
029200121002     c                              OLDatspiva <> ' ')
029300121002     c                   exsr      SR_data_Scad
029400121002     c                   leave
029500121002     c                   end
029600121002     c                   end
029700121002      *
029800121002     c                   end
029900121002      *
030000121002     c                   enddo
030100121002      *
030200121002     c                   endsr
030300121002     c*---------------------------------------------------------------*
030400121002     c     SR_data_Scad  begsr
030500121002     c*---------------------------------------------------------------*
030600121002     c                   do        *hival
030700121002     C                   EXFMT     LRZ1W01
030800121002     C*
030900121002     c                   if        *inkl
031000121002     c                   eval      d27err = '1'
031100121002     c                   exsr      srfine
031200121002     c                   end
031300121002     c*
031400121002     c                   exsr      SR_CTRdata
031500121002     c*
031600121002     c                   if        not *in01
031700121002     c                             and *inkf
031800121002     c                   exsr      sraggio
031900121002     c                   leave
032000121002     c                   end
032100121002     c                   enddo
032200121002      *
032300121002     c                   endsr
032400121002     c*---------------------------------------------------------------*
032500121002     c     SR_CTRdata    begsr
032600121002     c*---------------------------------------------------------------*
032700121002     c                   setoff                                       01
032800121002      **
032900121002     c* controllo la data immessa
033000121002     c                   if        v1dst = 0
033100121002     C                   SETON                                        01
033200121002     c                   leaveSR
033300121002     c                   else
033400121002      **
033500121002     C                   Z-ADD     V1dst         G02DAT
033600121002     C                   MOVEL     *BLANKS       G02ERR
033700121002     C                   CALL      'XSRDA8'
033800121002     C                   PARM                    WLBDAT
033900121002     C     G02ERR        IFEQ      '1'
034000121002     C                   SETON                                        01
034100121002     c                   leaveSR
034200121002     C                   ELSE
034300121002     C                   MOVE      G02INV        data_VIDscad
034400121002     c                   eval      dtvideo_scad = %date(G02inv:*iso)
034500121002      *
034600121002      * x testare la società in vigore sul nuovo periodo
034700121002      *   data scadenza + 1 gg.
034800121002     c     dtvideo_scad  adddur    1:*d          dtvideo_decor
034900121002     c                   eval      dtViDECORR = %dec(dtvideo_decor:*iso)
035000121002      *
035100121002     C                   ENDIF
035200121002      **
035300121002     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
035400121002     C                   Z-ADD     G02DAT        V1dst
035500121002     c                   end
035600121002      **
035700121002     c* deve essere > ultima valorizzazione e minore di due mesi avanti
035800121002      * e Non può essere inferiore della data Decorrenza della Tariffa
035900121002     c                   if        data_VIDscad < maxval
036000121002     c                               or
036100121002     c                             data_VIDscad > oggi2m
036200121002     c                               or
036300121002     c                             data_VIDscad < data_decorTAR
036400121002     c                   seton                                        01
036500121002     c                   leaveSR
036600121002     c                   end
036700121002      **
036800121002      ** A FRONTE DI UN CAMBIO SOCIETA OPERATIVA SULLA FILIALE:
036900121002      ** -----------------------------------------------------
037000121002      **  Controlla solo SE NON è TRAZIONISTA:
037100121002      **   La data di scadenza è corretta se appartiene al periodo della
037200121002      **   vecchia Società ossia la data scadenza NON PUO' essere maggiore
037300121002      **   alla data di fine validità della società operativa.
037400121002     C                   IF        esitoOLD = *blank and data_DECORtar > 0
037500121214      ***  Causa trascodifica Società è meglio controllare con la data di scaden
037600121214      ***  con la data di decorrenza (avendo TRASCODIFICATO senza aver chiuso e
037700121214      ***  tutti i records delle tariffe dei contratti etc.etc. in tal modo la s
037800121214      ***  è più sicura poichè compresa sicuramente entro l'infinito come limite
037900121214      ***  record della Società).
038000121214     c**************     eval      dtVALIDITA = %date(data_DECORtar:*iso)
038100121214     c                   eval      dtVALIDITA = %date(data_SCADEtar:*iso)
038200121002     c                   exsr      CHK_SOC_OPE
038300121002      * errore ricerca società operativa
038400121002     c                   if        *in02 = *on
038500121002     c                   leaveSR
038600121002     c                   end
038700121002      *
038800121002      ** Ritorna con la data limite della società in presenza
038900121002      **  di un cambio societario o di semplice limite scadenza Società
039000121002     c                   if        dtvideo_scad > valDatFine
039100121218     c                               and idsocieta <> d27soc
039200121218      *  X la trascodifica:
039300121218      **     si deve aggiungere in "AND" anche se le società sono diverse
039400121218      ** (fra AZSOF e quella a VIDEO) altrimenti, essendo la società la stessa,
039500121218      **  deve lasciare stare questo controllo ... senza alcun problema.
039600121218      *
039700121002     c                   seton                                        01
039800121002     c                   leaveSR
039900121002     c                   end
040000121002      **
040100121002     c                   end
040200121002      **
040300121002      ** Allo STESSO MODO  deve controllare che il nuovo Fornitore
040400121002      ** A FRONTE DI UN CAMBIO SOCIETA OPERATIVA SULLA FILIALE:
040500121002      ** -----------------------------------------------------
040600121002      **  Controlla che il nuovo FORNITORE NON sia un TRAZIONISTA:
040700121002      **   per verificare che la data di scadenza della tariffa non finisca
040800121002      **  sul periodo della nuova Società Operativa
040900121002     c                   exsr      sr_trulNEW
041000121002      **
041100121002      **  Controlla solo se NON E'un TRAZIONISTA
041200121002      *
041300121002     C                   IF        esitoNEW = *blank and dtViDECORR > 0
041400121214      ***  Causa trascodifica Società è meglio controllare con la data di scaden
041500121214      ***  con la data di decorrenza (avendo TRASCODIFICATO senza aver chiuso e
041600121214      ***  tutti i records delle tariffe dei contratti etc.etc. in tal modo la s
041700121214      ***  è più sicura poichè compresa sicuramente entro l'infinito come limite
041800121214      ***  record della Società).
041900121002     c                   eval      dtVALIDITA = dtvideo_decor
042000121002     c                   exsr      CHK_SOC_OPE
042100121003      * Se la società NON corrisponde a quella del nuovo FORNITORE
042200121003     c                   if        D27SOC <> idSocieta
042300121003     c                   seton                                        02
042400121003     c                   end
042500121002      * errore ricerca società operativa
042600121002     c                   if        *in02 = *on
042700121002     c                   leaveSR
042800121002     c                   end
042900121002      *
043000121002      **  la data scadenza non deve essere superiore alla data di inizio
043100121002      **  validità della società operativa
043200121002     c                   if        dtvideo_scad > valDatIniz
043300121018     c                             and valDatIniz > *loval
043400121218     c                               and idsocieta <> d27soc
043500121218      *  X la trascodifica:
043600121218      **     si deve aggiungere in "AND" anche se le società sono diverse
043700121218      ** (fra AZSOF e quella a VIDEO) altrimenti, essendo la società la stessa,
043800121218      **  deve lasciare stare questo controllo ... senza alcun problema.
043900121218      *
044000121002     c                   seton                                        01
044100121002     c                   leaveSR
044200121002     c                   end
044300121002      **
044400121002     c                   end
044500121002      **
044600121002     c                   endsr
044700121002     c*---------------------------------------------------------------*
044800121002     c     sr_trulNEW    begsr
044900121002     c*---------------------------------------------------------------*
045000121002      *
045100121002      *  identifico se Trazionista nel periodo seguente a quello da far
045200121002      *   scadere impostando la data scadenza + 1 ossia la possibile
045300121002      *   data decorrenza.
045400121002     c                   clear                   partraz
045500121002     c                   move      frnSOCIETA    parsoc
045600121002     c                   eval      pardtIN = %dec(dtvideo_decor:*iso)
045700121002     c                   eval      pardtFI = %dec(dtvideo_decor:*iso)
045800121002     c                   eval      pariva  = sogPARTIVA
045900121002      *
046000121002     c                   exsr      sr_trultraz
046100121002      *
046200121002     c                   eval        esitoNEW = esito
046300121002     c                   eval      dsaiatsNEW = dsaiats
046400121002      *
046500121002     c                   endsr
046600121002     c*---------------------------------------------------------------*
046700121002     c     sr_trulOLD    begsr
046800121002     c*---------------------------------------------------------------*
046900121002      *
047000121002     c                   clear                   partraz
047100121002     c                   move      adtcsf        parsoc
047200121002     c                   move      adtddt        pardtin
047300121002     c                   move      adtdst        pardtfi
047400121002     c                   movel     oldpartiva    pariva
047500121002      *
047600121002     c                   exsr      sr_trultraz
047700121002      *
047800121002     c                   eval        esitoOLD = esito
047900121002     c                   eval      dsaiatsOLD = dsaiats
048000121002      *
048100121002     c                   endsr
048200121002     c*---------------------------------------------------------------*
048300121002     c     sr_trultraz   begsr
048400121002     c*---------------------------------------------------------------*
048500121002      *
048600121001     c                   clear                   esito
048700121001     c                   clear                   dsAIATS
048800121001     c                   call      'TRULTRAZ'
048900121001     c                   parm                    partraz
049000121001     c                   parm                    dsAIATS
049100121001      *
049200121001     c                   endsr
049300090330     c*---------------------------------------------------------------*
049400090330     c     srfine        begsr
049500090330     c*---------------------------------------------------------------*
049600090330     C                   MOVEL     FNLV27ds      KPJBU
049700090330     C                   SETON                                        LR
049800090330     c                   return
049900090330     c                   endsr
050000090330     c*---------------------------------------------------------------*
050100090330     c     sraggio       begsr
050200090330     c*---------------------------------------------------------------*
050300090331     c                   if        d27tip = 'A'
050400121002      **
050500090330     c     tgtnrr        chain     fitgt00f
050600090330     c                   if        %found(fitgt00f)
050700121002     c                   eval      tgtdst = data_VIDscad
050800090330     c                   eval      tgtduv = oggi
050900090330     c                   eval      tgtpru = knmus
051000090330     c                   update    fitgt00
051100090330     c                   end
051200090331     c* aggiorno il FIFGT
051300090331     c                   call      'FICN03R'
051400090331     c                   parm                    tgtpdr
051500090331     c                   parm                    tgtsml
051600090331     c                   parm                    tgtprg
051700090331     c                   parm                    d27cmt
051800121002      **
051900090331     c                   else
052000121002      **
052100090331     c     adtnrr        chain     fiadt00f
052200090331     c                   if        %found(fiadt00f)
052300121002     c                   eval      adtdst = data_VIDscad
052400090331     c                   eval      adtduv = oggi
052500090331     c                   update    fiadt00
052600090331     c                   end
052700121002      **
052800090331     c                   end
052900121002      **
053000090330     c                   endsr
053100090331     c**********************************************************************
053200090331     c* reperimento la data maggiore di valorizzazione
053300090331     c**********************************************************************
053400090331     C     srmaxval      BEGSR
053500121002      **
053600090331     c                   if        d27tip = 'A'
053700090331     C/EXEC SQL
053800090428     C+ SELECT max(fttddc) INTO :maxval FROM fiftt00f WHERE FTTPDR =
053900090717     C+ :d27pdr
054000090331     C/END-EXEC
054100090331     c                   else
054200090331     C/EXEC SQL
054300090428     C+ SELECT max(attddc) INTO :maxval FROM fiatt00f WHERE aTTPDR =
054400090717     C+ :d27pdr
054500090331     C/END-EXEC
054600090331     c                   end
054700121002      **
054800090331     C                   ENDSR
054900071113     c*---------------------------------------------------------------*
055000071113     c     *inzsr        begsr
055100071113     c*---------------------------------------------------------------*
055200061012     c*  Operazioni iniziali
055300061012     C     *ENTRY        PLIST
055400061012     C                   PARM                    KPJBA
055500090330     C                   MOVEL     KPJBU         FNLv27ds
055600121001      **
055700090330     c                   clear                   d27err
055800090331     c                   if        d27cmt = '1'
055900090331     c                   eval      flgcmt = *on
056000090331     c                   else
056100090331     c                   eval      flgcmt = *off
056200090331     c                   end
056300121001      **
056400090331     c                   if        d27tip = 'A'
056500090331     c                   open      fitgt00f
056600090331     c                   open      fitgt01l
056700090331     c                   else
056800090331     c                   open      fiadt01l
056900090331     c                   open      fiadt00f
057000090331     c                   end
057100090331     c* reperisce la data max valorizzata, la scadenza non può essere <
057200090331     c* delle valorizzaz.
057300090331     c                   exsr      srmaxval
057400090331     c*
057500071113     C     Krco          KLIST
057600090330     C                   KFLD                    d27soc
057700090330     C                   KFLD                    d27ksc
057800090330     C     Ktgt          KLIST
057900090330     C                   KFLD                    d27pdr
058000090330     C                   KFLD                    tgtsml
058100090330     c                   clear                   tgtsml
058200090330     C     Ktra          KLIST
058300090330     C                   KFLD                    d27pdr
058400090330     C                   KFLD                    tradfi
058500090330     c                   clear                   tradfi
058600090423     C     Ktab          KLIST
058700090423     C                   KFLD                    tblkut
058800090423     C                   KFLD                    tblcod
058900090423     C                   KFLD                    tblkey
059000090330     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
059100090330     C                   TIME                    W0120            14 0
059200090330     C                   MOVE      W0120         WDAT              8 0
059300090330     C*
059400090330     C                   Z-ADD     WDAT          G02DAT
059500090330     C                   MOVEL     *BLANK        G02ERR
059600090330     C                   CALL      'XSRDA8'
059700090330     C                   PARM                    WLBDAT
059800090330     c                   movel     G02INV        oggi              8 0
059900090423     c* reperisce gg min durata allegato
060000090423     C                   movel     '5A'          tblcod
060100090423     C                   MOVEL(P)  'AUT'         tblKEY
060200090423     c                   z-add     1             tblkut
060300090423     c                   z-add     60            giomin
060400090423     C     KTAB          CHAIN     TABEL00F
060500090423     c                   if        %found(tabel00f)
060600090423     C                   movel     tbluni        ds5Aaut
060700090423     c                   if        §5AGGcpi <> 0
060800090423     C                   z-add     §5AGGcpi      giomin            3 0
060900090423     c                   end
061000090423     c                   end
061100090423     c                   move      oggi          dataiso
061200090423     c                   adddur    giomin:*d     dataiso
061300090423     c                   move      dataiso       oggi2m            8 0
061400120723ab   C*     Inizializza
061500120723ab    /FREE
061600120723ab     TibsSof_Init();
061700120723ab    /END-FREE
061800120723     C*
061900061012     c                   endsr
062000120723ab   C**************************************************************************
062100120723ab    * Controlla la Società operativa della Filiale e quella dei trazionisti
062200120723ab   C**************************************************************************
062300120723ab   C     CHK_SOC_OPE   begsr
062400120723ab   **
062500121002ab   ** verificare la società operativa in vigore
062600120723ab    /FREE
062700121002
062800120723EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
062900120723EDPDC                                             : dtVALIDITA
063000120723EDPDC                                             : 'O'
063100120723EDPDC                                             : valDatIniz
063200120723EDPDC                                             : valDatFine
063300120723ab                                                );
063400120723ab
063500121002EDPDC    // Gestire l'errore.  errore sulla data.
063600120723EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
063700120724           *in02 = '1';
063800120723ab     ENDIF;
063900120723ab
064000120723ab    /END-FREE
064100120723ab   C                   ENDSR
064200120723     C**************************************************************************
