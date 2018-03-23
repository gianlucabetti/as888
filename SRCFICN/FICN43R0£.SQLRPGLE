000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060125      * Il pgm consolida tutti i record convalidati dei conteggi in sede
000400060125      * usando l'istruzione sql:
000500060125      *
000501060125      * ?SELECT fiftt00f.*, case when fttfgs > 0 then digits(fttfgs) else
000502060125      * ?substr(digits(fttpdr), 1, 3) end as filiale FROM FIFtt00F WHERE
000503060125      * ?fttfvl = 'C' and fttftr = ' ' ORDER BY filiale, fttpdr, fttddc
000900060125      *
001000060125      *  ordinando per filiale di gestione se c'è altrimenti per
001100060125      *  i primi 3 byte del padroncino
001200060125      *  (case when fttfgs > 0 then digits(fttfgs) else
001300060125      *   substr(digits(fttpdr), 1, 3) end)
001400060125      *
001500060125      *****************************************************************
002500020328     ffiftt01l  Uf   e           k disk    commit
002501060125     Ftitas30c  UF   E           K DISK    commit
003000030429     F*---------------------------------------------------------------------------------
003100030429      * Per Controllare che non ci siano le trasmissioni in sede in corso
003200030429      *  durante l'apertura di questo programma si scrive un record su questo file.
003300030430     Ffiwalc1l  UF A E           K DISK    commit
003301060125     **
003302060125     ffiftd01l  if   e           k disk
003303060125     fFIAPD01l  if   e           k disk
003400030429     F*---------------------------------------------------------------------------------
003500000607     D kpjba         e ds
004500020205     D cncr80        E DS
004600020205     D UT§DSE0F      E DS
004601060125     D fiftt         E DS                  extname(fiftt00f)
004602060125     D DtasFlo       E DS
004603060125      **
005200030429     D WLBDAT          DS
005300030429     D  G02DAT                 1      8  0
005400030429     D  G02INV                 9     16  0
005500030429     D  G02ERR                17     17
005600030429     D  G02TGI                18     22  0
005700030429      *
006103060125     d dataoggi        s               d   datfmt(*eur)
006104060125     d data_gg1        s               d   datfmt(*iso)
006105060125      *
006800020328     D WrkSqlCmd       S           1024
007200060125     D sql             s             70    ctdata dim(3)
007201060125      *
007202060125     D savcdf          S                   like(fttCDF)
007203060125     D savsoc          S                   like(fttSOC)
007204060125     D fgsalfa         S              3a
007205060206     D sav_fgsalfa     S              3a
007206060125
007207060125     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
007208060125     ITITAS000      50
007209060125     ITITAS010      51
007210060125     ITITASP00      52
007211060125
007800020205     c**********************************************************************
007900060125     c* Lettura fifTT00F x FGS/PDR/DDC
008000020328     C/EXEC SQL
008100020328     C+ PREPARE S1 FROM :WrkSqlCmd
008200020328     C/END-EXEC
008300020328
008400020328     C/EXEC SQL
008500020328     C+ DECLARE A1 CURSOR FOR S1
008600020328     C/END-EXEC
008700020328
008800020328     C/EXEC SQL
008900020328     C+ OPEN A1
009000020328     C/END-EXEC
009200020328     C                   DOU       SqlCod <> 0
009300020328     C/EXEC SQL
009400060125     C+ FETCH NEXT FROM A1 INTO :fiftt, :FGSalfa
009500020328     C/END-EXEC
009700020328     C                   SELECT
009701060125     c*
009800020328     C                   WHEN      SqlCod = 0
009801060206     c*
009802060206     c* salva ed esegue il COMMIT
009803060206     c                   if        fgsalfa <> sav_fgsalfa
009804060206     c*
009805060206     c*    solo dopo il primo giro comincia ad eseguire il commit
009806060206     c                   if        sav_fgsalfa <> *blank
009807060206     c                   commit
009808060206     c                   end
009809060206     c*
009810060206     c                   move      fgsalfa       sav_fgsalfa
009811060206     c                   end
010201060125     c*
010300020329     c* se codice fornitore = *blanks lo reperisco dall'anagrafico
010400060125     c* se non c'è nell'anagrafico non consolido il conteggio
010500020402     c                   if        fttcdf = *blanks OR fttcdf = '00000000'
010600021203     c     KAPDF         chain     FIAPD01l
010601060125      **
010700021203     c                   if        %found(FIAPD01l) and apdcsf <> *blanks
010800060125     c                   move      *zeros        savcdf
010900060125     c                   move      apdksc        savcdf
011000060125     c                   movel     apdcsf        savsoc
011001060125      * ?
011100020329     c                   else
011102060125      * ? Salta aggiornamento di FTT
011200020329     c                   iter
011201060125      * ?              *------*
011300020329     c                   end
011400020329     c                   end
011401060125      * ?
012501060125      * ?DETTAGLIO  --> aggiorna FTTFTR = 'R'
012503060125     c     kftt          chain     fiftt01l
012504060125     c                   if        %found(fiftt01l)
012505060125      **
012506060125      ** solo se non aveva il fornitore lo deve imnpostare da anagrafica
012507060125     c                   if        fttcdf = *blanks
012508060125     c                   move      savcdf        fttcdf
012509060125     c                   movel     savsoc        fttsoc
012510060125     c                   end
012511060125      **
012512060125     c* se filiale di gestione = 0 metto i primi 3 caratteri del padroncino
012513060125     c                   if        fttfgs = 0
012514060125     c                   movel     fttpdr        fttfgs
012515060125     c                   end
012516060125     c*
012517060125     C                   MOVEL     'R'           fttftr
012518060125     c                   except    aggFTT
012519060125     C*
012520060125      * ?aggiorna TITAS (chi ha fatto Pick/Eti e Carico/Scarico)
012521060125      * ?  pilotato dai dettagli padroncini
012522060125      **  Solo x i records di Testata NON di totale
012523060125     c                   if        fttTSR <> *blank
012524060125     c                   exsr      UPD_TITAS
012525060125     c                   end
012526060125     C*
012527060125     c                   end
012601060125      **
012602060125      * ? EoF
012700020328     C                   WHEN      SqlCod = 100
012701060125      **
012702060125      * ? Errori
012800020328     C                   WHEN      SqlCod <> 0
012801060125      **
012900020328     C                   ENDSL
012901060125      **
013000020328     C                   ENDDO
013200020328     C/EXEC SQL
013300020328     C+ CLOSE A1
013400020328     C/END-EXEC
013900030430     c*
014000030430     C*  deve cancellare il record di allocazione x impedire le trasmissioni
014100030430     C*   in sede durante l'utilizzo di questo programma.
014200030430     C     Kwalc         setll     fiwalc1l
014300030430     C     Kwalc         reade     fiwalc1l
014400030430     c                   dow       not %eof(fiwalc1l)
014500030430     c                   delete    fiwalc00
014600030430     C     Kwalc         reade     fiwalc1l
014700030430     c                   end
014701060125     c*
014702060125     c* Pulisce records rimasti aperti da 2 gg. prima
014703060125     C     *loval        setll     fiwalc1l
014704060125     C                   read      fiwalc1l
014705060125     c                   dow       not %eof(fiwalc1l)
014707060125     c                   if        waldac < datameno1
014708060125     c                   delete    fiwalc00
014709060125     c                   end
014710060125     C                   read      fiwalc1l
014711060125     c                   end
014800030430     c*
014900020603     c                   commit
015000030429      *
015100000607     c                   seton                                        lr
015101060125     C**---------------------------------------------------------
015102060125     C** Aggiorna TITAS su chi ha fatto Carico/Scarico Pick/Etic
015103060125     C**---------------------------------------------------------
015104060125     C     UPD_TITAS     BEGSR
015105060125      *
015106060125      * imposta di default i flags per aggiornare TITAS
015107060125     C                   clear                   Aut_pick          1
015108060125     C                   clear                   Aut_Etic          1
015109060125     C                   clear                   Aut_Car           1
015110060125     C                   clear                   Aut_Scar          1
015111060125      *
015112060125      * ? sulla Testata c'è  chi ha fatto pick/eti e Car/Scarico
015113060125      * ? in base a questi flags aggiorna TITAS bolla x bolla
015117060125      *
015118060125      * Controlla se l'autotrasportatore ha effettuato personalmente
015119060125      *  l'operazione oppure è stata demandata alla cooperativa.
015120060125     c                   if        fttPEP = 'S' and FTTTSR = 'R'
015121060125     C                   eval      Aut_Etic = 'S'
015122060125     c                   end
015123060125     c                   if        fttPEP = 'S' and FTTTSR = 'C'
015124060125     C                   eval      Aut_pick = 'S'
015125060125     c                   end
015126060125     c                   if        fttLVL = 'S' and FTTTSR = 'R'
015127060125     C                   eval      Aut_Scar = 'S'
015128060125     c                   end
015129060125     c                   if        fttLVL = 'S' and FTTTSR = 'C'
015130060125     C                   eval      Aut_Car = 'S'
015131060125     c                   end
015135060125      **
015136060125      * ? ciclo su Dettaglio x PDR/TSR/NDC/DDC legge tutte le bolle
015137060125      * ?  x aggiornare TITAS
015138060125     C     Kftd          setll     Fiftd01l
015139060125     C     Kftd          reade     Fiftd01l
015140060125     c                   dow       not %EoF(Fiftd01l)
015141060125      * aggancia TITAS30C
015142060125     c                   setoff                                       505152
015143060125     C     KTAS          CHAIN     TITAS30C
015144060125     C                   IF        %Found(TITAS30C)
015145060125     c                   movel     tasflo        dtasflo
015146060125     c                   if        ftttsr = 'R'
015147060125     c                   if        Aut_Etic = *blank
015148060125     c                   movel     'C'           §FLOeti
015149060125     c                   endif
015150060125     c                   if        Aut_Scar = *blank
015151060125     c                   movel     'C'           §FLOsca
015152060125     c                   endif
015153060125     c                   else
015154060125     c                   if        Aut_Pick = *blank
015155060125     c                   movel     'C'           §FLOpic
015156060125     c                   endif
015157060125     c                   if        Aut_Car = *blank
015158060125     c                   movel     'C'           §FLOcar
015159060125     c                   endif
015160060125     c                   end
015161060125     c                   movel     dtasflo       tasflo
015162060125      * aggiorna TITAS
015163060125     C   50              UPDATE    TITAS000
015164060125     C   51              UPDATE    TITAS010
015165060125     C   52              UPDATE    TITASP00
015166060125     c                   EndIF
015167060125      *
015168060125     C     Kftd          reade     Fiftd01l
015169060125     c                   EndDO
015170060125      *
015171060125     C                   ENDSR
031500020205     C**-------------------------------------------------------------**
034900000607     c     *inzsr        begsr
034901060125     C**-------------------------------------------------------------**
035200000607     c     *entry        plist
035300020205     c                   parm                    kpjba
035400030429      *----------*
035401060125      * ?Alloca scrivendo un record nel Fiwalc1L per impedire che ci siano dei
035402060125      * ? conflitti durante il consolidamento in sede (Ex trasmissione a sede)
035700030429     C* GIRO DATA ODIERNA
035800030429     C                   TIME                    W0140            14 0
035900030429     C                   MOVE      W0140         UDATE8            8 0
035901060125     C     *eur          movel     UDATE8        dataoggi
035902060125     C     dataoggi      subdur    1:*d          data_gg1
035903060125     C                   move      data_gg1      datameno1         8 0
035906060125      **
036000030429     C                   Z-ADD     UDATE8        G02DAT
036100030429     C                   MOVE      *ZEROS        G02INV
036200030429     C                   MOVE      *BLANKS       G02ERR
036300030429     C                   CALL      'XSRDA8'
036400030429     C                   PARM                    WLBDAT
036500030429     C                   clear                   Fiwalc00
036600030429     C                   movel     w0140         walOrc
036700030429     C                   movel     g02inv        waldac
036800030429     C                   eval      WalSIF = KNSIF
036900030429     C                   eval      WalPGM = 'FICN43R   '
037000030429     C                   eval      WalNOJ = KNMEB
037100030429     C                   eval      WalNUS = KNMUS
037200030429     C                   eval      WalNRJ = KNRJO
037300030429     C                   write     Fiwalc00
037400030429     c                   feod      Fiwalc1l
037500030505     c                   commit
037600030429      *----------*
037700020205     C                   Z-ADD     1             CODUT
037800020205     C                   CALL      'X§PARUT'
037900020205     C                   PARM                    UT§DSE0F
038000020205     C                   MOVEL     REC80         CNCR80
038100060125      *
038200030429     C     Kwalc         KLIST
038300030429     C                   KFLD                    WalPGM
038400030429     C                   KFLD                    WalNOJ
038500030429     C                   KFLD                    WalNUS
038600030429     C                   KFLD                    WalNRJ
038700060125      *
038800020328     c     kftt          klist
038900020328     c                   kfld                    fttpdr
039000020328     c                   kfld                    ftttsr
039100020328     c                   kfld                    fttndc
039200020328     c                   kfld                    fttddc
039201060125      *
039300021203     c     kAPDF         klist
039400021203     c                   kfld                    apdtip
039500021203     c                   kfld                    fttpdr
039600021203     c                   movel     'A'           apdtip
039601060125      **
040600020328     c     kftd          klist
040700020328     c                   kfld                    fttpdr
040800020328     c                   kfld                    ftttsr
040900020328     c                   kfld                    fttndc
041000020328     c                   kfld                    fttddc
041001060125      *
041002060125     C     KTAS          KLIST
041003060125     C                   KFLD                    ftdAAS
041004060125     C                   KFLD                    ftdLNP
041005060125     C                   KFLD                    ftdNRS
041006060125     C                   KFLD                    ftdNSP
041007060125     c*
041100020328     c* MI COMPONGO L'ISTRUZIONE SQL
041200020328     c                   EVAL      WRKSQLCMD = sql(1)+sql(2)+sql(3)
041300020402     c*
041400020402     c                   endsr
043600000607     O*------------------------------------------------------*
043601060125      * ?Aggiornamento FIFTT x consolidamento in sede
043602060125     O*------------------------------------------------------*
043700060125     Ofiftt000  e            aggFTT
043800060125     o                       fttfgs
043801060125     o                       fttcdf
043802060125     o                       fttsoc
043803060125     o                       fttftr
043804060125     O*------------------------------------------------------*
044600020328**         SQL
045000060125SELECT fiftt00f.*, case when fttfgs > 0 then digits(fttfgs) else
045100060125substr(digits(fttpdr), 1, 3) end as filiale FROM FIFtt00F WHERE
045200060125fttfvl = 'C' and fttftr = ' ' ORDER BY filiale, fttpdr, fttddc
