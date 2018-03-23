000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060125      * Il pgm consolida tutti i record convalidati dei conteggi in sede
000400060125      * usando l'istruzione sql:
000500060125      *
000600060125      * ?SELECT fiftt00f.*, case when fttfgs > 0 then digits(fttfgs) else
000700060125      * ?substr(digits(fttpdr), 1, 3) end as filiale FROM FIFtt00F WHERE
000800060125      * ?fttfvl = 'C' and fttftr = ' ' ORDER BY filiale, fttpdr, fttddc
000900060125      *
001000060125      *  ordinando per filiale di gestione se c'è altrimenti per
001100060125      *  i primi 3 byte del padroncino
001200060125      *  (case when fttfgs > 0 then digits(fttfgs) else
001300060125      *   substr(digits(fttpdr), 1, 3) end)
001400060125      *
001500060125      *****************************************************************
001600071017     FTABEL00F  IF   E           K DISK
001700020328     ffiftt01l  Uf   e           k disk    commit
001800060125     Ftitas30c  UF   E           K DISK    commit
001900030429     F*---------------------------------------------------------------------------------
002000030429      * Per Controllare che non ci siano le trasmissioni in sede in corso
002100030429      *  durante l'apertura di questo programma si scrive un record su questo file.
002200030430     Ffiwalc1l  UF A E           K DISK    commit
002300060125     **
002400060125     ffiftd01l  if   e           k disk
002500060125     fFIAPD01l  if   e           k disk
002600030429     F*---------------------------------------------------------------------------------
002700000607     D kpjba         e ds
002800020205     D cncr80        E DS
002900020205     D UT§DSE0F      E DS
003000060125     D fiftt         E DS                  extname(fiftt00f)
003100060125     D DtasFlo       E DS
003200071017     D DsTB          E DS
003300060125      **
003400030429     D WLBDAT          DS
003500030429     D  G02DAT                 1      8  0
003600030429     D  G02INV                 9     16  0
003700030429     D  G02ERR                17     17
003800030429     D  G02TGI                18     22  0
003900030429      *
004000060125     d dataoggi        s               d   datfmt(*eur)
004100060125     d data_gg1        s               d   datfmt(*iso)
004200060125      *
004300020328     D WrkSqlCmd       S           1024
004400060125     D sql             s             70    ctdata dim(3)
004500060125      *
004600071017      * Tipi Bolle x individuare le bolle di recupero da escludere
004700071017     D CodTBr          S              2    DIM(99)
004800071017     D kKUT            s                   LIKE(TBLKUT)
004900071017     D kCOD            s                   LIKE(TBLCOD)
005000071017     D kKEY            s                   LIKE(TBLKEY)
005100060125     D savcdf          S                   like(fttCDF)
005200060125     D savsoc          S                   like(fttSOC)
005300060125     D fgsalfa         S              3a
005400060206     D sav_fgsalfa     S              3a
005500060125
005600060125     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
005700060125     ITITAS000      50
005800060125     ITITAS010      51
005900060125     ITITASP00      52
006000060125
006100020205     c**********************************************************************
006200060125     c* Lettura fifTT00F x FGS/PDR/DDC
006300020328     C/EXEC SQL
006400020328     C+ PREPARE S1 FROM :WrkSqlCmd
006500020328     C/END-EXEC
006600020328
006700020328     C/EXEC SQL
006800020328     C+ DECLARE A1 CURSOR FOR S1
006900020328     C/END-EXEC
007000020328
007100020328     C/EXEC SQL
007200020328     C+ OPEN A1
007300020328     C/END-EXEC
007400020328     C                   DOU       SqlCod <> 0
007500020328     C/EXEC SQL
007600060125     C+ FETCH NEXT FROM A1 INTO :fiftt, :FGSalfa
007700020328     C/END-EXEC
007800020328     C                   SELECT
007900060125     c*
008000020328     C                   WHEN      SqlCod = 0
008100060206     c*
008200060206     c* salva ed esegue il COMMIT
008300060206     c                   if        fgsalfa <> sav_fgsalfa
008400060206     c*
008500060206     c*    solo dopo il primo giro comincia ad eseguire il commit
008600060206     c                   if        sav_fgsalfa <> *blank
008700060206     c                   commit
008800060206     c                   end
008900060206     c*
009000060206     c                   move      fgsalfa       sav_fgsalfa
009100060206     c                   end
009200060125     c*
009300020329     c* se codice fornitore = *blanks lo reperisco dall'anagrafico
009400060125     c* se non c'è nell'anagrafico non consolido il conteggio
009500020402     c                   if        fttcdf = *blanks OR fttcdf = '00000000'
009600021203     c     KAPDF         chain     FIAPD01l
009700060125      **
009800021203     c                   if        %found(FIAPD01l) and apdcsf <> *blanks
009900060125     c                   move      *zeros        savcdf
010000060125     c                   move      apdksc        savcdf
010100060125     c                   movel     apdcsf        savsoc
010200060125      * ?
010300020329     c                   else
010400060125      * ? Salta aggiornamento di FTT
010500020329     c                   iter
010600060125      * ?              *------*
010700020329     c                   end
010800020329     c                   end
010900060125      * ?
011000060125      * ?DETTAGLIO  --> aggiorna FTTFTR = 'R'
011100060125     c     kftt          chain     fiftt01l
011200060125     c                   if        %found(fiftt01l)
011300060125      **
011400060125      ** solo se non aveva il fornitore lo deve imnpostare da anagrafica
011500060125     c                   if        fttcdf = *blanks
011600060125     c                   move      savcdf        fttcdf
011700060125     c                   movel     savsoc        fttsoc
011800060125     c                   end
011900060125      **
012000060125     c* se filiale di gestione = 0 metto i primi 3 caratteri del padroncino
012100060125     c                   if        fttfgs = 0
012200060125     c                   movel     fttpdr        fttfgs
012300060125     c                   end
012400060125     c*
012500060125     C                   MOVEL     'R'           fttftr
012600060125     c                   except    aggFTT
012700060125     C*
012800060125      * ?aggiorna TITAS (chi ha fatto Pick/Eti e Carico/Scarico)
012900060125      * ?  pilotato dai dettagli padroncini
013000060125      **  Solo x i records di Testata NON di totale
013100060125     c                   if        fttTSR <> *blank
013200060125     c                   exsr      UPD_TITAS
013300060125     c                   end
013400060328     C*
013500060328      * ?  ad ogni giro di testata aggiorna un gruppo di dati
013600060328     c                   commit
013700060125     C*
013800060125     c                   end
013900060125      **
014000060125      * ? EoF
014100020328     C                   WHEN      SqlCod = 100
014200060125      **
014300060125      * ? Errori
014400020328     C                   WHEN      SqlCod <> 0
014500060125      **
014600020328     C                   ENDSL
014700060125      **
014800020328     C                   ENDDO
014900020328     C/EXEC SQL
015000020328     C+ CLOSE A1
015100020328     C/END-EXEC
015200030430     c*
015300030430     C*  deve cancellare il record di allocazione x impedire le trasmissioni
015400030430     C*   in sede durante l'utilizzo di questo programma.
015500030430     C     Kwalc         setll     fiwalc1l
015600030430     C     Kwalc         reade     fiwalc1l
015700030430     c                   dow       not %eof(fiwalc1l)
015800030430     c                   delete    fiwalc00
015900030430     C     Kwalc         reade     fiwalc1l
016000030430     c                   end
016100060125     c*
016200060125     c* Pulisce records rimasti aperti da 2 gg. prima
016300060125     C     *loval        setll     fiwalc1l
016400060125     C                   read      fiwalc1l
016500060125     c                   dow       not %eof(fiwalc1l)
016600060125     c                   if        waldac < datameno1
016700060125     c                   delete    fiwalc00
016800060125     c                   end
016900060125     C                   read      fiwalc1l
017000060125     c                   end
017100030430     c*
017200020603     c                   commit
017300030429      *
017400000607     c                   seton                                        lr
017500060125     C**---------------------------------------------------------
017600060125     C** Aggiorna TITAS su chi ha fatto Carico/Scarico Pick/Etic
017700060125     C**---------------------------------------------------------
017800060125     C     UPD_TITAS     BEGSR
017900060125      *
018000060125      * imposta di default i flags per aggiornare TITAS
018100060125     C                   clear                   Aut_pick          1
018200060125     C                   clear                   Aut_Etic          1
018300060125     C                   clear                   Aut_Car           1
018400060125     C                   clear                   Aut_Scar          1
018500060125      *
018600060125      * ? sulla Testata c'è  chi ha fatto pick/eti e Car/Scarico
018700060125      * ? in base a questi flags aggiorna TITAS bolla x bolla
018800060125      *
018900060125      * Controlla se l'autotrasportatore ha effettuato personalmente
019000060125      *  l'operazione oppure è stata demandata alla cooperativa.
019100060125     c                   if        fttPEP = 'S' and FTTTSR = 'R'
019200060125     C                   eval      Aut_Etic = 'S'
019300060125     c                   end
019400060125     c                   if        fttPEP = 'S' and FTTTSR = 'C'
019500060125     C                   eval      Aut_pick = 'S'
019600060125     c                   end
019700060125     c                   if        fttLVL = 'S' and FTTTSR = 'R'
019800060125     C                   eval      Aut_Scar = 'S'
019900060125     c                   end
020000060125     c                   if        fttLVL = 'S' and FTTTSR = 'C'
020100060125     C                   eval      Aut_Car = 'S'
020200060125     c                   end
020300060125      **
020400060125      * ? ciclo su Dettaglio x PDR/TSR/NDC/DDC legge tutte le bolle
020500060125      * ?  x aggiornare TITAS
020600060125     C     Kftd          setll     Fiftd01l
020700060125     C     Kftd          reade     Fiftd01l
020800060125     c                   dow       not %EoF(Fiftd01l)
020900060125      * aggancia TITAS30C
021000071109     c******             setoff                                       505152
021100071017     C     KTAS          setll     TITAS30C
021200071017     c*
021300071017     c     Altra_bolla   tag
021400071109     c                   setoff                                       505152
021500071017     C     KTAS          reade     TITAS30C
021600071017     c*
021700071017     C                   IF        not %Eof(TITAS30C)
021800071017     c*
021900071017      *  se si tratta di una bolla di recupero occorre andare a cercare l'altra
022000071017      * bolla per aggiornare TASFLO correttamente (...e NON la BOLLA di RECUPERO).
022100071017     c     tasTBL        lookup    CodTBr                                 21
022200071017      * se è un recupero deve tentare sulla bolla successiva
022300071017     C   21              goto      Altra_bolla
022400071017      *
022500060125     c                   movel     tasflo        dtasflo
022600060125     c                   if        ftttsr = 'R'
022700060125     c                   if        Aut_Etic = *blank
022800060125     c                   movel     'C'           §FLOeti
022900060125     c                   endif
023000060125     c                   if        Aut_Scar = *blank
023100060125     c                   movel     'C'           §FLOsca
023200060125     c                   endif
023300060125     c                   else
023400060125     c                   if        Aut_Pick = *blank
023500060125     c                   movel     'C'           §FLOpic
023600060125     c                   endif
023700060125     c                   if        Aut_Car = *blank
023800060125     c                   movel     'C'           §FLOcar
023900060125     c                   endif
024000060125     c                   end
024100060125     c                   movel     dtasflo       tasflo
024200060125      * aggiorna TITAS
024300060125     C   50              UPDATE    TITAS000
024400060125     C   51              UPDATE    TITAS010
024500060125     C   52              UPDATE    TITASP00
024600060125     c                   EndIF
024700060125      *
024800060125     C     Kftd          reade     Fiftd01l
024900060125     c                   EndDO
025000060125      *
025100060125     C                   ENDSR
025200020205     C**-------------------------------------------------------------**
025300000607     c     *inzsr        begsr
025400060125     C**-------------------------------------------------------------**
025500000607     c     *entry        plist
025600020205     c                   parm                    kpjba
025700030429      *----------*
025800060125      * ?Alloca scrivendo un record nel Fiwalc1L per impedire che ci siano dei
025900060125      * ? conflitti durante il consolidamento in sede (Ex trasmissione a sede)
026000030429     C* GIRO DATA ODIERNA
026100030429     C                   TIME                    W0140            14 0
026200030429     C                   MOVE      W0140         UDATE8            8 0
026300060125     C     *eur          movel     UDATE8        dataoggi
026400060125     C     dataoggi      subdur    1:*d          data_gg1
026500060125     C                   move      data_gg1      datameno1         8 0
026600060125      **
026700030429     C                   Z-ADD     UDATE8        G02DAT
026800030429     C                   MOVE      *ZEROS        G02INV
026900030429     C                   MOVE      *BLANKS       G02ERR
027000030429     C                   CALL      'XSRDA8'
027100030429     C                   PARM                    WLBDAT
027200030429     C                   clear                   Fiwalc00
027300030429     C                   movel     w0140         walOrc
027400030429     C                   movel     g02inv        waldac
027500030429     C                   eval      WalSIF = KNSIF
027600030429     C                   eval      WalPGM = 'FICN43R   '
027700030429     C                   eval      WalNOJ = KNMEB
027800030429     C                   eval      WalNUS = KNMUS
027900030429     C                   eval      WalNRJ = KNRJO
028000060209      ****
028100060209     c                   goto      non_farlo
028200030429     C                   write     Fiwalc00
028300030429     c                   feod      Fiwalc1l
028400030505     c                   commit
028500060209     c     non_farlo     tag
028600030429      *----------*
028700020205     C                   Z-ADD     1             CODUT
028800020205     C                   CALL      'X§PARUT'
028900020205     C                   PARM                    UT§DSE0F
029000020205     C                   MOVEL     REC80         CNCR80
029100060125      *
029200030429     C     Kwalc         KLIST
029300030429     C                   KFLD                    WalPGM
029400030429     C                   KFLD                    WalNOJ
029500030429     C                   KFLD                    WalNUS
029600030429     C                   KFLD                    WalNRJ
029700060125      *
029800020328     c     kftt          klist
029900020328     c                   kfld                    fttpdr
030000020328     c                   kfld                    ftttsr
030100020328     c                   kfld                    fttndc
030200020328     c                   kfld                    fttddc
030300060125      *
030400021203     c     kAPDF         klist
030500021203     c                   kfld                    apdtip
030600021203     c                   kfld                    fttpdr
030700021203     c                   movel     'A'           apdtip
030800060125      **
030900020328     c     kftd          klist
031000020328     c                   kfld                    fttpdr
031100020328     c                   kfld                    ftttsr
031200020328     c                   kfld                    fttndc
031300020328     c                   kfld                    fttddc
031400060125      *
031500060125     C     KTAS          KLIST
031600060125     C                   KFLD                    ftdAAS
031700060125     C                   KFLD                    ftdLNP
031800060125     C                   KFLD                    ftdNRS
031900060125     C                   KFLD                    ftdNSP
032000071017     C* Definisco chiavi di accesso
032100071017     C     KTAB          KLIST
032200071017     C                   KFLD                    KKUT
032300071017     C                   KFLD                    KCOD
032400071017      *
032500071017     C* Caricamento Tabella TB
032600071017     c                   clear                   CodTBr
032700071017     C                   Z-ADD     0             TB                3 0
032800071017     C                   Z-ADD     CODUT         KKUT
032900071017     C                   MOVEL     'TB'          KCOD
033000071017     C     KTAB          setll     TABEL00F
033100071017     C     KTAB          reade     TABEL00F
033200071017     C                   DOW       not %Eof(TABEL00F)
033300071017     C     TBLFLG        IFEQ      *BLANKS
033400071017     C                   MOVEL     TBLUNI        DsTB
033500071017      * carica solo i tipi bolle di recupero
033600071017      *  che non dovranno essere presi in considerazione dalle letture del FTD
033700071017     c                   if        §TBRBL = 'R'
033800071017     C                   ADD       1             TB
033900071017     C                   MOVEL     TBLKEY        CodTBr(TB)
034000071017     c                   end
034100071017     C                   END
034200071017     C     KTAB          reade     TABEL00F
034300071017     C                   END
034400060125     c*
034500020328     c* MI COMPONGO L'ISTRUZIONE SQL
034600020328     c                   EVAL      WRKSQLCMD = sql(1)+sql(2)+sql(3)
034700020402     c*
034800020402     c                   endsr
034900000607     O*------------------------------------------------------*
035000060125      * ?Aggiornamento FIFTT x consolidamento in sede
035100060125     O*------------------------------------------------------*
035200060125     Ofiftt000  e            aggFTT
035300060125     o                       fttfgs
035400060125     o                       fttcdf
035500060125     o                       fttsoc
035600060125     o                       fttftr
035700060125     O*------------------------------------------------------*
035800020328**         SQL
035900060125SELECT fiftt00f.*, case when fttfgs > 0 then digits(fttfgs) else
036000060125substr(digits(fttpdr), 1, 3) end as filiale FROM FIFtt00F WHERE
036100060125fttfvl = 'C' and fttftr = ' ' ORDER BY filiale, fttpdr, fttddc
