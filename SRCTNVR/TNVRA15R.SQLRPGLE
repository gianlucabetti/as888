000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090109     H* TNVRA15R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800090109     Ffifgt00f  uF a E             DISK    rename(FIFGT000:FGTFisico)
000900090424     F********************************     usropn
001000090109     Ffitgt00f  o  a E           K DISK
001100090109      *
001200090423     Ffifpt01L  uF a E           K DISK
001300090423     Ffifpd01L  uF a E           K DISK
001400081211      *
001500090401     Ffiapd01L  iF   E           K DISK
001600090428     Ffiftt02L  iF   E           K DISK
001700090401      *
001800090424     FwxFGT00S  o  a E             DISK
001900081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000081211      * -------------------------------------------------------------
002100090421     d dapdflr       e ds
002200090421     dficn08ds       e ds
002300081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002400081110     d                                     prefix(STP:3)
002500081110     D esito           s             10i 0
002600081110     D FGT_REC         s              9s 0
002700090424     D STP_DATSCAD     s              8s 0
002800081230      *
002900090109     D KPJBA         E DS
003000090109      *
003100090109     D ficn05ds      E DS
003200090109     D dGPI          E DS
003300900521     C*****************************************************************
003400090112     C****+ dec( year(curdate())*10000 + month(curdate())*100 +
003500090112     C****+ day(curdate()) , 8, 0)
003600090112     C****+ between fgtddt and fgtdst
003700090112     C*****************************************************************
003800090422     C*
003900090423     c                   clear                   savSTPSMLpdr      8
004000081209     C                   SETON                                        LR
004100090422     C*
004200090422     C*=================================================================
004300020418     C*
004400090422     C*=================================================================
004500090422      **
004600090423      **  Ricordiamo che:
004700090423      **    PRIMA sono state fatte scadere al 30/04/2009 tutte le altre
004800090423      *      che non devono essere CONVERTITE.
004900090422      **
005000090423      **  Si divide in 2 il problema:
005100090423      **    Gli autisti che hanno tariffe relative ad un solo Periodo da ricondurre
005200090423      **    ad un unico progressivo 200 decorrenza 1/5 scadenza 31/12/2039
005300090423      **    e
005400090423      **    Gli autisti che invece hanno + tariffe in periodi diversi occorre generare
005500090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
005600090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
005700090423      **    decorrenza del progressivo 201 e così via......
005800090423      **
005900090423      **  ATTENZIONE: l'unica discriminante e se la tariffa era stata STAMPATA
006000090423      **   Se era stata STAMPATA sul nuovo progressivo le tariffe
006100090423      **   devono risultare
006200090423      **   CONFERMATE
006300090423      **
006400090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
006500090423      **   devono risultare
006600090423      **   NON CONFERMATE
006700090423      **
006800090423     C*=================================================================
006900090423      **  Queste sono le tariffe da CONVERTIRE creando la SUPERTESTATA
007000090423     C*=================================================================
007100090423      ** 1° PROBLEMA:
007200090423      **    DEVE leggere tutte le tariffe AUTISTI (C,R,G) con data decorrenza
007300090422      **  minore del 2010 e con Data SCADENZA superiore al 1°maggio 2009
007400090423      **  ma riconducibili ad un solo periodo ossia una sola supertariffa
007500090423      **  con progressivo 200 data decorrenza 1/5/2009 scadenza 31/12/2039.
007600090423      **  e le attuali tariffe avranno data scadenza 30/4/2009.
007700090423      *
007800090424      *  esempio:
007900090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA
008000090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.
008100090424      *                         PADR                          TAR.         TAR.
008200090424      *
008300090424      *     10001                 C    300    4    EUR    2008/08/01  2039/12/31
008400090424      *     10001                 G    999    6    EUR    2008/08/01  2039/12/31
008500090424      *     10001                 R    300    3    EUR    2008/08/01  2039/12/31
008600090424      *
008700090424      *     10006                 C    300    1    EUR    2008/08/01  2039/12/31
008800090424      *     10006                 G    999    1    EUR    2008/08/01  2039/12/31
008900090424      *     10006                 R    300    1    EUR    2008/08/01  2039/12/31
009000090424      *                                                       =            =
009100090424      *                                                  sono uguali  sono uguali
009200090424      *
009300090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009400090423     C+   SELECT fifgt00f.* , rrn(fifgt00f) FROM
009500090423     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
009600090423     C+   and fgtddt < 20100101 and fgtdst >= 20090501
009700090423     C+   and fgtsml concat digits(fgtpdr) in
009800090423     C+   (
009900090423     C+   select fgtsml concat digits(fgtpdr) from
010000090423     C+   (
010100090423     C+   select FGTSML, FGTPDR, count(*) from
010200090423     C+   (
010300090423     C+   SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
010400090423     C+   fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
010500090423     C+   and fgtddt < 20100101 and fgtdst >= 20090501
010600090423     C+   group BY FGTSML, FGTPDR, FGTDDT, FGTDST
010700090423     C+   ) as b
010800090423     C+   group BY FGTSML, FGTPDR
010900090423     C+   having count(*) = 1
011000090423     C+   ) as unPERIODO
011100090423     C+   )
011200090423     C+   ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
011300090422     C/END-EXEC
011400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
011500081110     c                   if        sqlCODE < 0
011600081110     c                   eval      esito = Sqlcode
011700081110     c                   dump(a)
011800081110     c                   return
011900081110     c                   end
012000081110     C/EXEC SQL
012100081110     C+ OPEN A1
012200081110     C/END-EXEC
012300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
012400081110     c                   if        sqlCODE < 0
012500081110     c                   eval      esito = Sqlcode
012600081110     c                   dump(a)
012700081110     c                   return
012800081110     c                   else
012900081110     c                   except    testa
013000081110     c                   end
013100090109      * >>>>>>>
013200081209      * resto nel ciclo finchè....
013300081110     C                   DOU       SqlCod < 0
013400081110      * lettura cursore
013500081110     C/EXEC SQL
013600090423     C+ Fetch Next From A1 Into :dsFGT, :FGT_REC
013700081110     C/END-EXEC
013800081110      *  EoF
013900081110     C                   if        SqlCod = 100
014000081110     c                   Leave
014100081110      *  x errori
014200081110     C                   ElseIf    SqlCod < 0
014300081110     c                   eval      esito = Sqlcode
014400081110     c                   dump(a)
014500081110     c                   leave
014600081110     C                   ENDif
014700090428      *
014800090428      *----------------------------->>>>>
014900090428      * Controlli su Angrafica e su Valorizzazioni
015000090428      *
015100090428     c                   clear                   errore           10
015200090428      *
015300090428      * rileva dall'anagrafica
015400090428     c                   eval      apdTIP = 'A'
015500090428     c                   eval      apdPDR = stpPDR
015600090428     c     Key_APD       chain     fiapd01l
015700090428     c                   if        %Found(fiapd01l)
015800090428      *
015900090428     c                   if        apdATB <> *blank
016000090428     c                   movel     'ANNULLATO'   errore
016100090428     c                   else
016200090428      *
016300090428      *-  Esluso da autofatturazione
016400090428     c                   if        apdPDD = 'S'
016500090428     c                   movel     'AUTOFAT'     errore
016600090428     c                   end
016700090428      *
016800090428     c                   endIF
016900090428      *
017000090428     c                   else
017100090428     c                   movel     'NO_APD'      errore
017200090428     c                   end
017300090428      *
017400090428      *  Controlla sulle valorizzazioni che non ce ne siano oppure
017500090428      *   siano precedenti al 20090101
017600090429      *  SEMPRE CHE non siano in decorrenza fra il 1/4 ed il 31/12
017700090430      *
017800090430     c                   z-add     0             datamaxima        8 0
017900090430     C/EXEC SQL
018000090430     C+ SELECT max(fgtddt) INTO :datamaxima FROM edpab/svFgt00F WHERE
018100090430     C+ fgtpdr = :STPpdr and fgtsml= :STPsml and FGTTSR in ('C', 'R', 'G')
018200090430     C/END-EXEC
018300090430     c                   if        datamaxima < 20090401
018400090429      *
018500090428     c                   if        errore = *blank
018600090428     c     stppdr        chain     fiftt02l
018700090428     c                   if        not %Found(fiftt02l)
018800090428     c                   movel     'NO_in_FTT'   errore
018900090428     c                   end
019000090428      *
019100090428     c                   if        errore = *blank
019200090428      *
019300090428     c                   eval      fttpdr = stpPDR
019400090428     c                   eval      fttfvl = 'C'
019500090428      *
019600090428     c     Key_FTT       setgt     fiftt02l
019700090428     c     Key_FTT       readpe    fiftt02l
019800090428      * Se inferiore al 2009
019900090428     c                   if        not %Eof(fiftt02l)
020000090428      *
020100090429     c                   if        fttddc < 20090101
020200090428     c                   movel     'FTT_<_2009'  errore
020300090428     c                   end
020400090428      *  SE EoF
020500090428     c                   else
020600090428     c                   movel     'FTT_no_C'    errore
020700090428     c                   end
020800090428     c
020900090428     c                   end
021000090428     c                   end
021100090429     c                   end
021200090428      *----------------------------->>>>>
021300090429      * NONOSTANTE l'errore se la Decorrenza è > del 30 Aprile
021400090429      *   devo tradurli
021500090429     c                   if         stpDDT > 20090430 and errore <> *blank
021600090429     c                   move      ' /EXE'       errore
021700090429     c                   end
021800090429      *
021900090427      *  se sono qui ho letto un record correttamente
022000090427     c   Of              except    testa
022100090427     c                   except    detta
022200090427      *
022300090428      *
022400090428      *----------------------------->>>>>
022500090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
022600090428      *
022700090428     c                   if        Errore  = *blank
022800090429     c                              or  stpDDT > 20090430
022900090428      *----------------------------->>>>>
023000090428      *
023100090109     c                   clear                   eseguito          1
023200090109     c                   clear                   verifica          1
023300081110      *
023400081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
023500090423     c                   movel     stpSML        stpSMLPDR         8
023600090109     c                   move      stpPDR        Autista           7 0
023700090423     c                   move      Autista       stpSMLPDR
023800090109      *
023900090109      *  Solo in aggiornamento
024000081110     c                   if        Aggiorna = 'S'
024100081110      *
024200090109      *  Deve scrivere la testata
024300090109      *    a rottura di simulazione/autista
024400090423      *     salva chiave di rottura
024500090423     c                   if        STPsmlPDR <> savSTPsmlPDR
024600090423     c                   eval      savSTPsmlPDR = STPsmlPDR
024700090423      *
024800090423      *  IMPOSTA la SUERTESTATA
024900090109     c                   clear                   fitgt000
025000090423     c                   z-add     STPpdr        tgtPDR
025100090423     c                   move      STPsml        tgtSML
025200090423     c                   z-add     200           tgtPRG
025300090423     c                   move      STPdiv        tgtDIV
025400090423      * data decorrenza/scadenza
025500090423     c                   z-add     20090501      tgtDDT
025600090423      *
025700090423     c                   if        stpDST > 20091231
025800090423     c                   z-add     20391231      tgtDST
025900090423     c                   else
026000090423     c                   z-add     stpDST        tgtDST
026100090423     c                   end
026200090423      *
026300090423     c                   z-add     *date         tgtDUV
026400090423     c                   moveL     'Convers'     tgtPRU
026500090401      *
026600090423      * rileva dall'anagrafica
026700090401     c                   eval      apdTIP = 'A'
026800090401     c                   eval      apdPDR = tgtPDR
026900090401     c     Key_APD       chain     fiapd01l
027000090423      *---
027100090401     c                   if        %Found(fiapd01l)
027200090423      *   società fornitore
027300090421     c                   movel     apdcsf        tgtsoc
027400090421     c                   movel     *all'0'       tgtcdf
027500090421     c                   move      apdksc        tgtcdf
027600090421     c                   movel     apdflr        dapdflr
027700090423      *  la massa in KG
027800090421     c                   eval      tgtpkl = §APDMCP
027900090423     c*  i tempi di carico e scarico
028000090421     c                   clear                   ficn08ds
028100090421     c                   eval      i08data= tgtddt
028200090421     c                   eval      i08tip= 'A'
028300090421     c                   eval      i08mcp = tgtpkl
028400090421     c                   call      'FICN08R'
028500090421     c                   parm                    ficn08ds
028600090421     c                   if        o08err =  ' '
028700090421     c                   eval      TGTTMPC = o08tc
028800090421     c                   eval      TGTTMPS = o08ts
028900090421     c                   end
029000090423      *---
029100090401     c                   end
029200090423      *
029300090423      *  prezzo carburante al litro
029400090109     c                   z-add     D§GPIGPI      tgtPCAR
029500090422      * Attenzione:
029600090423      *     se non sono state stampate nel vecchio
029700090423      *        non devono essere convalidate nel nuovo
029800090422     c                   if        stpDTS > 0
029900090424     c                   z-add     20090501      tgtDTS
030000090422     c                   end
030100090422      *
030200090109     c                   write     fitgt000
030300090109     c                   end
030400090423      *------------------
030500081216      *
030600090423      *  genera o aggiorna
030700090423      *     i dettagli con il nuovo progressivo
030800090423     c                   exsr      FPT_FPD
030900090109      *
031000090109      *------------------
031100081218      *   deve eliminare i puntamenti del codice di riferimento
031200090424     c********           open      fifgt00f
031300090423      *
031400081218     c     fgt_REC       chain     FIFGT00F
031500081218     c                   if        %Found(FIFGT00F)
031600090112      *
031700090422      *  Fa scadere la testata al 30 apr.2009
031800090423     c                   if            fgtddt < 20090501
031900090422     c                   eval      fgtDST = 20090430
032000090423     c                   end
032100090112      *
032200090422      *     Ma se decorreva già dal 1/5/2009
032300090422      *   non può farlo scadere il 30/4/2009
032400090112      * e quindi questa diventa la tariffa della supertestata
032500090423     c                   if            fgtddt = 20090501
032600090423      * data scadenza
032700090423     c                   if           fgtDST > 20091231
032800090423     c                   z-add     20391231      fgtDST
032900090423     c                   endIf
033000090423      *
033100090112     c                   z-add     200           fgtPRG
033200090112     c                   z-add     *date         fgtDUV
033300090112     c                   clear                   fgtDCN
033400090423      *
033500090423      * solo se era vuoto anche prima
033600090423     c                   if            stpDTS > 0
033700090424     c                   z-add     20090501      fgtDTS
033800090423     c                   elseif        stpDTS = 0
033900090112     c                   clear                   fgtDTS
034000090423     c                   end
034100090112     c                   clear                   fgtFTR
034200090112     c                   clear                   fgtDTR
034300090423      *
034400090423     c                   end
034500090423      *
034600090423      * aggiorna
034700081218     c                   update    fgtFisico
034800090109      *
034900090423      *  Quindi scrive un'altra testata SOLO SE decorrenza < 1/5/2009
035000090423     c                   IF           fgtDDT < 20090501
035100090420     c                   eval      fgtDDT = 20090501
035200090423      *
035300090423     c                   if        stpDST > 20091231
035400090423     c                   z-add     20391231      fgtDST
035500090423     c                   else
035600090423     c                   z-add     stpDST        fgtDST
035700090423     c                   end
035800090423      *
035900090109     c                   z-add     200           fgtPRG
036000090109     c                   z-add     *date         fgtDUV
036100090109     c                   clear                   fgtDCN
036200090424     c*****************  z-add     tgtDTS        fgtDTS
036300090109     c                   clear                   fgtFTR
036400090109     c                   clear                   fgtDTR
036500090109     c                   write     fgtFisico
036600090423     c                   endIF
036700090112      *
036800090423     c                   endIF
036900090112      *
037000090424     c*******            close     fifgt00f
037100090109      *
037200090109     c                   endif
037300090109      *------------------
037400090428      *----------------------------->>>>>
037500090428     c                   end
037600090428      *----------------------------->>>>>
037700090424      *
037800090424      *  scrive il dettaglio su un file
037900090424     c                   exsr      wri_DETTA
038000081110      *
038100081110     C                   ENDDO
038200081110     C/EXEC SQL
038300081110     C+ Close A1
038400081110     C/END-EXEC
038500081110     C                   if        Esito < 0
038600081110     c                   return
038700081110     c                   endIF
038800081110     C*
038900081110     C     FINE          TAG
039000081110     c                   except    finest
039100081110     c                   return
039200081209     C*------------------------------------------------------------------------*
039300081209     C* *INZSR - OPERAZIONI INIZIALI
039400081209     C*------------------------------------------------------------------------*
039500081209     C     *INZSR        BEGSR
039600081209      *
039700081209     C     *ENTRY        PLIST
039800081209     C                   PARM                    AGGIORNA          1
039900081209     C*
040000090109      * duplica le tariffe FGT
040100081209     C     fgt01K        kLIST
040200081211     C                   kfld                    stpPDR
040300081211     C                   kfld                    stpSML
040400081211     C                   kfld                    stpTSR
040500081211     C                   kfld                    stpCTR
040600081211     C                   kfld                    stpPRG
040700090109      *
040800090109      * duplica le tariffe FPT e FPD
040900090109     c     Key_FPT       KLIST
041000090109     c                   kfld                    stpPDR
041100090109     c                   kfld                    stpSML
041200090109     c                   kfld                    stpTSR
041300090109     c                   kfld                    stpCTR
041400090109     c                   kfld                    stpPRG
041500081209     C*
041600090401     c     Key_APD       KLIST
041700090401     c                   kfld                    apdTIP
041800090401     c                   kfld                    apdPDR
041900090401     C*
042000090428     c     Key_FTT       KLIST
042100090428     c                   kfld                    FTTPDR
042200090428     c                   kfld                    FTTFVL
042300090428     C*
042400081209     c                   clear                   esito
042500081209      *
042600081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
042700081209     C                   clear                   UDATE_8           8 0
042800081209     C                   MOVEL     *DATE         UDATE_8
042900090109      *
043000090109      * reperisce il prezzo di riferimento del carburante
043100090109     c                   call      'FICN05R'
043200090109     c                   parm                    ficn05DS
043300090109      *
043400090109     c                   iF        o05err = *blank
043500090109     c                   eval      dGPI = o05GPI
043600090109     c                   enD
043700090109      *
043800081209     c                   endSR
043900090109      *---------------------------------------------------------------*
044000090109      *  Scrive Testate e Dettaglio
044100090109      *---------------------------------------------------------------*
044200090423     c     FPT_FPD       begSR
044300090109     C*
044400090109     c     Key_FPT       setll     fifpt01L
044500090109     c     Key_FPT       reade     fifpt01L
044600090109     c                   Dow       not %EoF(fifpt01l)
044700090423     C*
044800090109     c                   z-add     200           fptPRG
044900090423     C*
045000090423      * se decorre dal 1/5 allora deve aggiornare
045100090423     c                   if        stpDDT = 20090501
045200090423     c                   update    fifpt000
045300090423     c                   else
045400090109     c                   write     fifpt000
045500090423     c                   end
045600090423      *
045700090109     c     Key_FPT       reade     fifpt01L
045800090109     c                   endDo
045900090109     C*
046000090109     c     Key_FPT       setll     fifpd01L
046100090109     c     Key_FPT       reade     fifpd01L
046200090109     c                   Dow       not %EoF(fifpd01l)
046300090423      *
046400090109     c                   z-add     200           fpdPRG
046500090423      *
046600090423      * se decorre dal 1/5 allora deve aggiornare
046700090423     c                   if        stpDDT = 20090501
046800090423     c                   update    fifpd000
046900090423     c                   else
047000090423     c                   write     fifpd000
047100090423     c                   end
047200090423      *
047300090109     c     Key_FPT       reade     fifpd01L
047400090109     c                   endDo
047500090109     C*
047600090109     c                   endSR
047700090424     C*---------------------------------------------------------------*
047800090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
047900090424      *---------------------------------------------------------------*
048000090424     c     WRI_DETTA     begSR
048100090424     C*
048200090424     c                   clear                   WSFGT000
048300090424     C*
048400090424     c                   eval      WSFSML  =  stpSML
048500090424     c                   eval      WSFPDR  =  stpPDR
048600090424     c                   eval      WSFTSR  =  stpTSR
048700090424     c                   eval      WSFCTR  =  stpCTR
048800090424     c                   eval      WSFPRG  = stpPRG
048900090424     c                   eval      WSFDDT  = stpDDT
049000090424     c                   eval      WSFDST  = stpDST
049100090424     c                   eval      WSFDTS  = stpDTS
049200090424     c                   eval      WSFDCN  = stpDCN
049300090424     c                   eval      WSFREC  = FGT_REC
049400090424     c                   eval      WSFSCAD = 0
049500090424     c                   eval      WSFDECOR= 0
049600090424     c                   eval      WSFPROGR= 'TNVRA15R'
049700090428     c**
049800090428     c                   eval      WSFNOTE = Errore
049900090424     c**
050000090424     c                   write     WSFGT000
050100090424     C*
050200090424     c                   endSR
050300081211     C*---------------------------------------------------------------*
050400081110     OQSYSPRT   E            TESTA            02
050500081110     O                                           16 'Simul'
050600081110     O                                           25 'Autista'
050700081110     O                                           30 'TSR'
050800081110     O                                           38 'Tariffa'
050900081110     O                                           45 'Progr'
051000081110     O                                           55 'Data_Dec'
051100081110     O                                           65 'Data_Sca'
051200081110     O                                           75 'Data_Stp'
051300081110     O                                           85 'Data_Cnv'
051400081211     O                                          115 'N_Rec_FGT'
051500081110     O          E            DETta       1
051600081110     O                       stpSML              14
051700081110     O                       stpPDR        z     25
051800081110     O                       stpTSR              29
051900081110     O                       stpCTR              36
052000081110     O                       stpPRG        z     43
052100081110     O                       stpDDT        z     55
052200081110     O                       stpDST        z     65
052300081110     O                       stpDTS        z     75
052400081110     O                       stpDCN        z     85
052500081211     o                       FGT_REC       z    115
052600090428     O                       errore             130
052700081110      ****
052800081110     o          E            FINEST      2
052900090109     o                                           30 '*** TNVRA15R ***'
053000090102     o                                           59 '***   FINE  STAMPA  ***'
053100090109     o                                          + 9 '***  SUPER TARIFFA  ***'
