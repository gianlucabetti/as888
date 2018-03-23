000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090423     H* TNVRA16R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800090109     Ffifgt00f  uF a E             DISK    rename(FIFGT000:FGTFisico)
000900090424     F****************************         usropn
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
002800090424     D STP_DATdeco     s              8s 0
002900081230      *
003000090109     D KPJBA         E DS
003100090109      *
003200090109     D ficn05ds      E DS
003300090109     D dGPI          E DS
003400900521     C*****************************************************************
003500090112     C****+ dec( year(curdate())*10000 + month(curdate())*100 +
003600090112     C****+ day(curdate()) , 8, 0)
003700090112     C****+ between fgtddt and fgtdst
003800090112     C*****************************************************************
003900090422     C*
004000090423     c                   clear                   savSTPSMLpdr      8
004100081209     C                   SETON                                        LR
004200090422     C*
004300090422     C*=================================================================
004400020418     C*
004500090422     C*=================================================================
004600090422      **
004700090423      **  Ricordiamo che:
004800090423      **    PRIMA sono state fatte scadere al 30/04/2009 tutte le altre
004900090423      *      che non devono essere CONVERTITE.
005000090422      **
005100090423      **  Si divide in 2 il problema:
005200090423      **  1)Gli autisti che hanno tariffe relative ad un solo Periodo da ricondurre
005300090423      **    ad un unico progressivo 200 decorrenza 1/5 scadenza 31/12/2039
005400090423      **    e
005500090423      **  2)Gli autisti che invece hanno + tariffe in periodi diversi occorre generare
005600090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
005700090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
005800090423      **    decorrenza del progressivo 201 e così via......
005900090423      **
006000090424      **  ATTENZIONE: nel 2° PROBLEMA posso dividere ancora in 2 le cose:
006100090424      **   Gli AUT che hanno tutti i relativi FGT in scadenza oltre il 2009
006200090424      **   ma con decorrenze differenti fra cui alcune maggiori/uguali del 1/5
006300090424      **    In tal caso si dovranno far scadere le tar. precedenti al 1/5 al 30/4
006400090424      **    e riaperte con progr.200 dal 1/5  scadenza 20391231
006500090424      **   mentre vanno aggiornate con progr.200 quelle con decorrenza 1/5 e
006600090424      **   scadenza 20391231
006700090424      **
006800090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
006900090423      **   devono risultare
007000090423      **   NON CONFERMATE
007100090423      **
007200090423     C*=================================================================
007300090423      **  Queste sono le tariffe da CONVERTIRE creando le SUPERTESTATE
007400090423     C*=================================================================
007500090424      ** 2° PROBLEMA
007600090423      **    DEVE leggere tutte le tariffe AUTISTI (C,R,G) con data decorrenza
007700090422      **  minore del 2010 e con Data SCADENZA superiore al 1°maggio 2009
007800090424      **  ma riconducibili a più periodi però con almeno una
007900090424      **  decorrenza superiore/uguale al 1/5/2009 e tutte con scadenza oltre il 2009.
008000090424      *
008100090424      *  esempio:
008200090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA    DATAMIN
008300090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.   SCADENZA
008400090424      *                         PADR                          TAR.         TAR.
008500090424      *
008600090424      *    700735                 C    300         EUR    2002/01/01  2039/12/31  2039/12/31
008700090424      *    700735                 C    999         EUR    2002/01/01  2039/12/31  2039/12/31
008800090424      *    700735                 G    999    6    EUR    2009/05/01  2040/12/31  2039/12/31
008900090424      *    700735                 R    300         EUR    2002/01/01  2039/12/31  2039/12/31
009000090424      *
009100090424      *    700738                 C    300    1    EUR    2004/03/31  2040/12/31  2040/12/31
009200090424      *    700738                 C    999    1    EUR    2004/03/31  2040/12/31  2040/12/31
009300090424      *    700738                 G    999    7    EUR    2009/05/01  2040/12/31  2040/12/31
009400090424      *    700738                 R    300    1    EUR    2004/03/31  2040/12/31  2040/12/31
009500090424      *                                                   Diverse         oltre il 2009
009600090424      *
009700090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009800090424     C+    with AUTSIMULA (AUTISTA, SIMULA, DATAMAX) as (
009900090424     C+    select fgtpdr, fgtsml, max(fgtddt) from
010000090424     C+    fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
010100090424     C+    and fgtddt < 20100101 and fgtdst >= 20090501
010200090424     C+    group BY FGTpdr, FGTsml
010300090424     C+            HAVING max(fgtddt) >= 20090501)
010400090424     C+  SELECT DATAMAXDECO, DATAMINSCAD,
010500090424     C+  a.* , rrn(a) FROM
010600090424     C+  fifgt00f as a join AUTSIMULA on a.fgtpdr=Autista
010700090424     C+  and a.fgtsml=SIMULA
010800090424     C+  join (
010900090424     C+  select fgtpdr, fgtsml, min(fgtdst) as DATAMINSCAD,
011000090424     C+  max(fgtddt) as DATAMAXDECO
011100090424     C+  from
011200090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
011300090424     C+  and fgtddt < 20100101 and
011400090424     C+  fgtdst >= 20090501
011500090424     C+  group BY FGTpdr, FGTsml
011600090424     C+           HAVING min(fgtdst) > 20091231
011700090424     C+  )  as SCADENZA
011800090424     C+   on a.fgtpdr=SCADENZA.fgtpdr and a.fgtsml=SCADENZA.fgtsml
011900090424     C+  WHERE  fgttsr in ('C', 'R', 'G')
012000090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
012100090424     C+  and a.fgtsml concat digits(a.fgtpdr) in
012200090424     C+  (
012300090424     C+  select fgtsml concat digits(fgtpdr) from
012400090424     C+  (
012500090424     C+  select FGTSML, FGTPDR, count(*) from
012600090424     C+  (
012700090424     C+  SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
012800090424     C+  fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
012900090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
013000090424     C+  group BY FGTSML, FGTPDR, FGTDDT, FGTDST
013100090424     C+  ) as b
013200090424     C+  group BY FGTSML, FGTPDR
013300090424     C+  having count(*) > 1
013400090424     C+  ) as unPERIODO
013500090424     C+  )
013600090424     C+  ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
013700090422     C/END-EXEC
013800081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
013900081110     c                   if        sqlCODE < 0
014000081110     c                   eval      esito = Sqlcode
014100081110     c                   dump(a)
014200081110     c                   return
014300081110     c                   end
014400081110     C/EXEC SQL
014500081110     C+ OPEN A1
014600081110     C/END-EXEC
014700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
014800081110     c                   if        sqlCODE < 0
014900081110     c                   eval      esito = Sqlcode
015000081110     c                   dump(a)
015100081110     c                   return
015200081110     c                   else
015300081110     c                   except    testa
015400081110     c                   end
015500090109      * >>>>>>>
015600081209      * resto nel ciclo finchè....
015700081110     C                   DOU       SqlCod < 0
015800081110      * lettura cursore
015900081110     C/EXEC SQL
016000090424     C+ FETCH NEXT FROM A1 INTO :STP_DatDECO, :STP_DatSCAD, :dsFGT,
016100090424     C+ :FGT_REC
016200081110     C/END-EXEC
016300081110      *  EoF
016400081110     C                   if        SqlCod = 100
016500081110     c                   Leave
016600081110      *  x errori
016700081110     C                   ElseIf    SqlCod < 0
016800081110     c                   eval      esito = Sqlcode
016900081110     c                   dump(a)
017000081110     c                   leave
017100081110     C                   ENDif
017200081110      *
017300090428      *
017400090428      *----------------------------->>>>>
017500090428      * Controlli su Angrafica e su Valorizzazioni
017600090428      *
017700090428     c                   clear                   errore           10
017800090428      *
017900090428      * rileva dall'anagrafica
018000090428     c                   eval      apdTIP = 'A'
018100090428     c                   eval      apdPDR = stpPDR
018200090428     c     Key_APD       chain     fiapd01l
018300090428     c                   if        %Found(fiapd01l)
018400090428      *
018500090428     c                   if        apdATB <> *blank
018600090428     c                   movel     'ANNULLATO'   errore
018700090428     c                   else
018800090428      *
018900090428      *-  Esluso da autofatturazione
019000090428     c                   if        apdPDD = 'S'
019100090428     c                   movel     'AUTOFAT'     errore
019200090428     c                   end
019300090428      *
019400090428     c                   endIF
019500090428      *
019600090428     c                   else
019700090428     c                   movel     'NO_APD'      errore
019800090428     c                   end
019900090428      *
020000090428      *  Controlla sulle valorizzazioni che non ce ne siano oppure
020100090428      *   siano precedenti al 20090101
020200090429      *  SEMPRE CHE non siano in decorrenza fra il 1/4 ed il 31/12
020300090429     c                   if        STPddt < 20090401 or STPddt > 20091231
020400090429      *
020500090428     c                   if        errore = *blank
020600090428     c     stppdr        chain     fiftt02l
020700090428     c                   if        not %Found(fiftt02l)
020800090428     c                   movel     'NO_in_FTT'   errore
020900090428     c                   end
021000090428      *
021100090428     c                   if        errore = *blank
021200090428      *
021300090428     c                   eval      fttpdr = stpPDR
021400090428     c                   eval      fttfvl = 'C'
021500090428      *
021600090428     c     Key_FTT       setgt     fiftt02l
021700090428     c     Key_FTT       readpe    fiftt02l
021800090428      * Se inferiore al 2009
021900090428     c                   if        not %Eof(fiftt02l)
022000090428      *
022100090428     c                   if        fttddc < 20090101
022200090428     c                   movel     'FTT_<_2009'  errore
022300090428     c                   end
022400090428      *  SE EoF
022500090428     c                   else
022600090428     c                   movel     'FTT_no_C'    errore
022700090428     c                   end
022800090428     c
022900090428     c                   end
023000090428     c                   end
023100090429     c                   end
023200090428      *----------------------------->>>>>
023300090428     c                   movel     errore        error5            5
023400090429      * NONOSTANTE l'errore se la Decorrenza è > del 30 Aprile
023500090429      *   devo tradurli
023600090429     c                   if         stpDDT > 20090430 and errore <> *blank
023700090429     c                   move      '/X'          error5
023800090429     c                   move      ' /EXE'       errore
023900090429     c                   end
024000090429      *
024100090427      *  se sono qui ho letto un record correttamente
024200090427     c   Of              except    testa
024300090427     c                   except    detta
024400090428      *
024500090428      *----------------------------->>>>>
024600090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
024700090428      *
024800090428     c                   if        Errore  = *blank
024900090429     c                              or  stpDDT > 20090430
025000090428      *----------------------------->>>>>
025100090428      *
025200090428     c                   clear                   eseguito          1
025300090428     c                   clear                   verifica          1
025400090427      *
025500081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
025600090423     c                   movel     stpSML        stpSMLPDR         8
025700090109     c                   move      stpPDR        Autista           7 0
025800090423     c                   move      Autista       stpSMLPDR
025900090109      *
026000090109      *  Solo in aggiornamento
026100081110     c                   if        Aggiorna = 'S'
026200090424      *
026300090424      *  Deve scrivere la testata poichè comunuque ha un SOLO periodo
026400090424      *  dal 1/5/2009 al 31/12/2039
026500090109      *    a rottura di simulazione/autista
026600090423      *     salva chiave di rottura
026700090423     c                   if        STPsmlPDR <> savSTPsmlPDR
026800090423     c                   eval      savSTPsmlPDR = STPsmlPDR
026900090423      *
027000090423      *  IMPOSTA la SUERTESTATA
027100090109     c                   clear                   fitgt000
027200090423     c                   z-add     STPpdr        tgtPDR
027300090423     c                   move      STPsml        tgtSML
027400090423     c                   z-add     200           tgtPRG
027500090423     c                   move      STPdiv        tgtDIV
027600090424      **
027700090423      * data decorrenza/scadenza
027800090423     c                   z-add     20090501      tgtDDT
027900090423      *
028000090424      * CON L'SQL la data scadenza sarà SEMPRE SUPERIORE al 2009
028100090424      *  QUINDI sempre la data all'infinito 31/12/2039
028200090424     c                   if        stp_DatSCAD > 20091231
028300090423     c                   z-add     20391231      tgtDST
028400090423     c                   else
028500090424     c                   z-add     stp_DatSCAD   tgtDST
028600090423     c                   end
028700090423      *
028800090423     c                   z-add     *date         tgtDUV
028900090423     c                   moveL     'Convers'     tgtPRU
029000090401      *
029100090423      * rileva dall'anagrafica
029200090401     c                   eval      apdTIP = 'A'
029300090401     c                   eval      apdPDR = tgtPDR
029400090401     c     Key_APD       chain     fiapd01l
029500090423      *---
029600090401     c                   if        %Found(fiapd01l)
029700090423      *   società fornitore
029800090421     c                   movel     apdcsf        tgtsoc
029900090421     c                   movel     *all'0'       tgtcdf
030000090421     c                   move      apdksc        tgtcdf
030100090421     c                   movel     apdflr        dapdflr
030200090423      *  la massa in KG
030300090421     c                   eval      tgtpkl = §APDMCP
030400090423     c*  i tempi di carico e scarico
030500090421     c                   clear                   ficn08ds
030600090421     c                   eval      i08data= tgtddt
030700090421     c                   eval      i08tip= 'A'
030800090421     c                   eval      i08mcp = tgtpkl
030900090421     c                   call      'FICN08R'
031000090421     c                   parm                    ficn08ds
031100090421     c                   if        o08err =  ' '
031200090421     c                   eval      TGTTMPC = o08tc
031300090421     c                   eval      TGTTMPS = o08ts
031400090421     c                   end
031500090423      *---
031600090401     c                   end
031700090423      *
031800090423      *  prezzo carburante al litro
031900090109     c                   z-add     D§GPIGPI      tgtPCAR
032000090422      * Attenzione:
032100090423      *     se non sono state stampate nel vecchio
032200090423      *        non devono essere convalidate nel nuovo
032300090424      * MA CON QUESTO SPECIFICO SQL potrei avere righe tariffe precedenti l'1/5
032400090424      * già STAMPATE e
032500090424      * TARIFFE decorrenti dal 1/5 non ANCORA STAMPATE......
032600090424      ***
032700090424      * Attenzione:
032800090424      *   C O S A     F A R E  ???????
032900090424      *     Ci sarà un pgm finale che allinea le date stampa x la superTESTATA
033000090424     c                   if        stpDTS > 0
033100090424     c                   z-add     20090501      tgtDTS
033200090424     c                   end
033300090422      *
033400090109     c                   write     fitgt000
033500090109     c                   end
033600090423      *------------------
033700081216      *
033800090423      *  genera o aggiorna
033900090423      *     i dettagli con il nuovo progressivo
034000090423     c                   exsr      FPT_FPD
034100090109      *
034200090109      *------------------
034300081218      *   deve eliminare i puntamenti del codice di riferimento
034400090424     c*************      open      fifgt00f
034500090423      *
034600081218     c     fgt_REC       chain     FIFGT00F
034700081218     c                   if        %Found(FIFGT00F)
034800090112      *
034900090422      *  Fa scadere la testata al 30 apr.2009
035000090423     c                   if            fgtddt < 20090501
035100090422     c                   eval      fgtDST = 20090430
035200090423     c                   end
035300090424      **
035400090422      *     Ma se decorreva già dal 1/5/2009
035500090422      *   non può farlo scadere il 30/4/2009
035600090112      * e quindi questa diventa la tariffa della supertestata
035700090423     c                   if            fgtddt = 20090501
035800090423      * data scadenza
035900090424      *   CON QUESTO SQL è SEMPRE MAGGIORE
036000090423     c                   if           fgtDST > 20091231
036100090423     c                   z-add     20391231      fgtDST
036200090423     c                   endIf
036300090423      *
036400090112     c                   z-add     200           fgtPRG
036500090112     c                   z-add     *date         fgtDUV
036600090112     c                   clear                   fgtDCN
036700090423      *
036800090423      * solo se era vuoto anche prima
036900090423     c                   if            stpDTS > 0
037000090424     c                   z-add     20090501      fgtDTS
037100090423     c                   elseif        stpDTS = 0
037200090112     c                   clear                   fgtDTS
037300090423     c                   end
037400090112     c                   clear                   fgtFTR
037500090112     c                   clear                   fgtDTR
037600090423      *
037700090423     c                   end
037800090423      *
037900090423      * aggiorna
038000081218     c                   update    fgtFisico
038100090109      *
038200090423      *  Quindi scrive un'altra testata SOLO SE decorrenza < 1/5/2009
038300090423     c                   IF           fgtDDT < 20090501
038400090420     c                   eval      fgtDDT = 20090501
038500090423      *
038600090423     c                   if        stpDST > 20091231
038700090423     c                   z-add     20391231      fgtDST
038800090423     c                   else
038900090424     c                   z-add     stp_DATscad   fgtDST
039000090423     c                   end
039100090423      *
039200090109     c                   z-add     200           fgtPRG
039300090109     c                   z-add     *date         fgtDUV
039400090109     c                   clear                   fgtDCN
039500090424     c*****************  z-add     tgtDTS        fgtDTS
039600090109     c                   clear                   fgtFTR
039700090109     c                   clear                   fgtDTR
039800090109     c                   write     fgtFisico
039900090423     c                   endIF
040000090112      *
040100090423     c                   endIF
040200090112      *
040300090424     c************       close     fifgt00f
040400090109      *
040500090109     c                   endif
040600090109      *------------------
040700090428      *----------------------------->>>>>
040800090428     c                   end
040900090428      *----------------------------->>>>>
041000090428      *
041100090424      *  scrive il dettaglio su un file
041200090424     c                   exsr      wri_DETTA
041300081110      *
041400081110     C                   ENDDO
041500081110     C/EXEC SQL
041600081110     C+ Close A1
041700081110     C/END-EXEC
041800081110     C                   if        Esito < 0
041900081110     c                   return
042000081110     c                   endIF
042100081110     C*
042200081110     C     FINE          TAG
042300081110     c                   except    finest
042400081110     c                   return
042500081209     C*------------------------------------------------------------------------*
042600081209     C* *INZSR - OPERAZIONI INIZIALI
042700081209     C*------------------------------------------------------------------------*
042800081209     C     *INZSR        BEGSR
042900081209      *
043000081209     C     *ENTRY        PLIST
043100081209     C                   PARM                    AGGIORNA          1
043200081209     C*
043300090109      * duplica le tariffe FGT
043400081209     C     fgt01K        kLIST
043500081211     C                   kfld                    stpPDR
043600081211     C                   kfld                    stpSML
043700081211     C                   kfld                    stpTSR
043800081211     C                   kfld                    stpCTR
043900081211     C                   kfld                    stpPRG
044000090109      *
044100090109      * duplica le tariffe FPT e FPD
044200090109     c     Key_FPT       KLIST
044300090109     c                   kfld                    stpPDR
044400090109     c                   kfld                    stpSML
044500090109     c                   kfld                    stpTSR
044600090109     c                   kfld                    stpCTR
044700090109     c                   kfld                    stpPRG
044800081209     C*
044900090401     c     Key_APD       KLIST
045000090401     c                   kfld                    apdTIP
045100090401     c                   kfld                    apdPDR
045200090401     C*
045300090428     c     Key_FTT       KLIST
045400090428     c                   kfld                    FTTPDR
045500090428     c                   kfld                    FTTFVL
045600090428     C*
045700081209     c                   clear                   esito
045800081209      *
045900081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
046000081209     C                   clear                   UDATE_8           8 0
046100081209     C                   MOVEL     *DATE         UDATE_8
046200090109      *
046300090109      * reperisce il prezzo di riferimento del carburante
046400090109     c                   call      'FICN05R'
046500090109     c                   parm                    ficn05DS
046600090109      *
046700090109     c                   iF        o05err = *blank
046800090109     c                   eval      dGPI = o05GPI
046900090109     c                   enD
047000090109      *
047100081209     c                   endSR
047200090109      *---------------------------------------------------------------*
047300090109      *  Scrive Testate e Dettaglio
047400090109      *---------------------------------------------------------------*
047500090423     c     FPT_FPD       begSR
047600090109     C*
047700090109     c     Key_FPT       setll     fifpt01L
047800090109     c     Key_FPT       reade     fifpt01L
047900090109     c                   Dow       not %EoF(fifpt01l)
048000090423     C*
048100090109     c                   z-add     200           fptPRG
048200090423     C*
048300090424      * se decorre dal 1/5 allora deve aggiornare
048400090423     c                   if        stpDDT = 20090501
048500090423     c                   update    fifpt000
048600090423     c                   else
048700090109     c                   write     fifpt000
048800090423     c                   end
048900090423      *
049000090109     c     Key_FPT       reade     fifpt01L
049100090109     c                   endDo
049200090109     C*
049300090109     c     Key_FPT       setll     fifpd01L
049400090109     c     Key_FPT       reade     fifpd01L
049500090109     c                   Dow       not %EoF(fifpd01l)
049600090423      *
049700090109     c                   z-add     200           fpdPRG
049800090423      *
049900090423      * se decorre dal 1/5 allora deve aggiornare
050000090423     c                   if        stpDDT = 20090501
050100090423     c                   update    fifpd000
050200090423     c                   else
050300090423     c                   write     fifpd000
050400090423     c                   end
050500090423      *
050600090109     c     Key_FPT       reade     fifpd01L
050700090109     c                   endDo
050800090109     C*
050900090109     c                   endSR
051000081211     C*---------------------------------------------------------------*
051100090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
051200090424      *---------------------------------------------------------------*
051300090424     c     WRI_DETTA     begSR
051400090424     C*
051500090424     c                   eval      WSFSML  =  stpSML
051600090424     c                   eval      WSFPDR  =  stpPDR
051700090424     c                   eval      WSFTSR  =  stpTSR
051800090424     c                   eval      WSFCTR  =  stpCTR
051900090424     c                   eval      WSFPRG  = stpPRG
052000090424     c                   eval      WSFDDT  = stpDDT
052100090424     c                   eval      WSFDST  = stpDST
052200090424     c                   eval      WSFDTS  = stpDTS
052300090424     c                   eval      WSFDCN  = stpDCN
052400090424     c                   eval      WSFREC  = FGT_REC
052500090424     c                   eval      WSFDECOR= stp_DATdeco
052600090424     c                   eval      WSFSCAD = stp_DATscad
052700090424     c                   eval      WSFPROGR= 'TNVRA17R'
052800090428     c**
052900090428     c                   eval      WSFNOTE = Errore
053000090424     c**
053100090424     c                   write     WSFGT000
053200090424     C*
053300090424     c                   endSR
053400090424     C*---------------------------------------------------------------*
053500081110     OQSYSPRT   E            TESTA            02
053600081110     O                                           16 'Simul'
053700081110     O                                           25 'Autista'
053800081110     O                                           30 'TSR'
053900081110     O                                           38 'Tariffa'
054000081110     O                                           45 'Progr'
054100081110     O                                           55 'Data_Dec'
054200081110     O                                           65 'Data_Sca'
054300081110     O                                           75 'Data_Stp'
054400081110     O                                           85 'Data_Cnv'
054500081211     O                                          115 'N_Rec_FGT'
054600090424     O                                          125 'Data_Min'
054700081110     O          E            DETta       1
054800081110     O                       stpSML              14
054900081110     O                       stpPDR        z     25
055000081110     O                       stpTSR              29
055100081110     O                       stpCTR              36
055200081110     O                       stpPRG        z     43
055300081110     O                       stpDDT        z     55
055400081110     O                       stpDST        z     65
055500081110     O                       stpDTS        z     75
055600081110     O                       stpDCN        z     85
055700081211     o                       FGT_REC       z    115
055800090424     O                       stp_DATscad   z    125
055900090428     O                       ERROR5             132
056000081110      ****
056100081110     o          E            FINEST      2
056200090424     o                                           30 '*** TNVRA17R ***'
056300090102     o                                           59 '***   FINE  STAMPA  ***'
056400090109     o                                          + 9 '***  SUPER TARIFFA  ***'
