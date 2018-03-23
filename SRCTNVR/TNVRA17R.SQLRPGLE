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
020300090430      *
020400090430     c                   z-add     0             datamaxima        8 0
020500090430     C/EXEC SQL
020600090430     C+ SELECT max(fgtddt) INTO :datamaxima FROM edpab/svFgt00F WHERE
020700090430     C+ fgtpdr = :STPpdr and fgtsml= :STPsml and FGTTSR in ('C', 'R', 'G')
020800090430     C/END-EXEC
020900090430     c                   if        datamaxima < 20090401
021000090429      *
021100090428     c                   if        errore = *blank
021200090428     c     stppdr        chain     fiftt02l
021300090428     c                   if        not %Found(fiftt02l)
021400090428     c                   movel     'NO_in_FTT'   errore
021500090428     c                   end
021600090428      *
021700090428     c                   if        errore = *blank
021800090428      *
021900090428     c                   eval      fttpdr = stpPDR
022000090428     c                   eval      fttfvl = 'C'
022100090428      *
022200090428     c     Key_FTT       setgt     fiftt02l
022300090428     c     Key_FTT       readpe    fiftt02l
022400090428      * Se inferiore al 2009
022500090428     c                   if        not %Eof(fiftt02l)
022600090428      *
022700090428     c                   if        fttddc < 20090101
022800090428     c                   movel     'FTT_<_2009'  errore
022900090428     c                   end
023000090428      *  SE EoF
023100090428     c                   else
023200090428     c                   movel     'FTT_no_C'    errore
023300090428     c                   end
023400090428     c
023500090428     c                   end
023600090428     c                   end
023700090429     c                   end
023800090428      *----------------------------->>>>>
023900090428     c                   movel     errore        error5            5
024000090429      * NONOSTANTE l'errore se la Decorrenza è > del 30 Aprile
024100090429      *   devo tradurli
024200090429     c                   if         stpDDT > 20090430 and errore <> *blank
024300090429     c                   move      '/X'          error5
024400090429     c                   move      ' /EXE'       errore
024500090429     c                   end
024600090429      *
024700090427      *  se sono qui ho letto un record correttamente
024800090427     c   Of              except    testa
024900090427     c                   except    detta
025000090428      *
025100090428      *----------------------------->>>>>
025200090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
025300090428      *
025400090428     c                   if        Errore  = *blank
025500090429     c                              or  stpDDT > 20090430
025600090428      *----------------------------->>>>>
025700090428      *
025800090428     c                   clear                   eseguito          1
025900090428     c                   clear                   verifica          1
026000090427      *
026100081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
026200090423     c                   movel     stpSML        stpSMLPDR         8
026300090109     c                   move      stpPDR        Autista           7 0
026400090423     c                   move      Autista       stpSMLPDR
026500090109      *
026600090109      *  Solo in aggiornamento
026700081110     c                   if        Aggiorna = 'S'
026800090424      *
026900090424      *  Deve scrivere la testata poichè comunuque ha un SOLO periodo
027000090424      *  dal 1/5/2009 al 31/12/2039
027100090109      *    a rottura di simulazione/autista
027200090423      *     salva chiave di rottura
027300090423     c                   if        STPsmlPDR <> savSTPsmlPDR
027400090423     c                   eval      savSTPsmlPDR = STPsmlPDR
027500090423      *
027600090423      *  IMPOSTA la SUERTESTATA
027700090109     c                   clear                   fitgt000
027800090423     c                   z-add     STPpdr        tgtPDR
027900090423     c                   move      STPsml        tgtSML
028000090423     c                   z-add     200           tgtPRG
028100090423     c                   move      STPdiv        tgtDIV
028200090424      **
028300090423      * data decorrenza/scadenza
028400090423     c                   z-add     20090501      tgtDDT
028500090423      *
028600090424      * CON L'SQL la data scadenza sarà SEMPRE SUPERIORE al 2009
028700090424      *  QUINDI sempre la data all'infinito 31/12/2039
028800090424     c                   if        stp_DatSCAD > 20091231
028900090423     c                   z-add     20391231      tgtDST
029000090423     c                   else
029100090424     c                   z-add     stp_DatSCAD   tgtDST
029200090423     c                   end
029300090423      *
029400090423     c                   z-add     *date         tgtDUV
029500090423     c                   moveL     'Convers'     tgtPRU
029600090401      *
029700090423      * rileva dall'anagrafica
029800090401     c                   eval      apdTIP = 'A'
029900090401     c                   eval      apdPDR = tgtPDR
030000090401     c     Key_APD       chain     fiapd01l
030100090423      *---
030200090401     c                   if        %Found(fiapd01l)
030300090423      *   società fornitore
030400090421     c                   movel     apdcsf        tgtsoc
030500090421     c                   movel     *all'0'       tgtcdf
030600090421     c                   move      apdksc        tgtcdf
030700090421     c                   movel     apdflr        dapdflr
030800090423      *  la massa in KG
030900090421     c                   eval      tgtpkl = §APDMCP
031000090423     c*  i tempi di carico e scarico
031100090421     c                   clear                   ficn08ds
031200090421     c                   eval      i08data= tgtddt
031300090421     c                   eval      i08tip= 'A'
031400090421     c                   eval      i08mcp = tgtpkl
031500090421     c                   call      'FICN08R'
031600090421     c                   parm                    ficn08ds
031700090421     c                   if        o08err =  ' '
031800090421     c                   eval      TGTTMPC = o08tc
031900090421     c                   eval      TGTTMPS = o08ts
032000090421     c                   end
032100090423      *---
032200090401     c                   end
032300090423      *
032400090423      *  prezzo carburante al litro
032500090109     c                   z-add     D§GPIGPI      tgtPCAR
032600090422      * Attenzione:
032700090423      *     se non sono state stampate nel vecchio
032800090423      *        non devono essere convalidate nel nuovo
032900090424      * MA CON QUESTO SPECIFICO SQL potrei avere righe tariffe precedenti l'1/5
033000090424      * già STAMPATE e
033100090424      * TARIFFE decorrenti dal 1/5 non ANCORA STAMPATE......
033200090424      ***
033300090424      * Attenzione:
033400090424      *   C O S A     F A R E  ???????
033500090424      *     Ci sarà un pgm finale che allinea le date stampa x la superTESTATA
033600090424     c                   if        stpDTS > 0
033700090424     c                   z-add     20090501      tgtDTS
033800090424     c                   end
033900090422      *
034000090109     c                   write     fitgt000
034100090109     c                   end
034200090423      *------------------
034300081216      *
034400090423      *  genera o aggiorna
034500090423      *     i dettagli con il nuovo progressivo
034600090423     c                   exsr      FPT_FPD
034700090109      *
034800090109      *------------------
034900081218      *   deve eliminare i puntamenti del codice di riferimento
035000090424     c*************      open      fifgt00f
035100090423      *
035200081218     c     fgt_REC       chain     FIFGT00F
035300081218     c                   if        %Found(FIFGT00F)
035400090112      *
035500090422      *  Fa scadere la testata al 30 apr.2009
035600090423     c                   if            fgtddt < 20090501
035700090422     c                   eval      fgtDST = 20090430
035800090423     c                   end
035900090424      **
036000090422      *     Ma se decorreva già dal 1/5/2009
036100090422      *   non può farlo scadere il 30/4/2009
036200090112      * e quindi questa diventa la tariffa della supertestata
036300090423     c                   if            fgtddt = 20090501
036400090423      * data scadenza
036500090424      *   CON QUESTO SQL è SEMPRE MAGGIORE
036600090423     c                   if           fgtDST > 20091231
036700090423     c                   z-add     20391231      fgtDST
036800090423     c                   endIf
036900090423      *
037000090112     c                   z-add     200           fgtPRG
037100090112     c                   z-add     *date         fgtDUV
037200090112     c                   clear                   fgtDCN
037300090423      *
037400090423      * solo se era vuoto anche prima
037500090423     c                   if            stpDTS > 0
037600090424     c                   z-add     20090501      fgtDTS
037700090423     c                   elseif        stpDTS = 0
037800090112     c                   clear                   fgtDTS
037900090423     c                   end
038000090112     c                   clear                   fgtFTR
038100090112     c                   clear                   fgtDTR
038200090423      *
038300090423     c                   end
038400090423      *
038500090423      * aggiorna
038600081218     c                   update    fgtFisico
038700090109      *
038800090423      *  Quindi scrive un'altra testata SOLO SE decorrenza < 1/5/2009
038900090423     c                   IF           fgtDDT < 20090501
039000090420     c                   eval      fgtDDT = 20090501
039100090423      *
039200090423     c                   if        stpDST > 20091231
039300090423     c                   z-add     20391231      fgtDST
039400090423     c                   else
039500090424     c                   z-add     stp_DATscad   fgtDST
039600090423     c                   end
039700090423      *
039800090109     c                   z-add     200           fgtPRG
039900090109     c                   z-add     *date         fgtDUV
040000090109     c                   clear                   fgtDCN
040100090424     c*****************  z-add     tgtDTS        fgtDTS
040200090109     c                   clear                   fgtFTR
040300090109     c                   clear                   fgtDTR
040400090109     c                   write     fgtFisico
040500090423     c                   endIF
040600090112      *
040700090423     c                   endIF
040800090112      *
040900090424     c************       close     fifgt00f
041000090109      *
041100090109     c                   endif
041200090109      *------------------
041300090428      *----------------------------->>>>>
041400090428     c                   end
041500090428      *----------------------------->>>>>
041600090428      *
041700090424      *  scrive il dettaglio su un file
041800090424     c                   exsr      wri_DETTA
041900081110      *
042000081110     C                   ENDDO
042100081110     C/EXEC SQL
042200081110     C+ Close A1
042300081110     C/END-EXEC
042400081110     C                   if        Esito < 0
042500081110     c                   return
042600081110     c                   endIF
042700081110     C*
042800081110     C     FINE          TAG
042900081110     c                   except    finest
043000081110     c                   return
043100081209     C*------------------------------------------------------------------------*
043200081209     C* *INZSR - OPERAZIONI INIZIALI
043300081209     C*------------------------------------------------------------------------*
043400081209     C     *INZSR        BEGSR
043500081209      *
043600081209     C     *ENTRY        PLIST
043700081209     C                   PARM                    AGGIORNA          1
043800081209     C*
043900090109      * duplica le tariffe FGT
044000081209     C     fgt01K        kLIST
044100081211     C                   kfld                    stpPDR
044200081211     C                   kfld                    stpSML
044300081211     C                   kfld                    stpTSR
044400081211     C                   kfld                    stpCTR
044500081211     C                   kfld                    stpPRG
044600090109      *
044700090109      * duplica le tariffe FPT e FPD
044800090109     c     Key_FPT       KLIST
044900090109     c                   kfld                    stpPDR
045000090109     c                   kfld                    stpSML
045100090109     c                   kfld                    stpTSR
045200090109     c                   kfld                    stpCTR
045300090109     c                   kfld                    stpPRG
045400081209     C*
045500090401     c     Key_APD       KLIST
045600090401     c                   kfld                    apdTIP
045700090401     c                   kfld                    apdPDR
045800090401     C*
045900090428     c     Key_FTT       KLIST
046000090428     c                   kfld                    FTTPDR
046100090428     c                   kfld                    FTTFVL
046200090428     C*
046300081209     c                   clear                   esito
046400081209      *
046500081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
046600081209     C                   clear                   UDATE_8           8 0
046700081209     C                   MOVEL     *DATE         UDATE_8
046800090109      *
046900090109      * reperisce il prezzo di riferimento del carburante
047000090109     c                   call      'FICN05R'
047100090109     c                   parm                    ficn05DS
047200090109      *
047300090109     c                   iF        o05err = *blank
047400090109     c                   eval      dGPI = o05GPI
047500090109     c                   enD
047600090109      *
047700081209     c                   endSR
047800090109      *---------------------------------------------------------------*
047900090109      *  Scrive Testate e Dettaglio
048000090109      *---------------------------------------------------------------*
048100090423     c     FPT_FPD       begSR
048200090109     C*
048300090109     c     Key_FPT       setll     fifpt01L
048400090109     c     Key_FPT       reade     fifpt01L
048500090109     c                   Dow       not %EoF(fifpt01l)
048600090423     C*
048700090109     c                   z-add     200           fptPRG
048800090423     C*
048900090424      * se decorre dal 1/5 allora deve aggiornare
049000090423     c                   if        stpDDT = 20090501
049100090423     c                   update    fifpt000
049200090423     c                   else
049300090109     c                   write     fifpt000
049400090423     c                   end
049500090423      *
049600090109     c     Key_FPT       reade     fifpt01L
049700090109     c                   endDo
049800090109     C*
049900090109     c     Key_FPT       setll     fifpd01L
050000090109     c     Key_FPT       reade     fifpd01L
050100090109     c                   Dow       not %EoF(fifpd01l)
050200090423      *
050300090109     c                   z-add     200           fpdPRG
050400090423      *
050500090423      * se decorre dal 1/5 allora deve aggiornare
050600090423     c                   if        stpDDT = 20090501
050700090423     c                   update    fifpd000
050800090423     c                   else
050900090423     c                   write     fifpd000
051000090423     c                   end
051100090423      *
051200090109     c     Key_FPT       reade     fifpd01L
051300090109     c                   endDo
051400090109     C*
051500090109     c                   endSR
051600081211     C*---------------------------------------------------------------*
051700090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
051800090424      *---------------------------------------------------------------*
051900090424     c     WRI_DETTA     begSR
052000090424     C*
052100090424     c                   eval      WSFSML  =  stpSML
052200090424     c                   eval      WSFPDR  =  stpPDR
052300090424     c                   eval      WSFTSR  =  stpTSR
052400090424     c                   eval      WSFCTR  =  stpCTR
052500090424     c                   eval      WSFPRG  = stpPRG
052600090424     c                   eval      WSFDDT  = stpDDT
052700090424     c                   eval      WSFDST  = stpDST
052800090424     c                   eval      WSFDTS  = stpDTS
052900090424     c                   eval      WSFDCN  = stpDCN
053000090424     c                   eval      WSFREC  = FGT_REC
053100090424     c                   eval      WSFDECOR= stp_DATdeco
053200090424     c                   eval      WSFSCAD = stp_DATscad
053300090424     c                   eval      WSFPROGR= 'TNVRA17R'
053400090428     c**
053500090428     c                   eval      WSFNOTE = Errore
053600090424     c**
053700090424     c                   write     WSFGT000
053800090424     C*
053900090424     c                   endSR
054000090424     C*---------------------------------------------------------------*
054100081110     OQSYSPRT   E            TESTA            02
054200081110     O                                           16 'Simul'
054300081110     O                                           25 'Autista'
054400081110     O                                           30 'TSR'
054500081110     O                                           38 'Tariffa'
054600081110     O                                           45 'Progr'
054700081110     O                                           55 'Data_Dec'
054800081110     O                                           65 'Data_Sca'
054900081110     O                                           75 'Data_Stp'
055000081110     O                                           85 'Data_Cnv'
055100081211     O                                          115 'N_Rec_FGT'
055200090424     O                                          125 'Data_Min'
055300081110     O          E            DETta       1
055400081110     O                       stpSML              14
055500081110     O                       stpPDR        z     25
055600081110     O                       stpTSR              29
055700081110     O                       stpCTR              36
055800081110     O                       stpPRG        z     43
055900081110     O                       stpDDT        z     55
056000081110     O                       stpDST        z     65
056100081110     O                       stpDTS        z     75
056200081110     O                       stpDCN        z     85
056300081211     o                       FGT_REC       z    115
056400090424     O                       stp_DATscad   z    125
056500090428     O                       ERROR5             132
056600081110      ****
056700081110     o          E            FINEST      2
056800090424     o                                           30 '*** TNVRA17R ***'
056900090102     o                                           59 '***   FINE  STAMPA  ***'
057000090109     o                                          + 9 '***  SUPER TARIFFA  ***'
