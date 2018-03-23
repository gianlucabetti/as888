000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090424     H* TNVRA18R *---------------------------------------------------*
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
002900090427     D STP_GIORNI      s              8s 0
003000081230      *
003100090109     D KPJBA         E DS
003200090109      *
003300090109     D ficn05ds      E DS
003400090109     D dGPI          E DS
003500900521     C*****************************************************************
003600090112     C****+ dec( year(curdate())*10000 + month(curdate())*100 +
003700090112     C****+ day(curdate()) , 8, 0)
003800090112     C****+ between fgtddt and fgtdst
003900090112     C*****************************************************************
004000090422     C*
004100090423     c                   clear                   savSTPSMLpdr      8
004200081209     C                   SETON                                        LR
004300090422     C*
004400090422     C*=================================================================
004500020418     C*
004600090422     C*=================================================================
004700090422      **
004800090423      **  Ricordiamo che:
004900090423      **    PRIMA sono state fatte scadere al 30/04/2009 tutte le altre
005000090423      *      che non devono essere CONVERTITE.
005100090422      **
005200090424      ** Si divide ancora in 2 il 2° PROBLEMA:
005300090424      **  a) Le Tariffe che hanno fatto scadere il 3/5 e Decorrere il 4/5
005400090424      **     possono essere fatte ricondurre ad un solo Periodo NUOVO
005500090424      **    ossia
005600090424      **     occorre far scadere il 30/4 quelle del 3/5 e
005700090424      **             far attivare il 1/5 quelle del 4/5
005800090424      **
005900090424      **    ATTENZIONE PERO' sono quelle che hanno 1 solo giorno di DIFFERENZA
006000090424      **      fra DATAmaxDECORRENZA e DATAminSCADENZA  con
006100090424      **      scadenza ENTRO la 1°settimana di MAGGIO
006200090424      **
006300090424      **  b)  RIMANGONO QUINDI le vere tariffe con periodi diversi
006400090424      **     che hanno DATAminSCADENZA oltre la 1°SETTIMANA di Maggio
006500090424      **    occorre x queste generare
006600090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
006700090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
006800090423      **    decorrenza del progressivo 201 e così via......
006900090424      **
007000090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
007100090423      **   devono risultare
007200090423      **   NON CONFERMATE
007300090423      **
007400090423     C*=================================================================
007500090423      **  Queste sono le tariffe da CONVERTIRE creando le SUPERTESTATE
007600090423     C*=================================================================
007700090424      ** 2° PROBLEMA  soluzione (a)
007800090424      *
007900090424      *  esempio:
008000090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA
008100090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.
008200090424      *                         PADR                          TAR.         TAR.
008300090424      *
008400090424      *    190036                 C    300         EUR    2009/03/23  2009/05/03
008500090424      *    190036                 C    300    1    EUR    2009/05/04  2039/12/31
008600090424      *    190036                 G    999         EUR    2009/03/23  2039/12/31
008700090424      *    190036                 R    300         EUR    2009/03/23  2009/05/03
008800090424      *    190036                 R    300    1    EUR    2009/05/04  2039/12/31
008900090424      *
009000090424      *    190037                 C    300         EUR    2009/03/23  2009/05/03
009100090424      *    190037                 C    300    1    EUR    2009/05/04  2039/12/31
009200090424      *    190037                 G    999         EUR    2009/03/23  2039/12/31
009300090424      *    190037                 R    300         EUR    2009/03/23  2009/05/03
009400090424      *    190037                 R    300    1    EUR    2009/05/04  2039/12/31
009500090424      *
009600090424      *    190038                 C    300         EUR    2009/03/23  2009/05/03
009700090424      *    190038                 C    300    1    EUR    2009/05/04  2039/12/31
009800090424      *    190038                 G    999         EUR    2009/03/23  2009/05/03
009900090424      *    190038                 G    999    1    EUR    2009/05/04  2039/12/31
010000090424      *    190038                 R    300         EUR    2009/03/23  2009/05/03
010100090424      *    190038                 R    300    1    EUR    2009/05/04  2039/12/31
010200090424      *
010300090422     C/EXEC SQL DECLARE A1 CURSOR FOR
010400090424     C+  with AUTSIMULA (AUTISTA, SIMULA, DATAMAX) as (
010500090424     C+  select fgtpdr, fgtsml, max(fgtddt) from
010600090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
010700090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
010800090424     C+  group BY FGTpdr, FGTsml
010900090424     C+  having max(fgtddt) >= 20090501)
011000090424     C+     Select * FROM (
011100090424     C+  SELECT DATAMAXDECOR, DATAMINSCAD,
011200090424     C+  (DATAMAXDECOR - DATAMINSCAD) as Giorni,
011300090424     C+  a.* , rrn(a) FROM
011400090424     C+  fifgt00f as a join AUTSIMULA on a.fgtpdr=Autista
011500090424     C+  and a.fgtsml=SIMULA
011600090424     C+  join (
011700090424     C+  select fgtpdr, fgtsml, min(fgtdst) as DATAMINSCAD,
011800090424     C+  max(fgtddt) as DATAMAXDECOR
011900090424     C+  from
012000090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
012100090424     C+  and fgtddt < 20100101 and
012200090424     C+  fgtdst >= 20090501
012300090424     C+  group BY FGTpdr, FGTsml
012400090424     C+  having min(fgtdst) <= 20090509
012500090424     C+  )  as SCADENZA
012600090424     C+   on a.fgtpdr=SCADENZA.fgtpdr and a.fgtsml=SCADENZA.fgtsml
012700090424     C+  WHERE  fgttsr in ('C', 'R', 'G')
012800090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
012900090424     C+  and a.fgtsml concat digits(a.fgtpdr) in
013000090424     C+  (
013100090424     C+  select fgtsml concat digits(fgtpdr) from
013200090424     C+  (
013300090424     C+  select FGTSML, FGTPDR, count(*) from
013400090424     C+  (
013500090424     C+  SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
013600090424     C+  fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
013700090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
013800090424     C+  group BY FGTSML, FGTPDR, FGTDDT, FGTDST
013900090424     C+  ) as b
014000090424     C+  group BY FGTSML, FGTPDR
014100090424     C+  having count(*) > 1
014200090424     C+  ) as unPERIODO
014300090424     C+  )
014400090424     C+  )as DIFFE_GIORNI
014500090424     C+  where GIORNI = 1
014600090424     C+  ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
014700090422     C/END-EXEC
014800081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
014900081110     c                   if        sqlCODE < 0
015000081110     c                   eval      esito = Sqlcode
015100081110     c                   dump(a)
015200081110     c                   return
015300081110     c                   end
015400081110     C/EXEC SQL
015500081110     C+ OPEN A1
015600081110     C/END-EXEC
015700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
015800081110     c                   if        sqlCODE < 0
015900081110     c                   eval      esito = Sqlcode
016000081110     c                   dump(a)
016100081110     c                   return
016200081110     c                   else
016300081110     c                   except    testa
016400081110     c                   end
016500090109      * >>>>>>>
016600081209      * resto nel ciclo finchè....
016700081110     C                   DOU       SqlCod < 0
016800081110      * lettura cursore
016900081110     C/EXEC SQL
017000090427     C+ FETCH NEXT FROM A1 INTO :STP_DatDECO, :STP_DatSCAD, :STP_GIORNI,
017100090427     C+ :dsFGT, :FGT_REC
017200081110     C/END-EXEC
017300081110      *  EoF
017400081110     C                   if        SqlCod = 100
017500081110     c                   Leave
017600081110      *  x errori
017700081110     C                   ElseIf    SqlCod < 0
017800081110     c                   eval      esito = Sqlcode
017900081110     c                   dump(a)
018000081110     c                   leave
018100081110     C                   ENDif
018200090428      *
018300090428      *----------------------------->>>>>
018400090428      * Controlli su Angrafica e su Valorizzazioni
018500090428      *
018600090428     c                   clear                   errore           10
018700090428      *
018800090428      * rileva dall'anagrafica
018900090428     c                   eval      apdTIP = 'A'
019000090428     c                   eval      apdPDR = stpPDR
019100090428     c     Key_APD       chain     fiapd01l
019200090428     c                   if        %Found(fiapd01l)
019300090428      *
019400090428     c                   if        apdATB <> *blank
019500090428     c                   movel     'ANNULLATO'   errore
019600090428     c                   else
019700090428      *
019800090428      *-  Esluso da autofatturazione
019900090428     c                   if        apdPDD = 'S'
020000090428     c                   movel     'AUTOFAT'     errore
020100090428     c                   end
020200090428      *
020300090428     c                   endIF
020400090428      *
020500090428     c                   else
020600090428     c                   movel     'NO_APD'      errore
020700090428     c                   end
020800090428      *
020900090428      *  Controlla sulle valorizzazioni che non ce ne siano oppure
021000090428      *   siano precedenti al 20090101
021100090429      *  SEMPRE CHE non siano in decorrenza fra il 1/4 ed il 31/12
021200090430      *
021300090430     c                   z-add     0             datamaxima        8 0
021400090430     C/EXEC SQL
021500090430     C+ SELECT max(fgtddt) INTO :datamaxima FROM edpab/svFgt00F WHERE
021600090430     C+ fgtpdr = :STPpdr and fgtsml= :STPsml and FGTTSR in ('C', 'R', 'G')
021700090430     C/END-EXEC
021800090430     c                   if        datamaxima < 20090401
021900090430      *
022000090428     c                   if        errore = *blank
022100090428     c     stppdr        chain     fiftt02l
022200090428     c                   if        not %Found(fiftt02l)
022300090428     c                   movel     'NO_in_FTT'   errore
022400090428     c                   end
022500090428      *
022600090428     c                   if        errore = *blank
022700090428      *
022800090428     c                   eval      fttpdr = stpPDR
022900090428     c                   eval      fttfvl = 'C'
023000090428      *
023100090428     c     Key_FTT       setgt     fiftt02l
023200090428     c     Key_FTT       readpe    fiftt02l
023300090428      * Se inferiore al 2009
023400090428     c                   if        not %Eof(fiftt02l)
023500090428      *
023600090428     c                   if        fttddc < 20090101
023700090428     c                   movel     'FTT_<_2009'  errore
023800090428     c                   end
023900090428      *  SE EoF
024000090428     c                   else
024100090428     c                   movel     'FTT_no_C'    errore
024200090428     c                   end
024300090428     c
024400090428     c                   end
024500090428     c                   end
024600090429     c                   end
024700090428      *----------------------------->>>>>
024800090428     c                   movel     errore        error5            5
024900090429      * NONOSTANTE l'errore se la Decorrenza è > del 30 Aprile
025000090429      *   devo tradurli
025100090429     c                   if         stpDDT > 20090430 and errore <> *blank
025200090429     c                   move      '/X'          error5
025300090429     c                   move      ' /EXE'       errore
025400090429     c                   end
025500090429      *
025600090427      *  se sono qui ho letto un record correttamente
025700090427     c   Of              except    testa
025800090427     c                   except    detta
025900090428      *----------------------------->>>>>
026000090428      *
026100090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
026200090428      *
026300090428     c                   if        Errore  = *blank
026400090429     c                              or  stpDDT > 20090430
026500090428      *----------------------------->>>>>
026600090428      *
026700090428     c                   clear                   eseguito          1
026800090428     c                   clear                   verifica          1
026900090427      *
027000081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
027100090423     c                   movel     stpSML        stpSMLPDR         8
027200090109     c                   move      stpPDR        Autista           7 0
027300090423     c                   move      Autista       stpSMLPDR
027400090109      *
027500090109      *  Solo in aggiornamento
027600081110     c                   if        Aggiorna = 'S'
027700090424      *
027800090424      *  Deve scrivere la testata poichè comunuque ha un SOLO periodo
027900090424      *  dal 1/5/2009 al 31/12/2039
028000090109      *    a rottura di simulazione/autista
028100090423      *     salva chiave di rottura
028200090423     c                   if        STPsmlPDR <> savSTPsmlPDR
028300090423     c                   eval      savSTPsmlPDR = STPsmlPDR
028400090423      *
028500090423      *  IMPOSTA la SUERTESTATA
028600090109     c                   clear                   fitgt000
028700090423     c                   z-add     STPpdr        tgtPDR
028800090423     c                   move      STPsml        tgtSML
028900090423     c                   z-add     200           tgtPRG
029000090423     c                   move      STPdiv        tgtDIV
029100090424      **
029200090423      * data decorrenza/scadenza
029300090423     c                   z-add     20090501      tgtDDT
029400090423      *
029500090424      *  QUINDI sempre la data all'infinito 31/12/2039
029600090423     c                   z-add     20391231      tgtDST
029700090423      *
029800090423     c                   z-add     *date         tgtDUV
029900090423     c                   moveL     'Convers'     tgtPRU
030000090401      *
030100090423      * rileva dall'anagrafica
030200090401     c                   eval      apdTIP = 'A'
030300090401     c                   eval      apdPDR = tgtPDR
030400090401     c     Key_APD       chain     fiapd01l
030500090423      *---
030600090401     c                   if        %Found(fiapd01l)
030700090423      *   società fornitore
030800090421     c                   movel     apdcsf        tgtsoc
030900090421     c                   movel     *all'0'       tgtcdf
031000090421     c                   move      apdksc        tgtcdf
031100090421     c                   movel     apdflr        dapdflr
031200090423      *  la massa in KG
031300090421     c                   eval      tgtpkl = §APDMCP
031400090423     c*  i tempi di carico e scarico
031500090421     c                   clear                   ficn08ds
031600090421     c                   eval      i08data= tgtddt
031700090421     c                   eval      i08tip= 'A'
031800090421     c                   eval      i08mcp = tgtpkl
031900090421     c                   call      'FICN08R'
032000090421     c                   parm                    ficn08ds
032100090421     c                   if        o08err =  ' '
032200090421     c                   eval      TGTTMPC = o08tc
032300090421     c                   eval      TGTTMPS = o08ts
032400090421     c                   end
032500090423      *---
032600090401     c                   end
032700090423      *
032800090423      *  prezzo carburante al litro
032900090109     c                   z-add     D§GPIGPI      tgtPCAR
033000090422      * Attenzione:
033100090423      *     se non sono state stampate nel vecchio
033200090423      *        non devono essere convalidate nel nuovo
033300090424      * MA CON QUESTO SPECIFICO SQL potrei avere righe tariffe precedenti l'1/5
033400090424      * già STAMPATE e
033500090424      * TARIFFE decorrenti dal 1/5 non ANCORA STAMPATE......
033600090424      ***
033700090424      * Attenzione:
033800090424      *   C O S A     F A R E  ???????
033900090424      *     Ci sarà un pgm finale che allinea le date stampa x la superTESTATA
034000090424     c                   if        stpDTS > 0
034100090424     c                   z-add     20090501      tgtDTS
034200090424     c                   end
034300090422      *
034400090109     c                   write     fitgt000
034500090109     c                   end
034600090423      *------------------
034700081216      *
034800090424      *  aggiorna i dettagli solo per il progressivo di quelle tariffe
034900090424      *   che decorrevano dal 4/5 poichè avranno il progr. 200 legate alla
035000090424      *  Supertestata
035100090424      *      ma solo se la data coincide con la DATA MAX di Decorrenza
035200090424      *        che nel nostro caso diventa il 1/5
035300090424     c                   if        stpDDT = STP_DATdeco
035400090424      *
035500090423     c                   exsr      FPT_FPD
035600090424      *
035700090424     c                   end
035800090109      *
035900090109      *------------------
036000090424      *   deve solo aggiornare la scadenza al 30/4
036100090424      *     oppure
036200090424      *   la Decorrenza al 1/5 ed il progressivo 200
036300090424     c*************      open      fifgt00f
036400090423      *
036500081218     c     fgt_REC       chain     FIFGT00F
036600081218     c                   if        %Found(FIFGT00F)
036700090112      *
036800090422      *  Fa scadere la testata al 30 apr.2009
036900090424     c                   if            fgtdst = STP_datSCAD
037000090422     c                   eval      fgtDST = 20090430
037100090423     c                   end
037200090424      **
037300090424      *     Ma se decorre oltre il 1/5/2009
037400090424      *        Deve impostare   il 1/5/2009 ed il progressivo 200
037500090112      * e quindi questa diventa la tariffa della supertestata
037600090424     c                   if            fgtddt = STP_datDECO
037700090424     c                   z-add     20090501      fgtDDT
037800090112     c                   z-add     200           fgtPRG
037900090424      *
038000090112     c                   z-add     *date         fgtDUV
038100090423      *
038200090423      * solo se era vuoto anche prima
038300090423     c                   if            stpDTS > 0
038400090424     c                   z-add     20090501      fgtDTS
038500090423     c                   elseif        stpDTS = 0
038600090112     c                   clear                   fgtDTS
038700090423     c                   end
038800090112     c                   clear                   fgtFTR
038900090112     c                   clear                   fgtDTR
039000090423      *
039100090423     c                   end
039200090423      *
039300090423      * aggiorna
039400081218     c                   update    fgtFisico
039500090112      *
039600090423     c                   endIF
039700090112      *
039800090424     c************       close     fifgt00f
039900090109      *
040000090109     c                   endif
040100090109      *------------------
040200090428      *----------------------------->>>>>
040300090428     c                   end
040400090428      *----------------------------->>>>>
040500090428      *
040600090424      *  scrive il dettaglio su un file
040700090424     c                   exsr      wri_DETTA
040800081110      *
040900081110     C                   ENDDO
041000081110     C/EXEC SQL
041100081110     C+ Close A1
041200081110     C/END-EXEC
041300081110     C                   if        Esito < 0
041400081110     c                   return
041500081110     c                   endIF
041600081110     C*
041700081110     C     FINE          TAG
041800081110     c                   except    finest
041900081110     c                   return
042000081209     C*------------------------------------------------------------------------*
042100081209     C* *INZSR - OPERAZIONI INIZIALI
042200081209     C*------------------------------------------------------------------------*
042300081209     C     *INZSR        BEGSR
042400081209      *
042500081209     C     *ENTRY        PLIST
042600081209     C                   PARM                    AGGIORNA          1
042700081209     C*
042800090109      * duplica le tariffe FGT
042900081209     C     fgt01K        kLIST
043000081211     C                   kfld                    stpPDR
043100081211     C                   kfld                    stpSML
043200081211     C                   kfld                    stpTSR
043300081211     C                   kfld                    stpCTR
043400081211     C                   kfld                    stpPRG
043500090109      *
043600090109      * duplica le tariffe FPT e FPD
043700090109     c     Key_FPT       KLIST
043800090109     c                   kfld                    stpPDR
043900090109     c                   kfld                    stpSML
044000090109     c                   kfld                    stpTSR
044100090109     c                   kfld                    stpCTR
044200090109     c                   kfld                    stpPRG
044300081209     C*
044400090401     c     Key_APD       KLIST
044500090401     c                   kfld                    apdTIP
044600090401     c                   kfld                    apdPDR
044700090401     C*
044800090428     c     Key_FTT       KLIST
044900090428     c                   kfld                    FTTPDR
045000090428     c                   kfld                    FTTFVL
045100090428     C*
045200081209     c                   clear                   esito
045300081209      *
045400081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
045500081209     C                   clear                   UDATE_8           8 0
045600081209     C                   MOVEL     *DATE         UDATE_8
045700090109      *
045800090109      * reperisce il prezzo di riferimento del carburante
045900090109     c                   call      'FICN05R'
046000090109     c                   parm                    ficn05DS
046100090109      *
046200090109     c                   iF        o05err = *blank
046300090109     c                   eval      dGPI = o05GPI
046400090109     c                   enD
046500090109      *
046600081209     c                   endSR
046700090109      *---------------------------------------------------------------*
046800090109      *  Scrive Testate e Dettaglio
046900090109      *---------------------------------------------------------------*
047000090423     c     FPT_FPD       begSR
047100090109     C*
047200090109     c     Key_FPT       setll     fifpt01L
047300090109     c     Key_FPT       reade     fifpt01L
047400090109     c                   Dow       not %EoF(fifpt01l)
047500090423     C*
047600090424      * deve solo aggiornare il progressivo
047700090424     c                   z-add     200           fptPRG
047800090423     c                   update    fifpt000
047900090423      *
048000090109     c     Key_FPT       reade     fifpt01L
048100090109     c                   endDo
048200090109     C*
048300090109     c     Key_FPT       setll     fifpd01L
048400090109     c     Key_FPT       reade     fifpd01L
048500090109     c                   Dow       not %EoF(fifpd01l)
048600090423      *
048700090424      * deve solo aggiornare il progressivo
048800090109     c                   z-add     200           fpdPRG
048900090423     c                   update    fifpd000
049000090423      *
049100090109     c     Key_FPT       reade     fifpd01L
049200090109     c                   endDo
049300090109     C*
049400090109     c                   endSR
049500081211     C*---------------------------------------------------------------*
049600090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
049700090424      *---------------------------------------------------------------*
049800090424     c     WRI_DETTA     begSR
049900090424     C*
050000090424     c                   eval      WSFSML  =  stpSML
050100090424     c                   eval      WSFPDR  =  stpPDR
050200090424     c                   eval      WSFTSR  =  stpTSR
050300090424     c                   eval      WSFCTR  =  stpCTR
050400090424     c                   eval      WSFPRG  = stpPRG
050500090424     c                   eval      WSFDDT  = stpDDT
050600090424     c                   eval      WSFDST  = stpDST
050700090424     c                   eval      WSFDTS  = stpDTS
050800090424     c                   eval      WSFDCN  = stpDCN
050900090424     c                   eval      WSFREC  = FGT_REC
051000090424     c                   eval      WSFDECOR= stp_DATdeco
051100090424     c                   eval      WSFSCAD = stp_DATscad
051200090424     c                   eval      WSFPROGR= 'TNVRA18R'
051300090428     c**
051400090428     c                   eval      WSFNOTE = Errore
051500090424     c**
051600090424     c                   write     WSFGT000
051700090424     C*
051800090424     c                   endSR
051900090424     C*---------------------------------------------------------------*
052000081110     OQSYSPRT   E            TESTA            02
052100081110     O                                           16 'Simul'
052200081110     O                                           25 'Autista'
052300081110     O                                           30 'TSR'
052400081110     O                                           38 'Tariffa'
052500081110     O                                           45 'Progr'
052600081110     O                                           55 'Data_Dec'
052700081110     O                                           65 'Data_Sca'
052800081110     O                                           75 'Data_Stp'
052900081110     O                                           85 'Data_Cnv'
053000081211     O                                          115 'N_Rec_FGT'
053100090424     O                                          125 'Data_Min'
053200081110     O          E            DETta       1
053300081110     O                       stpSML              14
053400081110     O                       stpPDR        z     25
053500081110     O                       stpTSR              29
053600081110     O                       stpCTR              36
053700081110     O                       stpPRG        z     43
053800081110     O                       stpDDT        z     55
053900081110     O                       stpDST        z     65
054000081110     O                       stpDTS        z     75
054100081110     O                       stpDCN        z     85
054200081211     o                       FGT_REC       z    115
054300090424     O                       stp_DATscad   z    125
054400090428     O                       ERROR5             132
054500081110      ****
054600081110     o          E            FINEST      2
054700090424     o                                           30 '*** TNVRA18R ***'
054800090102     o                                           59 '***   FINE  STAMPA  ***'
054900090109     o                                          + 9 '***  SUPER TARIFFA  ***'
