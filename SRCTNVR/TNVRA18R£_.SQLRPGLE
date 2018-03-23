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
001600090401      *
001700090424     FwxFGT00S  o  a E             DISK
001800081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001900081211      * -------------------------------------------------------------
002000090421     d dapdflr       e ds
002100090421     dficn08ds       e ds
002200081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002300081110     d                                     prefix(STP:3)
002400081110     D esito           s             10i 0
002500081110     D FGT_REC         s              9s 0
002600090424     D STP_DATSCAD     s              8s 0
002700090424     D STP_DATdeco     s              8s 0
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
005000090424      ** Si divide ancora in 2 il 2° PROBLEMA:
005100090424      **  a) Le Tariffe che hanno fatto scadere il 3/5 e Decorrere il 4/5
005200090424      **     possono essere fatte ricondurre ad un solo Periodo NUOVO
005300090424      **    ossia
005400090424      **     occorre far scadere il 30/4 quelle del 3/5 e
005500090424      **             far attivare il 1/5 quelle del 4/5
005600090424      **
005700090424      **    ATTENZIONE PERO' sono quelle che hanno 1 solo giorno di DIFFERENZA
005800090424      **      fra DATAmaxDECORRENZA e DATAminSCADENZA  con
005900090424      **      scadenza ENTRO la 1°settimana di MAGGIO
006000090424      **
006100090424      **  b)  RIMANGONO QUINDI le vere tariffe con periodi diversi
006200090424      **     che hanno DATAminSCADENZA oltre la 1°SETTIMANA di Maggio
006300090424      **    occorre x queste generare
006400090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
006500090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
006600090423      **    decorrenza del progressivo 201 e così via......
006700090424      **
006800090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
006900090423      **   devono risultare
007000090423      **   NON CONFERMATE
007100090423      **
007200090423     C*=================================================================
007300090423      **  Queste sono le tariffe da CONVERTIRE creando le SUPERTESTATE
007400090423     C*=================================================================
007500090424      ** 2° PROBLEMA  soluzione (a)
007600090424      *
007700090424      *  esempio:
007800090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA
007900090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.
008000090424      *                         PADR                          TAR.         TAR.
008100090424      *
008200090424      *    190036                 C    300         EUR    2009/03/23  2009/05/03
008300090424      *    190036                 C    300    1    EUR    2009/05/04  2039/12/31
008400090424      *    190036                 G    999         EUR    2009/03/23  2039/12/31
008500090424      *    190036                 R    300         EUR    2009/03/23  2009/05/03
008600090424      *    190036                 R    300    1    EUR    2009/05/04  2039/12/31
008700090424      *
008800090424      *    190037                 C    300         EUR    2009/03/23  2009/05/03
008900090424      *    190037                 C    300    1    EUR    2009/05/04  2039/12/31
009000090424      *    190037                 G    999         EUR    2009/03/23  2039/12/31
009100090424      *    190037                 R    300         EUR    2009/03/23  2009/05/03
009200090424      *    190037                 R    300    1    EUR    2009/05/04  2039/12/31
009300090424      *
009400090424      *    190038                 C    300         EUR    2009/03/23  2009/05/03
009500090424      *    190038                 C    300    1    EUR    2009/05/04  2039/12/31
009600090424      *    190038                 G    999         EUR    2009/03/23  2009/05/03
009700090424      *    190038                 G    999    1    EUR    2009/05/04  2039/12/31
009800090424      *    190038                 R    300         EUR    2009/03/23  2009/05/03
009900090424      *    190038                 R    300    1    EUR    2009/05/04  2039/12/31
010000090424      *
010100090422     C/EXEC SQL DECLARE A1 CURSOR FOR
010200090424     C+  with AUTSIMULA (AUTISTA, SIMULA, DATAMAX) as (
010300090424     C+  select fgtpdr, fgtsml, max(fgtddt) from
010400090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
010500090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
010600090424     C+  group BY FGTpdr, FGTsml
010700090424     C+  having max(fgtddt) >= 20090501)
010800090424     C+     Select * FROM (
010900090424     C+  SELECT DATAMAXDECOR, DATAMINSCAD,
011000090424     C+  (DATAMAXDECOR - DATAMINSCAD) as Giorni,
011100090424     C+  a.* , rrn(a) FROM
011200090424     C+  fifgt00f as a join AUTSIMULA on a.fgtpdr=Autista
011300090424     C+  and a.fgtsml=SIMULA
011400090424     C+  join (
011500090424     C+  select fgtpdr, fgtsml, min(fgtdst) as DATAMINSCAD,
011600090424     C+  max(fgtddt) as DATAMAXDECOR
011700090424     C+  from
011800090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
011900090424     C+  and fgtddt < 20100101 and
012000090424     C+  fgtdst >= 20090501
012100090424     C+  group BY FGTpdr, FGTsml
012200090424     C+  having min(fgtdst) <= 20090509
012300090424     C+  )  as SCADENZA
012400090424     C+   on a.fgtpdr=SCADENZA.fgtpdr and a.fgtsml=SCADENZA.fgtsml
012500090424     C+  WHERE  fgttsr in ('C', 'R', 'G')
012600090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
012700090424     C+  and a.fgtsml concat digits(a.fgtpdr) in
012800090424     C+  (
012900090424     C+  select fgtsml concat digits(fgtpdr) from
013000090424     C+  (
013100090424     C+  select FGTSML, FGTPDR, count(*) from
013200090424     C+  (
013300090424     C+  SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
013400090424     C+  fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
013500090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
013600090424     C+  group BY FGTSML, FGTPDR, FGTDDT, FGTDST
013700090424     C+  ) as b
013800090424     C+  group BY FGTSML, FGTPDR
013900090424     C+  having count(*) > 1
014000090424     C+  ) as unPERIODO
014100090424     C+  )
014200090424     C+  )as DIFFE_GIORNI
014300090424     C+  where GIORNI = 1
014400090424     C+  ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
014500090422     C/END-EXEC
014600081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
014700081110     c                   if        sqlCODE < 0
014800081110     c                   eval      esito = Sqlcode
014900081110     c                   dump(a)
015000081110     c                   return
015100081110     c                   end
015200081110     C/EXEC SQL
015300081110     C+ OPEN A1
015400081110     C/END-EXEC
015500081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
015600081110     c                   if        sqlCODE < 0
015700081110     c                   eval      esito = Sqlcode
015800081110     c                   dump(a)
015900081110     c                   return
016000081110     c                   else
016100081110     c                   except    testa
016200081110     c                   end
016300090109      * >>>>>>>
016400081209      * resto nel ciclo finchè....
016500081110     C                   DOU       SqlCod < 0
016600081110      * lettura cursore
016700081110     C/EXEC SQL
016800090424     C+ FETCH NEXT FROM A1 INTO :STP_DatDECO, :STP_DatSCAD, :dsFGT,
016900090424     C+ :FGT_REC
017000081110     C/END-EXEC
017100081110      *  EoF
017200081110     C                   if        SqlCod = 100
017300081110     c                   Leave
017400081110      *  x errori
017500081110     C                   ElseIf    SqlCod < 0
017600081110     c                   eval      esito = Sqlcode
017700081110     c                   dump(a)
017800081110     c                   leave
017900081110     C                   ENDif
018000081110      *
018100090109     c                   clear                   eseguito          1
018200090109     c                   clear                   verifica          1
018300090102     c                   clear                   errore           10
018400081110      *
018500090427      *  se sono qui ho letto un record correttamente
018600090427     c   Of              except    testa
018700090427     c                   except    detta
018800090427      *
018900081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
019000090423     c                   movel     stpSML        stpSMLPDR         8
019100090109     c                   move      stpPDR        Autista           7 0
019200090423     c                   move      Autista       stpSMLPDR
019300090109      *
019400090109      *  Solo in aggiornamento
019500081110     c                   if        Aggiorna = 'S'
019600090424      *
019700090424      *  Deve scrivere la testata poichè comunuque ha un SOLO periodo
019800090424      *  dal 1/5/2009 al 31/12/2039
019900090109      *    a rottura di simulazione/autista
020000090423      *     salva chiave di rottura
020100090423     c                   if        STPsmlPDR <> savSTPsmlPDR
020200090423     c                   eval      savSTPsmlPDR = STPsmlPDR
020300090423      *
020400090423      *  IMPOSTA la SUERTESTATA
020500090109     c                   clear                   fitgt000
020600090423     c                   z-add     STPpdr        tgtPDR
020700090423     c                   move      STPsml        tgtSML
020800090423     c                   z-add     200           tgtPRG
020900090423     c                   move      STPdiv        tgtDIV
021000090424      **
021100090423      * data decorrenza/scadenza
021200090423     c                   z-add     20090501      tgtDDT
021300090423      *
021400090424      *  QUINDI sempre la data all'infinito 31/12/2039
021500090423     c                   z-add     20391231      tgtDST
021600090423      *
021700090423     c                   z-add     *date         tgtDUV
021800090423     c                   moveL     'Convers'     tgtPRU
021900090401      *
022000090423      * rileva dall'anagrafica
022100090401     c                   eval      apdTIP = 'A'
022200090401     c                   eval      apdPDR = tgtPDR
022300090401     c     Key_APD       chain     fiapd01l
022400090423      *---
022500090401     c                   if        %Found(fiapd01l)
022600090423      *   società fornitore
022700090421     c                   movel     apdcsf        tgtsoc
022800090421     c                   movel     *all'0'       tgtcdf
022900090421     c                   move      apdksc        tgtcdf
023000090421     c                   movel     apdflr        dapdflr
023100090423      *  la massa in KG
023200090421     c                   eval      tgtpkl = §APDMCP
023300090423     c*  i tempi di carico e scarico
023400090421     c                   clear                   ficn08ds
023500090421     c                   eval      i08data= tgtddt
023600090421     c                   eval      i08tip= 'A'
023700090421     c                   eval      i08mcp = tgtpkl
023800090421     c                   call      'FICN08R'
023900090421     c                   parm                    ficn08ds
024000090421     c                   if        o08err =  ' '
024100090421     c                   eval      TGTTMPC = o08tc
024200090421     c                   eval      TGTTMPS = o08ts
024300090421     c                   end
024400090423      *---
024500090401     c                   end
024600090423      *
024700090423      *  prezzo carburante al litro
024800090109     c                   z-add     D§GPIGPI      tgtPCAR
024900090422      * Attenzione:
025000090423      *     se non sono state stampate nel vecchio
025100090423      *        non devono essere convalidate nel nuovo
025200090424      * MA CON QUESTO SPECIFICO SQL potrei avere righe tariffe precedenti l'1/5
025300090424      * già STAMPATE e
025400090424      * TARIFFE decorrenti dal 1/5 non ANCORA STAMPATE......
025500090424      ***
025600090424      * Attenzione:
025700090424      *   C O S A     F A R E  ???????
025800090424      *     Ci sarà un pgm finale che allinea le date stampa x la superTESTATA
025900090424     c                   if        stpDTS > 0
026000090424     c                   z-add     20090501      tgtDTS
026100090424     c                   end
026200090422      *
026300090109     c                   write     fitgt000
026400090109     c                   end
026500090423      *------------------
026600081216      *
026700090424      *  aggiorna i dettagli solo per il progressivo di quelle tariffe
026800090424      *   che decorrevano dal 4/5 poichè avranno il progr. 200 legate alla
026900090424      *  Supertestata
027000090424      *      ma solo se la data coincide con la DATA MAX di Decorrenza
027100090424      *        che nel nostro caso diventa il 1/5
027200090424     c                   if        stpDDT = STP_DATdeco
027300090424      *
027400090423     c                   exsr      FPT_FPD
027500090424      *
027600090424     c                   end
027700090109      *
027800090109      *------------------
027900090424      *   deve solo aggiornare la scadenza al 30/4
028000090424      *     oppure
028100090424      *   la Decorrenza al 1/5 ed il progressivo 200
028200090424     c*************      open      fifgt00f
028300090423      *
028400081218     c     fgt_REC       chain     FIFGT00F
028500081218     c                   if        %Found(FIFGT00F)
028600090112      *
028700090422      *  Fa scadere la testata al 30 apr.2009
028800090424     c                   if            fgtdst = STP_datSCAD
028900090422     c                   eval      fgtDST = 20090430
029000090423     c                   end
029100090424      **
029200090424      *     Ma se decorre oltre il 1/5/2009
029300090424      *        Deve impostare   il 1/5/2009 ed il progressivo 200
029400090112      * e quindi questa diventa la tariffa della supertestata
029500090424     c                   if            fgtddt = STP_datDECO
029600090424     c                   z-add     20090501      fgtDDT
029700090112     c                   z-add     200           fgtPRG
029800090424      *
029900090112     c                   z-add     *date         fgtDUV
030000090423      *
030100090423      * solo se era vuoto anche prima
030200090423     c                   if            stpDTS > 0
030300090424     c                   z-add     20090501      fgtDTS
030400090423     c                   elseif        stpDTS = 0
030500090112     c                   clear                   fgtDTS
030600090423     c                   end
030700090112     c                   clear                   fgtFTR
030800090112     c                   clear                   fgtDTR
030900090423      *
031000090423     c                   end
031100090423      *
031200090423      * aggiorna
031300081218     c                   update    fgtFisico
031400090112      *
031500090423     c                   endIF
031600090112      *
031700090424     c************       close     fifgt00f
031800090109      *
031900090109     c                   endif
032000090109      *------------------
032100090424      *  scrive il dettaglio su un file
032200090424     c                   exsr      wri_DETTA
032300081110      *
032400081110     C                   ENDDO
032500081110     C/EXEC SQL
032600081110     C+ Close A1
032700081110     C/END-EXEC
032800081110     C                   if        Esito < 0
032900081110     c                   return
033000081110     c                   endIF
033100081110     C*
033200081110     C     FINE          TAG
033300081110     c                   except    finest
033400081110     c                   return
033500081209     C*------------------------------------------------------------------------*
033600081209     C* *INZSR - OPERAZIONI INIZIALI
033700081209     C*------------------------------------------------------------------------*
033800081209     C     *INZSR        BEGSR
033900081209      *
034000081209     C     *ENTRY        PLIST
034100081209     C                   PARM                    AGGIORNA          1
034200081209     C*
034300090109      * duplica le tariffe FGT
034400081209     C     fgt01K        kLIST
034500081211     C                   kfld                    stpPDR
034600081211     C                   kfld                    stpSML
034700081211     C                   kfld                    stpTSR
034800081211     C                   kfld                    stpCTR
034900081211     C                   kfld                    stpPRG
035000090109      *
035100090109      * duplica le tariffe FPT e FPD
035200090109     c     Key_FPT       KLIST
035300090109     c                   kfld                    stpPDR
035400090109     c                   kfld                    stpSML
035500090109     c                   kfld                    stpTSR
035600090109     c                   kfld                    stpCTR
035700090109     c                   kfld                    stpPRG
035800081209     C*
035900090401     c     Key_APD       KLIST
036000090401     c                   kfld                    apdTIP
036100090401     c                   kfld                    apdPDR
036200090401     C*
036300081209     c                   clear                   esito
036400081209      *
036500081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
036600081209     C                   clear                   UDATE_8           8 0
036700081209     C                   MOVEL     *DATE         UDATE_8
036800090109      *
036900090109      * reperisce il prezzo di riferimento del carburante
037000090109     c                   call      'FICN05R'
037100090109     c                   parm                    ficn05DS
037200090109      *
037300090109     c                   iF        o05err = *blank
037400090109     c                   eval      dGPI = o05GPI
037500090109     c                   enD
037600090109      *
037700081209     c                   endSR
037800090109      *---------------------------------------------------------------*
037900090109      *  Scrive Testate e Dettaglio
038000090109      *---------------------------------------------------------------*
038100090423     c     FPT_FPD       begSR
038200090109     C*
038300090109     c     Key_FPT       setll     fifpt01L
038400090109     c     Key_FPT       reade     fifpt01L
038500090109     c                   Dow       not %EoF(fifpt01l)
038600090423     C*
038700090424      * deve solo aggiornare il progressivo
038800090424     c                   z-add     200           fptPRG
038900090423     c                   update    fifpt000
039000090423      *
039100090109     c     Key_FPT       reade     fifpt01L
039200090109     c                   endDo
039300090109     C*
039400090109     c     Key_FPT       setll     fifpd01L
039500090109     c     Key_FPT       reade     fifpd01L
039600090109     c                   Dow       not %EoF(fifpd01l)
039700090423      *
039800090424      * deve solo aggiornare il progressivo
039900090109     c                   z-add     200           fpdPRG
040000090423     c                   update    fifpd000
040100090423      *
040200090109     c     Key_FPT       reade     fifpd01L
040300090109     c                   endDo
040400090109     C*
040500090109     c                   endSR
040600081211     C*---------------------------------------------------------------*
040700090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
040800090424      *---------------------------------------------------------------*
040900090424     c     WRI_DETTA     begSR
041000090424     C*
041100090424     c                   eval      WSFSML  =  stpSML
041200090424     c                   eval      WSFPDR  =  stpPDR
041300090424     c                   eval      WSFTSR  =  stpTSR
041400090424     c                   eval      WSFCTR  =  stpCTR
041500090424     c                   eval      WSFPRG  = stpPRG
041600090424     c                   eval      WSFDDT  = stpDDT
041700090424     c                   eval      WSFDST  = stpDST
041800090424     c                   eval      WSFDTS  = stpDTS
041900090424     c                   eval      WSFDCN  = stpDCN
042000090424     c                   eval      WSFREC  = FGT_REC
042100090424     c                   eval      WSFDECOR= stp_DATdeco
042200090424     c                   eval      WSFSCAD = stp_DATscad
042300090424     c                   eval      WSFPROGR= 'TNVRA18R'
042400090424     c**
042500090424     c                   write     WSFGT000
042600090424     C*
042700090424     c                   endSR
042800090424     C*---------------------------------------------------------------*
042900081110     OQSYSPRT   E            TESTA            02
043000081110     O                                           16 'Simul'
043100081110     O                                           25 'Autista'
043200081110     O                                           30 'TSR'
043300081110     O                                           38 'Tariffa'
043400081110     O                                           45 'Progr'
043500081110     O                                           55 'Data_Dec'
043600081110     O                                           65 'Data_Sca'
043700081110     O                                           75 'Data_Stp'
043800081110     O                                           85 'Data_Cnv'
043900081211     O                                          115 'N_Rec_FGT'
044000090424     O                                          125 'Data_Min'
044100081110     O          E            DETta       1
044200081110     O                       stpSML              14
044300081110     O                       stpPDR        z     25
044400081110     O                       stpTSR              29
044500081110     O                       stpCTR              36
044600081110     O                       stpPRG        z     43
044700081110     O                       stpDDT        z     55
044800081110     O                       stpDST        z     65
044900081110     O                       stpDTS        z     75
045000081110     O                       stpDCN        z     85
045100081211     o                       FGT_REC       z    115
045200090424     O                       stp_DATscad   z    125
045300081110      ****
045400081110     o          E            FINEST      2
045500090424     o                                           30 '*** TNVRA18R ***'
045600090102     o                                           59 '***   FINE  STAMPA  ***'
045700090109     o                                          + 9 '***  SUPER TARIFFA  ***'
