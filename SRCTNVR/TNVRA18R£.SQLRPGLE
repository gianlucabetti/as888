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
002800090427     D STP_GIORNI      s              8s 0
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
005100090424      ** Si divide ancora in 2 il 2° PROBLEMA:
005200090424      **  a) Le Tariffe che hanno fatto scadere il 3/5 e Decorrere il 4/5
005300090424      **     possono essere fatte ricondurre ad un solo Periodo NUOVO
005400090424      **    ossia
005500090424      **     occorre far scadere il 30/4 quelle del 3/5 e
005600090424      **             far attivare il 1/5 quelle del 4/5
005700090424      **
005800090424      **    ATTENZIONE PERO' sono quelle che hanno 1 solo giorno di DIFFERENZA
005900090424      **      fra DATAmaxDECORRENZA e DATAminSCADENZA  con
006000090424      **      scadenza ENTRO la 1°settimana di MAGGIO
006100090424      **
006200090424      **  b)  RIMANGONO QUINDI le vere tariffe con periodi diversi
006300090424      **     che hanno DATAminSCADENZA oltre la 1°SETTIMANA di Maggio
006400090424      **    occorre x queste generare
006500090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
006600090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
006700090423      **    decorrenza del progressivo 201 e così via......
006800090424      **
006900090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
007000090423      **   devono risultare
007100090423      **   NON CONFERMATE
007200090423      **
007300090423     C*=================================================================
007400090423      **  Queste sono le tariffe da CONVERTIRE creando le SUPERTESTATE
007500090423     C*=================================================================
007600090424      ** 2° PROBLEMA  soluzione (a)
007700090424      *
007800090424      *  esempio:
007900090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA
008000090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.
008100090424      *                         PADR                          TAR.         TAR.
008200090424      *
008300090424      *    190036                 C    300         EUR    2009/03/23  2009/05/03
008400090424      *    190036                 C    300    1    EUR    2009/05/04  2039/12/31
008500090424      *    190036                 G    999         EUR    2009/03/23  2039/12/31
008600090424      *    190036                 R    300         EUR    2009/03/23  2009/05/03
008700090424      *    190036                 R    300    1    EUR    2009/05/04  2039/12/31
008800090424      *
008900090424      *    190037                 C    300         EUR    2009/03/23  2009/05/03
009000090424      *    190037                 C    300    1    EUR    2009/05/04  2039/12/31
009100090424      *    190037                 G    999         EUR    2009/03/23  2039/12/31
009200090424      *    190037                 R    300         EUR    2009/03/23  2009/05/03
009300090424      *    190037                 R    300    1    EUR    2009/05/04  2039/12/31
009400090424      *
009500090424      *    190038                 C    300         EUR    2009/03/23  2009/05/03
009600090424      *    190038                 C    300    1    EUR    2009/05/04  2039/12/31
009700090424      *    190038                 G    999         EUR    2009/03/23  2009/05/03
009800090424      *    190038                 G    999    1    EUR    2009/05/04  2039/12/31
009900090424      *    190038                 R    300         EUR    2009/03/23  2009/05/03
010000090424      *    190038                 R    300    1    EUR    2009/05/04  2039/12/31
010100090424      *
010200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
010300090424     C+  with AUTSIMULA (AUTISTA, SIMULA, DATAMAX) as (
010400090424     C+  select fgtpdr, fgtsml, max(fgtddt) from
010500090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
010600090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
010700090424     C+  group BY FGTpdr, FGTsml
010800090424     C+  having max(fgtddt) >= 20090501)
010900090424     C+     Select * FROM (
011000090424     C+  SELECT DATAMAXDECOR, DATAMINSCAD,
011100090424     C+  (DATAMAXDECOR - DATAMINSCAD) as Giorni,
011200090424     C+  a.* , rrn(a) FROM
011300090424     C+  fifgt00f as a join AUTSIMULA on a.fgtpdr=Autista
011400090424     C+  and a.fgtsml=SIMULA
011500090424     C+  join (
011600090424     C+  select fgtpdr, fgtsml, min(fgtdst) as DATAMINSCAD,
011700090424     C+  max(fgtddt) as DATAMAXDECOR
011800090424     C+  from
011900090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
012000090424     C+  and fgtddt < 20100101 and
012100090424     C+  fgtdst >= 20090501
012200090424     C+  group BY FGTpdr, FGTsml
012300090424     C+  having min(fgtdst) <= 20090509
012400090424     C+  )  as SCADENZA
012500090424     C+   on a.fgtpdr=SCADENZA.fgtpdr and a.fgtsml=SCADENZA.fgtsml
012600090424     C+  WHERE  fgttsr in ('C', 'R', 'G')
012700090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
012800090424     C+  and a.fgtsml concat digits(a.fgtpdr) in
012900090424     C+  (
013000090424     C+  select fgtsml concat digits(fgtpdr) from
013100090424     C+  (
013200090424     C+  select FGTSML, FGTPDR, count(*) from
013300090424     C+  (
013400090424     C+  SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
013500090424     C+  fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
013600090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
013700090424     C+  group BY FGTSML, FGTPDR, FGTDDT, FGTDST
013800090424     C+  ) as b
013900090424     C+  group BY FGTSML, FGTPDR
014000090424     C+  having count(*) > 1
014100090424     C+  ) as unPERIODO
014200090424     C+  )
014300090424     C+  )as DIFFE_GIORNI
014400090424     C+  where GIORNI = 1
014500090424     C+  ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
014600090422     C/END-EXEC
014700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
014800081110     c                   if        sqlCODE < 0
014900081110     c                   eval      esito = Sqlcode
015000081110     c                   dump(a)
015100081110     c                   return
015200081110     c                   end
015300081110     C/EXEC SQL
015400081110     C+ OPEN A1
015500081110     C/END-EXEC
015600081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
015700081110     c                   if        sqlCODE < 0
015800081110     c                   eval      esito = Sqlcode
015900081110     c                   dump(a)
016000081110     c                   return
016100081110     c                   else
016200081110     c                   except    testa
016300081110     c                   end
016400090109      * >>>>>>>
016500081209      * resto nel ciclo finchè....
016600081110     C                   DOU       SqlCod < 0
016700081110      * lettura cursore
016800081110     C/EXEC SQL
016900090427     C+ FETCH NEXT FROM A1 INTO :STP_DatDECO, :STP_DatSCAD, :STP_GIORNI,
017000090427     C+ :dsFGT, :FGT_REC
017100081110     C/END-EXEC
017200081110      *  EoF
017300081110     C                   if        SqlCod = 100
017400081110     c                   Leave
017500081110      *  x errori
017600081110     C                   ElseIf    SqlCod < 0
017700081110     c                   eval      esito = Sqlcode
017800081110     c                   dump(a)
017900081110     c                   leave
018000081110     C                   ENDif
018100081110      *
018200090109     c                   clear                   eseguito          1
018300090109     c                   clear                   verifica          1
018400090102     c                   clear                   errore           10
018500081110      *
018600090427      *  se sono qui ho letto un record correttamente
018700090427     c   Of              except    testa
018800090427     c                   except    detta
018900090427      *
019000081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
019100090423     c                   movel     stpSML        stpSMLPDR         8
019200090109     c                   move      stpPDR        Autista           7 0
019300090423     c                   move      Autista       stpSMLPDR
019400090109      *
019500090109      *  Solo in aggiornamento
019600081110     c                   if        Aggiorna = 'S'
019700090424      *
019800090424      *  Deve scrivere la testata poichè comunuque ha un SOLO periodo
019900090424      *  dal 1/5/2009 al 31/12/2039
020000090109      *    a rottura di simulazione/autista
020100090423      *     salva chiave di rottura
020200090423     c                   if        STPsmlPDR <> savSTPsmlPDR
020300090423     c                   eval      savSTPsmlPDR = STPsmlPDR
020400090423      *
020500090423      *  IMPOSTA la SUERTESTATA
020600090109     c                   clear                   fitgt000
020700090423     c                   z-add     STPpdr        tgtPDR
020800090423     c                   move      STPsml        tgtSML
020900090423     c                   z-add     200           tgtPRG
021000090423     c                   move      STPdiv        tgtDIV
021100090424      **
021200090423      * data decorrenza/scadenza
021300090423     c                   z-add     20090501      tgtDDT
021400090423      *
021500090424      *  QUINDI sempre la data all'infinito 31/12/2039
021600090423     c                   z-add     20391231      tgtDST
021700090423      *
021800090423     c                   z-add     *date         tgtDUV
021900090423     c                   moveL     'Convers'     tgtPRU
022000090401      *
022100090423      * rileva dall'anagrafica
022200090401     c                   eval      apdTIP = 'A'
022300090401     c                   eval      apdPDR = tgtPDR
022400090401     c     Key_APD       chain     fiapd01l
022500090423      *---
022600090401     c                   if        %Found(fiapd01l)
022700090423      *   società fornitore
022800090421     c                   movel     apdcsf        tgtsoc
022900090421     c                   movel     *all'0'       tgtcdf
023000090421     c                   move      apdksc        tgtcdf
023100090421     c                   movel     apdflr        dapdflr
023200090423      *  la massa in KG
023300090421     c                   eval      tgtpkl = §APDMCP
023400090423     c*  i tempi di carico e scarico
023500090421     c                   clear                   ficn08ds
023600090421     c                   eval      i08data= tgtddt
023700090421     c                   eval      i08tip= 'A'
023800090421     c                   eval      i08mcp = tgtpkl
023900090421     c                   call      'FICN08R'
024000090421     c                   parm                    ficn08ds
024100090421     c                   if        o08err =  ' '
024200090421     c                   eval      TGTTMPC = o08tc
024300090421     c                   eval      TGTTMPS = o08ts
024400090421     c                   end
024500090423      *---
024600090401     c                   end
024700090423      *
024800090423      *  prezzo carburante al litro
024900090109     c                   z-add     D§GPIGPI      tgtPCAR
025000090422      * Attenzione:
025100090423      *     se non sono state stampate nel vecchio
025200090423      *        non devono essere convalidate nel nuovo
025300090424      * MA CON QUESTO SPECIFICO SQL potrei avere righe tariffe precedenti l'1/5
025400090424      * già STAMPATE e
025500090424      * TARIFFE decorrenti dal 1/5 non ANCORA STAMPATE......
025600090424      ***
025700090424      * Attenzione:
025800090424      *   C O S A     F A R E  ???????
025900090424      *     Ci sarà un pgm finale che allinea le date stampa x la superTESTATA
026000090424     c                   if        stpDTS > 0
026100090424     c                   z-add     20090501      tgtDTS
026200090424     c                   end
026300090422      *
026400090109     c                   write     fitgt000
026500090109     c                   end
026600090423      *------------------
026700081216      *
026800090424      *  aggiorna i dettagli solo per il progressivo di quelle tariffe
026900090424      *   che decorrevano dal 4/5 poichè avranno il progr. 200 legate alla
027000090424      *  Supertestata
027100090424      *      ma solo se la data coincide con la DATA MAX di Decorrenza
027200090424      *        che nel nostro caso diventa il 1/5
027300090424     c                   if        stpDDT = STP_DATdeco
027400090424      *
027500090423     c                   exsr      FPT_FPD
027600090424      *
027700090424     c                   end
027800090109      *
027900090109      *------------------
028000090424      *   deve solo aggiornare la scadenza al 30/4
028100090424      *     oppure
028200090424      *   la Decorrenza al 1/5 ed il progressivo 200
028300090424     c*************      open      fifgt00f
028400090423      *
028500081218     c     fgt_REC       chain     FIFGT00F
028600081218     c                   if        %Found(FIFGT00F)
028700090112      *
028800090422      *  Fa scadere la testata al 30 apr.2009
028900090424     c                   if            fgtdst = STP_datSCAD
029000090422     c                   eval      fgtDST = 20090430
029100090423     c                   end
029200090424      **
029300090424      *     Ma se decorre oltre il 1/5/2009
029400090424      *        Deve impostare   il 1/5/2009 ed il progressivo 200
029500090112      * e quindi questa diventa la tariffa della supertestata
029600090424     c                   if            fgtddt = STP_datDECO
029700090424     c                   z-add     20090501      fgtDDT
029800090112     c                   z-add     200           fgtPRG
029900090424      *
030000090112     c                   z-add     *date         fgtDUV
030100090423      *
030200090423      * solo se era vuoto anche prima
030300090423     c                   if            stpDTS > 0
030400090424     c                   z-add     20090501      fgtDTS
030500090423     c                   elseif        stpDTS = 0
030600090112     c                   clear                   fgtDTS
030700090423     c                   end
030800090112     c                   clear                   fgtFTR
030900090112     c                   clear                   fgtDTR
031000090423      *
031100090423     c                   end
031200090423      *
031300090423      * aggiorna
031400081218     c                   update    fgtFisico
031500090112      *
031600090423     c                   endIF
031700090112      *
031800090424     c************       close     fifgt00f
031900090109      *
032000090109     c                   endif
032100090109      *------------------
032200090424      *  scrive il dettaglio su un file
032300090424     c                   exsr      wri_DETTA
032400081110      *
032500081110     C                   ENDDO
032600081110     C/EXEC SQL
032700081110     C+ Close A1
032800081110     C/END-EXEC
032900081110     C                   if        Esito < 0
033000081110     c                   return
033100081110     c                   endIF
033200081110     C*
033300081110     C     FINE          TAG
033400081110     c                   except    finest
033500081110     c                   return
033600081209     C*------------------------------------------------------------------------*
033700081209     C* *INZSR - OPERAZIONI INIZIALI
033800081209     C*------------------------------------------------------------------------*
033900081209     C     *INZSR        BEGSR
034000081209      *
034100081209     C     *ENTRY        PLIST
034200081209     C                   PARM                    AGGIORNA          1
034300081209     C*
034400090109      * duplica le tariffe FGT
034500081209     C     fgt01K        kLIST
034600081211     C                   kfld                    stpPDR
034700081211     C                   kfld                    stpSML
034800081211     C                   kfld                    stpTSR
034900081211     C                   kfld                    stpCTR
035000081211     C                   kfld                    stpPRG
035100090109      *
035200090109      * duplica le tariffe FPT e FPD
035300090109     c     Key_FPT       KLIST
035400090109     c                   kfld                    stpPDR
035500090109     c                   kfld                    stpSML
035600090109     c                   kfld                    stpTSR
035700090109     c                   kfld                    stpCTR
035800090109     c                   kfld                    stpPRG
035900081209     C*
036000090401     c     Key_APD       KLIST
036100090401     c                   kfld                    apdTIP
036200090401     c                   kfld                    apdPDR
036300090401     C*
036400081209     c                   clear                   esito
036500081209      *
036600081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
036700081209     C                   clear                   UDATE_8           8 0
036800081209     C                   MOVEL     *DATE         UDATE_8
036900090109      *
037000090109      * reperisce il prezzo di riferimento del carburante
037100090109     c                   call      'FICN05R'
037200090109     c                   parm                    ficn05DS
037300090109      *
037400090109     c                   iF        o05err = *blank
037500090109     c                   eval      dGPI = o05GPI
037600090109     c                   enD
037700090109      *
037800081209     c                   endSR
037900090109      *---------------------------------------------------------------*
038000090109      *  Scrive Testate e Dettaglio
038100090109      *---------------------------------------------------------------*
038200090423     c     FPT_FPD       begSR
038300090109     C*
038400090109     c     Key_FPT       setll     fifpt01L
038500090109     c     Key_FPT       reade     fifpt01L
038600090109     c                   Dow       not %EoF(fifpt01l)
038700090423     C*
038800090424      * deve solo aggiornare il progressivo
038900090424     c                   z-add     200           fptPRG
039000090423     c                   update    fifpt000
039100090423      *
039200090109     c     Key_FPT       reade     fifpt01L
039300090109     c                   endDo
039400090109     C*
039500090109     c     Key_FPT       setll     fifpd01L
039600090109     c     Key_FPT       reade     fifpd01L
039700090109     c                   Dow       not %EoF(fifpd01l)
039800090423      *
039900090424      * deve solo aggiornare il progressivo
040000090109     c                   z-add     200           fpdPRG
040100090423     c                   update    fifpd000
040200090423      *
040300090109     c     Key_FPT       reade     fifpd01L
040400090109     c                   endDo
040500090109     C*
040600090109     c                   endSR
040700081211     C*---------------------------------------------------------------*
040800090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
040900090424      *---------------------------------------------------------------*
041000090424     c     WRI_DETTA     begSR
041100090424     C*
041200090424     c                   eval      WSFSML  =  stpSML
041300090424     c                   eval      WSFPDR  =  stpPDR
041400090424     c                   eval      WSFTSR  =  stpTSR
041500090424     c                   eval      WSFCTR  =  stpCTR
041600090424     c                   eval      WSFPRG  = stpPRG
041700090424     c                   eval      WSFDDT  = stpDDT
041800090424     c                   eval      WSFDST  = stpDST
041900090424     c                   eval      WSFDTS  = stpDTS
042000090424     c                   eval      WSFDCN  = stpDCN
042100090424     c                   eval      WSFREC  = FGT_REC
042200090424     c                   eval      WSFDECOR= stp_DATdeco
042300090424     c                   eval      WSFSCAD = stp_DATscad
042400090424     c                   eval      WSFPROGR= 'TNVRA18R'
042500090424     c**
042600090424     c                   write     WSFGT000
042700090424     C*
042800090424     c                   endSR
042900090424     C*---------------------------------------------------------------*
043000081110     OQSYSPRT   E            TESTA            02
043100081110     O                                           16 'Simul'
043200081110     O                                           25 'Autista'
043300081110     O                                           30 'TSR'
043400081110     O                                           38 'Tariffa'
043500081110     O                                           45 'Progr'
043600081110     O                                           55 'Data_Dec'
043700081110     O                                           65 'Data_Sca'
043800081110     O                                           75 'Data_Stp'
043900081110     O                                           85 'Data_Cnv'
044000081211     O                                          115 'N_Rec_FGT'
044100090424     O                                          125 'Data_Min'
044200081110     O          E            DETta       1
044300081110     O                       stpSML              14
044400081110     O                       stpPDR        z     25
044500081110     O                       stpTSR              29
044600081110     O                       stpCTR              36
044700081110     O                       stpPRG        z     43
044800081110     O                       stpDDT        z     55
044900081110     O                       stpDST        z     65
045000081110     O                       stpDTS        z     75
045100081110     O                       stpDCN        z     85
045200081211     o                       FGT_REC       z    115
045300090424     O                       stp_DATscad   z    125
045400081110      ****
045500081110     o          E            FINEST      2
045600090424     o                                           30 '*** TNVRA18R ***'
045700090102     o                                           59 '***   FINE  STAMPA  ***'
045800090109     o                                          + 9 '***  SUPER TARIFFA  ***'
