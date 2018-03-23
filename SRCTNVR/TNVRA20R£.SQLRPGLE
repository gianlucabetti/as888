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
005000090423      **  Si divide in 2 il problema:
005100090423      **  1)Gli autisti che hanno tariffe relative ad un solo Periodo da ricondurre
005200090423      **    ad un unico progressivo 200 decorrenza 1/5 scadenza 31/12/2039
005300090423      **    e
005400090423      **  2)Gli autisti che invece hanno + tariffe in periodi diversi occorre generare
005500090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
005600090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
005700090423      **    decorrenza del progressivo 201 e così via......
005800090423      **
005900090424      **  ATTENZIONE: nel 2° PROBLEMA posso dividere ancora in 2 le cose:
006000090424      **   Gli AUT che hanno tutti i relativi FGT in scadenza oltre il 2009
006100090424      **   ma con decorrenze differenti fra cui alcune maggiori/uguali del 1/5
006200090424      **    In tal caso si dovranno far scadere le tar. precedenti al 1/5 al 30/4
006300090424      **    e riaperte con progr.200 dal 1/5  scadenza 20391231
006400090424      **   mentre vanno aggiornate con progr.200 quelle con decorrenza 1/5 e
006500090424      **   scadenza 20391231
006600090424      **
006700090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
006800090423      **   devono risultare
006900090423      **   NON CONFERMATE
007000090423      **
007100090423     C*=================================================================
007200090423      **  Queste sono le tariffe da CONVERTIRE creando le SUPERTESTATE
007300090423     C*=================================================================
007400090424      ** 2° PROBLEMA
007500090423      **    DEVE leggere tutte le tariffe AUTISTI (C,R,G) con data decorrenza
007600090422      **  minore del 2010 e con Data SCADENZA superiore al 1°maggio 2009
007700090424      **  ma riconducibili a più periodi però con almeno una
007800090424      **  decorrenza superiore/uguale al 1/5/2009 e tutte con scadenza oltre il 2009.
007900090424      *
008000090424      *  esempio:
008100090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA    DATAMIN
008200090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.   SCADENZA
008300090424      *                         PADR                          TAR.         TAR.
008400090424      *
008500090424      *    700735                 C    300         EUR    2002/01/01  2039/12/31  2039/12/31
008600090424      *    700735                 C    999         EUR    2002/01/01  2039/12/31  2039/12/31
008700090424      *    700735                 G    999    6    EUR    2009/05/01  2040/12/31  2039/12/31
008800090424      *    700735                 R    300         EUR    2002/01/01  2039/12/31  2039/12/31
008900090424      *
009000090424      *    700738                 C    300    1    EUR    2004/03/31  2040/12/31  2040/12/31
009100090424      *    700738                 C    999    1    EUR    2004/03/31  2040/12/31  2040/12/31
009200090424      *    700738                 G    999    7    EUR    2009/05/01  2040/12/31  2040/12/31
009300090424      *    700738                 R    300    1    EUR    2004/03/31  2040/12/31  2040/12/31
009400090424      *                                                   Diverse         oltre il 2009
009500090424      *
009600090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009700090424     C+    with AUTSIMULA (AUTISTA, SIMULA, DATAMAX) as (
009800090424     C+    select fgtpdr, fgtsml, max(fgtddt) from
009900090424     C+    fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
010000090424     C+    and fgtddt < 20100101 and fgtdst >= 20090501
010100090424     C+    group BY FGTpdr, FGTsml
010200090424     C+            HAVING max(fgtddt) >= 20090501)
010300090424     C+  SELECT DATAMAXDECO, DATAMINSCAD,
010400090424     C+  a.* , rrn(a) FROM
010500090424     C+  fifgt00f as a join AUTSIMULA on a.fgtpdr=Autista
010600090424     C+  and a.fgtsml=SIMULA
010700090424     C+  join (
010800090424     C+  select fgtpdr, fgtsml, min(fgtdst) as DATAMINSCAD,
010900090424     C+  max(fgtddt) as DATAMAXDECO
011000090424     C+  from
011100090424     C+  fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
011200090424     C+  and fgtddt < 20100101 and
011300090424     C+  fgtdst >= 20090501
011400090424     C+  group BY FGTpdr, FGTsml
011500090424     C+           HAVING min(fgtdst) > 20091231
011600090424     C+  )  as SCADENZA
011700090424     C+   on a.fgtpdr=SCADENZA.fgtpdr and a.fgtsml=SCADENZA.fgtsml
011800090424     C+  WHERE  fgttsr in ('C', 'R', 'G')
011900090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
012000090424     C+  and a.fgtsml concat digits(a.fgtpdr) in
012100090424     C+  (
012200090424     C+  select fgtsml concat digits(fgtpdr) from
012300090424     C+  (
012400090424     C+  select FGTSML, FGTPDR, count(*) from
012500090424     C+  (
012600090424     C+  SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
012700090424     C+  fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
012800090424     C+  and fgtddt < 20100101 and fgtdst >= 20090501
012900090424     C+  group BY FGTSML, FGTPDR, FGTDDT, FGTDST
013000090424     C+  ) as b
013100090424     C+  group BY FGTSML, FGTPDR
013200090424     C+  having count(*) > 1
013300090424     C+  ) as unPERIODO
013400090424     C+  )
013500090424     C+  ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
013600090422     C/END-EXEC
013700081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
013800081110     c                   if        sqlCODE < 0
013900081110     c                   eval      esito = Sqlcode
014000081110     c                   dump(a)
014100081110     c                   return
014200081110     c                   end
014300081110     C/EXEC SQL
014400081110     C+ OPEN A1
014500081110     C/END-EXEC
014600081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
014700081110     c                   if        sqlCODE < 0
014800081110     c                   eval      esito = Sqlcode
014900081110     c                   dump(a)
015000081110     c                   return
015100081110     c                   else
015200081110     c                   except    testa
015300081110     c                   end
015400090109      * >>>>>>>
015500081209      * resto nel ciclo finchè....
015600081110     C                   DOU       SqlCod < 0
015700081110      * lettura cursore
015800081110     C/EXEC SQL
015900090424     C+ FETCH NEXT FROM A1 INTO :STP_DatDECO, :STP_DatSCAD, :dsFGT,
016000090424     C+ :FGT_REC
016100081110     C/END-EXEC
016200081110      *  EoF
016300081110     C                   if        SqlCod = 100
016400081110     c                   Leave
016500081110      *  x errori
016600081110     C                   ElseIf    SqlCod < 0
016700081110     c                   eval      esito = Sqlcode
016800081110     c                   dump(a)
016900081110     c                   leave
017000081110     C                   ENDif
017100081110      *
017200090109     c                   clear                   eseguito          1
017300090109     c                   clear                   verifica          1
017400090102     c                   clear                   errore           10
017500081110      *
017600081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
017700090423     c                   movel     stpSML        stpSMLPDR         8
017800090109     c                   move      stpPDR        Autista           7 0
017900090423     c                   move      Autista       stpSMLPDR
018000090109      *
018100090109      *  Solo in aggiornamento
018200081110     c                   if        Aggiorna = 'S'
018300090424      *
018400090424      *  Deve scrivere la testata poichè comunuque ha un SOLO periodo
018500090424      *  dal 1/5/2009 al 31/12/2039
018600090109      *    a rottura di simulazione/autista
018700090423      *     salva chiave di rottura
018800090423     c                   if        STPsmlPDR <> savSTPsmlPDR
018900090423     c                   eval      savSTPsmlPDR = STPsmlPDR
019000090423      *
019100090423      *  IMPOSTA la SUERTESTATA
019200090109     c                   clear                   fitgt000
019300090423     c                   z-add     STPpdr        tgtPDR
019400090423     c                   move      STPsml        tgtSML
019500090423     c                   z-add     200           tgtPRG
019600090423     c                   move      STPdiv        tgtDIV
019700090424      **
019800090423      * data decorrenza/scadenza
019900090423     c                   z-add     20090501      tgtDDT
020000090423      *
020100090424      * CON L'SQL la data scadenza sarà SEMPRE SUPERIORE al 2009
020200090424      *  QUINDI sempre la data all'infinito 31/12/2039
020300090424     c                   if        stp_DatSCAD > 20091231
020400090423     c                   z-add     20391231      tgtDST
020500090423     c                   else
020600090424     c                   z-add     stp_DatSCAD   tgtDST
020700090423     c                   end
020800090423      *
020900090423     c                   z-add     *date         tgtDUV
021000090423     c                   moveL     'Convers'     tgtPRU
021100090401      *
021200090423      * rileva dall'anagrafica
021300090401     c                   eval      apdTIP = 'A'
021400090401     c                   eval      apdPDR = tgtPDR
021500090401     c     Key_APD       chain     fiapd01l
021600090423      *---
021700090401     c                   if        %Found(fiapd01l)
021800090423      *   società fornitore
021900090421     c                   movel     apdcsf        tgtsoc
022000090421     c                   movel     *all'0'       tgtcdf
022100090421     c                   move      apdksc        tgtcdf
022200090421     c                   movel     apdflr        dapdflr
022300090423      *  la massa in KG
022400090421     c                   eval      tgtpkl = §APDMCP
022500090423     c*  i tempi di carico e scarico
022600090421     c                   clear                   ficn08ds
022700090421     c                   eval      i08data= tgtddt
022800090421     c                   eval      i08tip= 'A'
022900090421     c                   eval      i08mcp = tgtpkl
023000090421     c                   call      'FICN08R'
023100090421     c                   parm                    ficn08ds
023200090421     c                   if        o08err =  ' '
023300090421     c                   eval      TGTTMPC = o08tc
023400090421     c                   eval      TGTTMPS = o08ts
023500090421     c                   end
023600090423      *---
023700090401     c                   end
023800090423      *
023900090423      *  prezzo carburante al litro
024000090109     c                   z-add     D§GPIGPI      tgtPCAR
024100090422      * Attenzione:
024200090423      *     se non sono state stampate nel vecchio
024300090423      *        non devono essere convalidate nel nuovo
024400090424      * MA CON QUESTO SPECIFICO SQL potrei avere righe tariffe precedenti l'1/5
024500090424      * già STAMPATE e
024600090424      * TARIFFE decorrenti dal 1/5 non ANCORA STAMPATE......
024700090424      ***
024800090424      * Attenzione:
024900090424      *   C O S A     F A R E  ???????
025000090424      *     Ci sarà un pgm finale che allinea le date stampa x la superTESTATA
025100090424     c                   if        stpDTS > 0
025200090424     c                   z-add     20090501      tgtDTS
025300090424     c                   end
025400090422      *
025500090109     c                   write     fitgt000
025600090109     c                   end
025700090423      *------------------
025800081216      *
025900090423      *  genera o aggiorna
026000090423      *     i dettagli con il nuovo progressivo
026100090423     c                   exsr      FPT_FPD
026200090109      *
026300090109      *------------------
026400081218      *   deve eliminare i puntamenti del codice di riferimento
026500090424     c*************      open      fifgt00f
026600090423      *
026700081218     c     fgt_REC       chain     FIFGT00F
026800081218     c                   if        %Found(FIFGT00F)
026900090112      *
027000090422      *  Fa scadere la testata al 30 apr.2009
027100090423     c                   if            fgtddt < 20090501
027200090422     c                   eval      fgtDST = 20090430
027300090423     c                   end
027400090424      **
027500090422      *     Ma se decorreva già dal 1/5/2009
027600090422      *   non può farlo scadere il 30/4/2009
027700090112      * e quindi questa diventa la tariffa della supertestata
027800090423     c                   if            fgtddt = 20090501
027900090423      * data scadenza
028000090424      *   CON QUESTO SQL è SEMPRE MAGGIORE
028100090423     c                   if           fgtDST > 20091231
028200090423     c                   z-add     20391231      fgtDST
028300090423     c                   endIf
028400090423      *
028500090112     c                   z-add     200           fgtPRG
028600090112     c                   z-add     *date         fgtDUV
028700090112     c                   clear                   fgtDCN
028800090423      *
028900090423      * solo se era vuoto anche prima
029000090423     c                   if            stpDTS > 0
029100090424     c                   z-add     20090501      fgtDTS
029200090423     c                   elseif        stpDTS = 0
029300090112     c                   clear                   fgtDTS
029400090423     c                   end
029500090112     c                   clear                   fgtFTR
029600090112     c                   clear                   fgtDTR
029700090423      *
029800090423     c                   end
029900090423      *
030000090423      * aggiorna
030100081218     c                   update    fgtFisico
030200090109      *
030300090423      *  Quindi scrive un'altra testata SOLO SE decorrenza < 1/5/2009
030400090423     c                   IF           fgtDDT < 20090501
030500090420     c                   eval      fgtDDT = 20090501
030600090423      *
030700090423     c                   if        stpDST > 20091231
030800090423     c                   z-add     20391231      fgtDST
030900090423     c                   else
031000090424     c                   z-add     stp_DATscad   fgtDST
031100090423     c                   end
031200090423      *
031300090109     c                   z-add     200           fgtPRG
031400090109     c                   z-add     *date         fgtDUV
031500090109     c                   clear                   fgtDCN
031600090424     c*****************  z-add     tgtDTS        fgtDTS
031700090109     c                   clear                   fgtFTR
031800090109     c                   clear                   fgtDTR
031900090109     c                   write     fgtFisico
032000090423     c                   endIF
032100090112      *
032200090423     c                   endIF
032300090112      *
032400090424     c************       close     fifgt00f
032500090109      *
032600090109     c                   endif
032700090109      *------------------
032800081210      *  se sono qui ho letto un record correttamente
032900081210     c   Of              except    testa
033000081210     c                   except    detta
033100090424      *
033200090424      *  scrive il dettaglio su un file
033300090424     c                   exsr      wri_DETTA
033400081110      *
033500081110     C                   ENDDO
033600081110     C/EXEC SQL
033700081110     C+ Close A1
033800081110     C/END-EXEC
033900081110     C                   if        Esito < 0
034000081110     c                   return
034100081110     c                   endIF
034200081110     C*
034300081110     C     FINE          TAG
034400081110     c                   except    finest
034500081110     c                   return
034600081209     C*------------------------------------------------------------------------*
034700081209     C* *INZSR - OPERAZIONI INIZIALI
034800081209     C*------------------------------------------------------------------------*
034900081209     C     *INZSR        BEGSR
035000081209      *
035100081209     C     *ENTRY        PLIST
035200081209     C                   PARM                    AGGIORNA          1
035300081209     C*
035400090109      * duplica le tariffe FGT
035500081209     C     fgt01K        kLIST
035600081211     C                   kfld                    stpPDR
035700081211     C                   kfld                    stpSML
035800081211     C                   kfld                    stpTSR
035900081211     C                   kfld                    stpCTR
036000081211     C                   kfld                    stpPRG
036100090109      *
036200090109      * duplica le tariffe FPT e FPD
036300090109     c     Key_FPT       KLIST
036400090109     c                   kfld                    stpPDR
036500090109     c                   kfld                    stpSML
036600090109     c                   kfld                    stpTSR
036700090109     c                   kfld                    stpCTR
036800090109     c                   kfld                    stpPRG
036900081209     C*
037000090401     c     Key_APD       KLIST
037100090401     c                   kfld                    apdTIP
037200090401     c                   kfld                    apdPDR
037300090401     C*
037400081209     c                   clear                   esito
037500081209      *
037600081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
037700081209     C                   clear                   UDATE_8           8 0
037800081209     C                   MOVEL     *DATE         UDATE_8
037900090109      *
038000090109      * reperisce il prezzo di riferimento del carburante
038100090109     c                   call      'FICN05R'
038200090109     c                   parm                    ficn05DS
038300090109      *
038400090109     c                   iF        o05err = *blank
038500090109     c                   eval      dGPI = o05GPI
038600090109     c                   enD
038700090109      *
038800081209     c                   endSR
038900090109      *---------------------------------------------------------------*
039000090109      *  Scrive Testate e Dettaglio
039100090109      *---------------------------------------------------------------*
039200090423     c     FPT_FPD       begSR
039300090109     C*
039400090109     c     Key_FPT       setll     fifpt01L
039500090109     c     Key_FPT       reade     fifpt01L
039600090109     c                   Dow       not %EoF(fifpt01l)
039700090423     C*
039800090109     c                   z-add     200           fptPRG
039900090423     C*
040000090424      * se decorre dal 1/5 allora deve aggiornare
040100090423     c                   if        stpDDT = 20090501
040200090423     c                   update    fifpt000
040300090423     c                   else
040400090109     c                   write     fifpt000
040500090423     c                   end
040600090423      *
040700090109     c     Key_FPT       reade     fifpt01L
040800090109     c                   endDo
040900090109     C*
041000090109     c     Key_FPT       setll     fifpd01L
041100090109     c     Key_FPT       reade     fifpd01L
041200090109     c                   Dow       not %EoF(fifpd01l)
041300090423      *
041400090109     c                   z-add     200           fpdPRG
041500090423      *
041600090423      * se decorre dal 1/5 allora deve aggiornare
041700090423     c                   if        stpDDT = 20090501
041800090423     c                   update    fifpd000
041900090423     c                   else
042000090423     c                   write     fifpd000
042100090423     c                   end
042200090423      *
042300090109     c     Key_FPT       reade     fifpd01L
042400090109     c                   endDo
042500090109     C*
042600090109     c                   endSR
042700081211     C*---------------------------------------------------------------*
042800090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
042900090424      *---------------------------------------------------------------*
043000090424     c     WRI_DETTA     begSR
043100090424     C*
043200090424     c                   eval      WSFSML  =  stpSML
043300090424     c                   eval      WSFPDR  =  stpPDR
043400090424     c                   eval      WSFTSR  =  stpTSR
043500090424     c                   eval      WSFCTR  =  stpCTR
043600090424     c                   eval      WSFPRG  = stpPRG
043700090424     c                   eval      WSFDDT  = stpDDT
043800090424     c                   eval      WSFDST  = stpDST
043900090424     c                   eval      WSFDTS  = stpDTS
044000090424     c                   eval      WSFDCN  = stpDCN
044100090424     c                   eval      WSFREC  = FGT_REC
044200090424     c                   eval      WSFDECOR= stp_DATdeco
044300090424     c                   eval      WSFSCAD = stp_DATscad
044400090424     c                   eval      WSFPROGR= 'TNVRA17R'
044500090424     c**
044600090424     c                   write     WSFGT000
044700090424     C*
044800090424     c                   endSR
044900090424     C*---------------------------------------------------------------*
045000081110     OQSYSPRT   E            TESTA            02
045100081110     O                                           16 'Simul'
045200081110     O                                           25 'Autista'
045300081110     O                                           30 'TSR'
045400081110     O                                           38 'Tariffa'
045500081110     O                                           45 'Progr'
045600081110     O                                           55 'Data_Dec'
045700081110     O                                           65 'Data_Sca'
045800081110     O                                           75 'Data_Stp'
045900081110     O                                           85 'Data_Cnv'
046000081211     O                                          115 'N_Rec_FGT'
046100090424     O                                          125 'Data_Min'
046200081110     O          E            DETta       1
046300081110     O                       stpSML              14
046400081110     O                       stpPDR        z     25
046500081110     O                       stpTSR              29
046600081110     O                       stpCTR              36
046700081110     O                       stpPRG        z     43
046800081110     O                       stpDDT        z     55
046900081110     O                       stpDST        z     65
047000081110     O                       stpDTS        z     75
047100081110     O                       stpDCN        z     85
047200081211     o                       FGT_REC       z    115
047300090424     O                       stp_DATscad   z    125
047400081110      ****
047500081110     o          E            FINEST      2
047600090424     o                                           30 '*** TNVRA17R ***'
047700090102     o                                           59 '***   FINE  STAMPA  ***'
047800090109     o                                          + 9 '***  SUPER TARIFFA  ***'
